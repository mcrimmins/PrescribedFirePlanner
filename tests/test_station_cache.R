# ==============================================================================
# tests/test_station_cache.R -- check the bundled station snapshots
#
# Checks data/stations_bundled/ (built by scripts/build_station_cache.R):
#   * the manifest lists exactly the stations marked bundled = TRUE
#   * every file exists, its md5 matches the manifest, and it reads back
#   * each snapshot meets the weather download contract, starts in 2005,
#     ends on the manifest's end date, has the manifest's row count and is
#     RAW (no screening or calendar columns baked in)
#   * optional: one June week from each snapshot matches a live FEMS download
#     (a WARN if it differs -- FEMS can revise past data -- and skipped if
#     FEMS can't be reached)
#
# Run from the project root with PrescribedFirePlanner.Rproj open:
#     source("tests/test_station_cache.R")
# Set check_live <- FALSE below to skip the FEMS comparison.
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)

  check_live <- TRUE
  live_week  <- as.Date(c("2024-06-10", "2024-06-16"))
  max_total_mb <- 50

  out_dir  <- file.path("data", "stations_bundled")
  manifest <- file.path(out_dir, "manifest.csv")
  featured <- file.path("data", "stations_featured.csv")

  .T <- .test_open("test_station_cache")

  tryCatch({
    .env_header("Bundled station snapshot checks")
    .section("A. Files and manifest")
    at_root <- file.exists("PrescribedFirePlanner.Rproj")
    .check("A1 working directory is the project root", at_root)
    if (!at_root) stop("not at project root")
    .check("A2 data/stations_featured.csv exists", file.exists(featured))
    .check("A3 manifest exists", file.exists(manifest), manifest)
    if (!file.exists(featured) || !file.exists(manifest)) stop("run scripts/build_station_cache.R first")

    fs <- utils::read.csv(featured, colClasses = c(station_id = "character"), stringsAsFactors = FALSE)
    mf <- utils::read.csv(manifest, colClasses = c(station_id = "character", md5 = "character"),
                          stringsAsFactors = FALSE)
    listed <- fs$station_id[as.logical(fs$bundled) %in% TRUE]
    .check("A4 manifest stations = stations marked bundled", setequal(mf$station_id, listed),
           paste0("bundled: ", paste(listed, collapse = ","), "; manifest: ", paste(mf$station_id, collapse = ",")))
    .check("A5 manifest has one row per station", !anyDuplicated(mf$station_id))
    .check("A6 stations_featured.csv has a reviewed column", "reviewed" %in% names(fs))
    files <- list.files(out_dir, pattern = "\\.rds$")
    stray <- setdiff(sub("\\.rds$", "", files), mf$station_id)
    .qc("A7 no snapshot files outside the manifest", !length(stray),
        if (length(stray)) paste("remove with git rm:", paste(stray, collapse = ",")) else "")
    total_mb <- sum(file.size(file.path(out_dir, files))) / 1e6
    .check(paste0("A8 total size under ", max_total_mb, " MB"), total_mb < max_total_mb, paste0(round(total_mb, 1), " MB"))

    snaps <- list()
    for (i in seq_len(nrow(mf))) {
      m <- mf[i, ]; id <- paste0("S", i)
      .section(paste0(id, ". ", m$station_id, " ", m$name))
      path <- file.path(out_dir, m$file)
      ok_file <- file.exists(path)
      .check(paste0(id, ".1 file exists"), ok_file, path)
      if (!ok_file) next
      md5 <- unname(tools::md5sum(path))
      .check(paste0(id, ".2 md5 matches the manifest"), identical(md5, m$md5),
             if (identical(md5, m$md5)) "" else paste("file", md5, "vs manifest", m$md5))
      r <- .run(paste(m$station_id, "read"), readRDS(path))
      .check(paste0(id, ".3 reads back"), r$ok && is.data.frame(r$value))
      if (!(r$ok && is.data.frame(r$value))) next
      wx <- r$value
      .check_wx_contract(wx, paste0(id, ".4"), m$station_id)
      raw_only <- !any(c("year", "month", "doy", "hour_clock", "in_rx") %in% names(wx)) && is.null(attr(wx, "qc_log"))
      .check(paste0(id, ".5 snapshot is raw (no calendar, screening or flag columns)"), raw_only)
      .check(paste0(id, ".6 row count matches the manifest"), nrow(wx) == m$rows, paste(nrow(wx), "vs", m$rows))
      .check(paste0(id, ".7 dates match the manifest"),
             format(min(wx$date)) == m$start && format(max(wx$date)) == m$end,
             paste0(min(wx$date), " to ", max(wx$date), " vs ", m$start, " to ", m$end))
      .qc(paste0(id, ".8 record starts in 2005"), format(min(wx$date), "%Y") == "2005", paste("starts", min(wx$date)))
      hpd <- tapply(wx$hour, wx$date, length)
      .qc(paste0(id, ".9 every date has 24 rows"), all(hpd == 24),
          if (all(hpd == 24)) "" else paste(sum(hpd != 24), "dates differ"))
      .info(paste0(id, ".10 snapshot"), sprintf("%s rows, %.1f MB, obs minute %s, downloaded %s",
                                                format(nrow(wx), big.mark = ","), file.size(path) / 1e6,
                                                m$obs_minute, m$downloaded_at))
      snaps[[m$station_id]] <- wx
    }

    .section("L. Live comparison with FEMS")
    if (!check_live) {
      .info("L0 skipped", "check_live is FALSE")
    } else {
      have_layer <- file.exists("R/fems_download.R") && nzchar(Sys.getenv("FEMS_KEY"))
      if (!have_layer) {
        .info("L0 skipped", "no FEMS credentials in this session")
      } else {
        src <- .run("source R/fems_download.R", source("R/fems_download.R"))
        vals <- setdiff(.WX_COLS, c("station_id", "date", "hour", "record_type"))
        for (sid in names(snaps)) {
          lv <- .run(paste(sid, "live week"), fems_download_weather(sid, live_week[1], live_week[2], verbose = FALSE))
          if (!lv$ok || !is.data.frame(lv$value) || !nrow(lv$value)) {
            .info(paste0("L ", sid), "FEMS not reachable or no rows; comparison skipped")
            next
          }
          a <- snaps[[sid]]; a <- a[a$date >= live_week[1] & a$date <= live_week[2], ]
          b <- lv$value
          key <- c("date", "hour", "record_type")
          a <- a[do.call(order, a[key]), ]; b <- b[do.call(order, b[key]), ]
          same_keys <- nrow(a) == nrow(b) && all(a$date == b$date & a$hour == b$hour)
          diffs <- if (same_keys) vapply(vals, function(v) {
            x <- a[[v]]; y <- b[[v]]
            sum(!((is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & abs(x - y) < 1e-6)))
          }, numeric(1)) else NA
          # A WARN, not a FAIL: FEMS can revise past data after a snapshot is taken.
          .qc(paste0("L ", sid, " snapshot week matches live FEMS"),
                 same_keys && all(diffs == 0),
                 if (!same_keys) paste("rows", nrow(a), "vs", nrow(b))
                 else if (all(diffs == 0)) paste(nrow(a), "rows identical")
                 else paste("differing values:", paste(names(diffs)[diffs > 0], diffs[diffs > 0], sep = "=", collapse = ", ")))
        }
      }
    }

  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })

  .test_finish()
})
