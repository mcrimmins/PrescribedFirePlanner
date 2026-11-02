# ==============================================================================
# scripts/build_station_cache.R -- build the bundled station snapshots
#
# Downloads each station marked bundled = TRUE in data/stations_featured.csv
# through the app's own download layer (fems_download_weather) and saves the
# RAW result -- no screening, no calendar columns -- to
#     data/stations_bundled/<station_id>.rds   (saveRDS, xz compression)
# plus data/stations_bundled/manifest.csv, which the app and
# tests/test_station_cache.R check against.
#
# Screening and quality checks run when the app loads a station, so they are
# deliberately NOT applied here: a change to the screening rules then applies
# to bundled and live stations alike without rebuilding.
#
# Run from the project root with PrescribedFirePlanner.Rproj open:
#     source("scripts/build_station_cache.R")
# About 20-30 s per station. Then run tests/test_station_cache.R, commit the
# changed files in data/stations_bundled/ and redeploy.
#
# To change which stations are bundled: edit the `bundled` column in
# data/stations_featured.csv and run this again. Snapshots of stations that
# are no longer listed are reported but not deleted -- remove them with git rm.
#
# A station that fails to download keeps its previous snapshot and manifest
# row (if any); the summary says so.
# ==============================================================================

local({

  # ---- settings --------------------------------------------------------------
  start_date <- as.Date("2005-01-01")
  end_date   <- Sys.Date() - 1          # yesterday (local): avoids a part-day
  only       <- NULL                    # e.g. c("80802") to rebuild just one

  out_dir   <- file.path("data", "stations_bundled")
  manifest  <- file.path(out_dir, "manifest.csv")
  featured  <- file.path("data", "stations_featured.csv")
  log_dir   <- file.path("tests", "logs")

  # ---- log -------------------------------------------------------------------
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(log_dir, paste0("build_station_cache_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
  con <- file(log_file, open = "wt", encoding = "UTF-8")
  .say <- function(...) { txt <- paste0(...); cat(txt, "\n", sep = ""); cat(txt, "\n", sep = "", file = con) }

  results <- list()
  tryCatch({
    if (!file.exists("R/fems_download.R") || !file.exists("PrescribedFirePlanner.Rproj"))
      stop("run from the project root (open PrescribedFirePlanner.Rproj)")
    if (!file.exists(featured)) stop("missing ", featured)

    .say("Build bundled station snapshots")
    .say("Run at:  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
    .say("Range:   ", start_date, " to ", end_date)
    source("R/fems_download.R")

    fs <- utils::read.csv(featured, colClasses = c(station_id = "character"), stringsAsFactors = FALSE)
    if (!all(c("station_id", "name", "bundled") %in% names(fs)))
      stop(featured, " needs columns station_id, name, bundled")
    want <- fs[as.logical(fs$bundled) %in% TRUE, ]
    if (!is.null(only)) want <- want[want$station_id %in% only, ]
    .say("Stations: ", paste(want$station_id, want$name, collapse = "; "))
    if (!nrow(want)) stop("no stations marked bundled = TRUE")

    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    old <- if (file.exists(manifest))
      utils::read.csv(manifest, colClasses = c(station_id = "character", md5 = "character"), stringsAsFactors = FALSE)
      else NULL

    contract <- c("station_id", "date", "hour", "record_type", "obs_minute",
                  "temperature", "relativeHumidity", "precipitation", "windSpeed",
                  "windDirection", "gustSpeed", "gustDirection", "solarRadiation",
                  "vpd", "hdw", "dewpoint")

    for (i in seq_len(nrow(want))) {
      sid <- want$station_id[i]; nm <- want$name[i]
      .say(""); .say("---- ", sid, " ", nm)
      t0 <- Sys.time()
      wx <- tryCatch(
        withCallingHandlers(
          fems_download_weather(sid, start_date, end_date),
          message = function(m) { .say("  | ", sub("\n$", "", conditionMessage(m))); invokeRestart("muffleMessage") }),
        error = function(e) { .say("  ERROR: ", conditionMessage(e)); NULL })
      secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

      problem <- if (is.null(wx) || !nrow(wx)) "download returned nothing"
        else if (length(setdiff(contract, names(wx)))) paste("missing columns:", paste(setdiff(contract, names(wx)), collapse = ","))
        else if (any(duplicated(wx[c("station_id", "date", "hour", "record_type")]))) "duplicate keys"
        else if (!all(wx$station_id == sid)) "wrong station_id"
        else NULL
      if (!is.null(problem)) {
        .say("  NOT SAVED (", problem, "); previous snapshot kept if there was one")
        results[[sid]] <- data.frame(station_id = sid, name = nm, status = "FAILED", detail = problem)
        next
      }

      wx <- wx[order(wx$date, wx$hour), ]
      dest <- file.path(out_dir, paste0(sid, ".rds"))
      tmp  <- paste0(dest, ".tmp")
      saveRDS(wx, tmp, compress = "xz")
      chk <- readRDS(tmp)                                # read back before replacing
      if (!identical(dim(chk), dim(wx))) { unlink(tmp); stop("read-back mismatch for ", sid) }
      if (file.exists(dest)) unlink(dest)
      if (!file.rename(tmp, dest)) stop("could not move ", tmp, " to ", dest)

      md5 <- unname(tools::md5sum(dest))
      mins <- sort(unique(wx$obs_minute))
      row <- data.frame(
        station_id = sid, name = nm, file = basename(dest),
        start = format(min(wx$date)), end = format(max(wx$date)), rows = nrow(wx),
        obs_minute = paste(mins, collapse = ";"),
        downloaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
        bytes = file.size(dest), md5 = md5, stringsAsFactors = FALSE)
      results[[sid]] <- data.frame(station_id = sid, name = nm, status = "saved",
                                   detail = sprintf("%d rows, %s to %s, %.1f MB, %s s",
                                                    nrow(wx), row$start, row$end, row$bytes / 1e6, secs))
      .say("  saved ", dest, " -- ", results[[sid]]$detail)
      if (!is.null(old)) old <- old[old$station_id != sid, ]
      old <- rbind(old, row)
    }

    # Keep rows only for stations still listed (failed ones keep their old row).
    listed <- fs$station_id[as.logical(fs$bundled) %in% TRUE]
    if (!is.null(old)) {
      old <- old[old$station_id %in% listed, ]
      old <- old[order(match(old$station_id, listed)), ]
      utils::write.csv(old, manifest, row.names = FALSE)
      .say(""); .say("Wrote ", manifest, " (", nrow(old), " stations)")
    }

    orphans <- setdiff(sub("\\.rds$", "", list.files(out_dir, pattern = "\\.rds$")), listed)
    if (length(orphans)) .say("No longer bundled (remove with git rm): ",
                              paste(file.path(out_dir, paste0(orphans, ".rds")), collapse = ", "))
  }, error = function(e) .say("ABORTED: ", conditionMessage(e)))

  .say(""); .say("---- Summary")
  if (length(results)) for (r in results) .say(sprintf("  %-6s %-12s %-6s %s", r$station_id, r$name, r$status, r$detail))
  total <- sum(file.size(list.files(out_dir, pattern = "\\.rds$", full.names = TRUE)))
  .say("Bundled files total: ", round(total / 1e6, 1), " MB")
  .say("Next: source(\"tests/test_station_cache.R\"), then commit data/stations_bundled/.")
  .say("Log: ", normalizePath(log_file, winslash = "/", mustWork = FALSE))
  close(con)
  invisible(results)
})
