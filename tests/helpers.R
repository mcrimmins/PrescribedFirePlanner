# ==============================================================================
# tests/helpers.R -- shared helpers for tests/*.R (not app code)
#
# Source it INSIDE a test's local({ ... }) block, so nothing lands in the
# global environment:
#
#     local({
#       source("tests/helpers.R", local = TRUE)
#       .T <- .test_open("test_name")
#       ...
#       .test_finish()
#     })
#
# Every name starts with a dot (carryover sec. 2: a test helper called hr()
# once masked shiny::hr()).
#
# Check levels written to the transcript:
#   PASS / FAIL  code or contract checks -- these decide the verdict
#   OK / WARN    data-quality checks -- a WARN is listed in the summary but
#                does not fail the test (it describes the station, not the code)
#   INFO         diagnostics only
# ==============================================================================

# ---- transcript -------------------------------------------------------------
.test_open <- function(name) {
  dir <- file.path("tests", "logs")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  T <- new.env()
  T$name    <- name
  T$stamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  T$file    <- file.path(dir, paste0(name, "_", T$stamp, ".txt"))
  T$con     <- file(T$file, open = "wt", encoding = "UTF-8")
  T$results <- data.frame(id = character(0), status = character(0),
                          detail = character(0), stringsAsFactors = FALSE)
  T$abort   <- ""
  T$csvs    <- character(0)
  T$t0      <- Sys.time()
  T
}

.say <- function(...) {
  txt <- paste0(...)
  cat(txt, "\n", sep = "")
  cat(txt, "\n", sep = "", file = .T$con)
}
.show <- function(x) for (ln in utils::capture.output(print(x, row.names = FALSE))) .say("    ", ln)
.section <- function(title) {
  .say("")
  .say("---- ", title, " ", strrep("-", max(3, 70 - nchar(title))))
}

.record <- function(id, status, detail) {
  .T$results[nrow(.T$results) + 1, ] <- list(id, status, detail)
  .say(sprintf("[%s] %s%s", status, id, if (nzchar(detail)) paste0(" -- ", detail) else ""))
}
.check <- function(id, ok, detail = "") .record(id, if (isTRUE(ok)) "PASS" else "FAIL", detail)
.qc    <- function(id, ok, detail = "") .record(id, if (isTRUE(ok)) "OK" else "WARN", detail)
.info  <- function(id, detail = "")     .record(id, "INFO", detail)

# Run an expression, echoing its messages/warnings into the transcript and
# turning an error into a value instead of aborting the test.
.run <- function(label, expr) {
  warns <- character(0)
  t0 <- Sys.time()
  val <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      .say("  ERROR in ", label, ": ", conditionMessage(e))
      structure(list(msg = conditionMessage(e)), class = "test_error")
    }),
    message = function(m) {
      .say("  | ", sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      .say("  ! warning: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  .say("  (", label, ": ", secs, " s)")
  list(value = val, ok = !inherits(val, "test_error"), warnings = warns, secs = secs)
}

.write_csv <- function(df, suffix) {
  path <- file.path("tests", "logs", paste0(.T$name, "_", .T$stamp, "_", suffix, ".csv"))
  utils::write.csv(df, path, row.names = FALSE)
  .T$csvs <- c(.T$csvs, path)
  .say("    wrote ", path)
  invisible(path)
}

.env_header <- function(title, lines = character(0)) {
  .say(title)
  .say("Run at:   ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  for (ln in lines) .say(ln)
  .say("R:        ", R.version.string, " on ", R.version$platform)
  .say("WD:       ", normalizePath(getwd(), winslash = "/"))
  for (p in c("httr", "jsonlite", "dplyr", "tibble")) {
    v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "NOT INSTALLED")
    .say(sprintf("          %-9s %s", p, v))
  }
}

.test_finish <- function() {
  .section("Summary")
  r <- .T$results
  n <- function(s) sum(r$status == s)
  .say(sprintf("%d PASS, %d FAIL, %d OK, %d WARN, %d INFO",
               n("PASS"), n("FAIL"), n("OK"), n("WARN"), n("INFO")))
  if (n("FAIL") > 0) {
    .say("Failed checks:")
    for (i in which(r$status == "FAIL")) .say("  - ", r$id[i], "  ", r$detail[i])
  }
  if (n("WARN") > 0) {
    .say("Data-quality warnings (do not fail the test):")
    for (i in which(r$status == "WARN")) .say("  - ", r$id[i], "  ", r$detail[i])
  }
  if (nzchar(.T$abort)) .say(.T$abort)
  if (length(.T$csvs)) {
    .say("CSV outputs:")
    for (p in .T$csvs) .say("  ", normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  mins <- round(as.numeric(difftime(Sys.time(), .T$t0, units = "mins")), 1)
  .say("Elapsed:    ", mins, " min")
  .say("Transcript: ", normalizePath(.T$file, winslash = "/", mustWork = FALSE))
  ok <- n("FAIL") == 0 && !nzchar(.T$abort) && n("PASS") > 0
  .say("VERDICT: ", if (ok) "PASS" else "FAIL",
       if (ok && n("WARN") > 0) paste0(" (with ", n("WARN"), " data-quality warnings)") else "")
  close(.T$con)
  invisible(ok)
}

# ---- setup: project root, credentials, download layer -----------------------
# Returns TRUE if the download layer is usable; stops the test otherwise.
.setup_layer <- function(prefix = "A") {
  at_root <- file.exists("R/fems_download.R") && file.exists("R/fems_api.R") &&
             file.exists("PrescribedFirePlanner.Rproj")
  .check(paste0(prefix, "1 working directory is the project root"), at_root,
         if (at_root) "" else "open PrescribedFirePlanner.Rproj (or setwd() to it) and re-run")
  if (!at_root) stop("not at project root")

  user <- Sys.getenv("FEMS_USER", ""); key <- Sys.getenv("FEMS_KEY", "")
  .check(paste0(prefix, "2 FEMS_USER and FEMS_KEY are set"), nzchar(user) && nzchar(key),
         if (nzchar(user) && nzchar(key)) paste0("user=", user) else "edit .Renviron, then restart R")

  src <- .run("source R/fems_download.R", source("R/fems_download.R"))
  fns <- c("fems_gql_paged", "fems_download_weather", "fems_download_nfdrs")
  have <- vapply(fns, exists, logical(1), envir = globalenv())
  .check(paste0(prefix, "3 download layer loads"), src$ok && all(have),
         if (all(have)) "" else paste("missing:", paste(fns[!have], collapse = ", ")))
  .check(paste0(prefix, "4 jsonlite not attached (would mask shiny::validate)"),
         !("package:jsonlite" %in% search()))
  if (!(src$ok && all(have))) stop("download layer did not load")
  TRUE
}

# ---- station metadata -------------------------------------------------------
# Joins the two metadata CSVs for one station. NULL if not found.
.metadata_row <- function(station_id) {
  a <- utils::read.csv("data/station_metadata_FEMS3_042225.csv",
                       colClasses = c(station_id = "character"), stringsAsFactors = FALSE)
  b <- utils::read.csv("data/FEMS_3.0_RAWS_Master_Station_List_and_Metadata.csv",
                       colClasses = c(station_id = "character"), stringsAsFactors = FALSE)
  ra <- a[a$station_id == station_id, ]
  rb <- b[b$station_id == station_id, ]
  if (nrow(ra) == 0) return(NULL)
  list(station_id   = station_id,
       name         = ra$station_name[1],
       lat          = as.numeric(ra$latitude[1]),
       lon          = as.numeric(ra$longitude[1]),
       elev_ft      = as.numeric(ra$elevation[1]),
       tz           = ra$tz[1],
       state        = if (nrow(rb)) rb$state[1] else NA,
       tz_abbrev    = if (nrow(rb)) rb$time_zone[1] else NA,
       por_start    = if (nrow(rb)) as.Date(rb$period_record_start[1], "%m/%d/%Y") else as.Date(NA),
       avg_precip_in = if (nrow(rb)) suppressWarnings(as.numeric(rb$avg_annual_precip[1])) else NA_real_,
       status       = if (nrow(rb)) rb$station_status[1] else NA)
}

# ---- time and sun -----------------------------------------------------------
.offset_of <- function(x) {
  x <- as.character(x)
  ifelse(grepl("Z$", x), "Z", sub("^.*([+-][0-9]{2}:[0-9]{2})$", "\\1", x))
}
.offset_hours <- function(off) {
  s <- ifelse(substr(off, 1, 1) == "-", -1, 1)
  s * (as.numeric(substr(off, 2, 3)) + as.numeric(substr(off, 5, 6)) / 60)
}
# UTC offsets an IANA zone uses in mid-January and mid-July, as "-07:00".
.tz_offsets <- function(tz, year = 2024) {
  f <- function(d) {
    z <- format(as.POSIXct(paste(d, "12:00:00"), tz = tz), "%z")
    paste0(substr(z, 1, 3), ":", substr(z, 4, 5))
  }
  jan <- f(sprintf("%d-01-15", year)); jul <- f(sprintf("%d-07-15", year))
  list(jan = jan, jul = jul, dst = jan != jul,
       std = if (.offset_hours(jan) <= .offset_hours(jul)) jan else jul)
}
# Solar noon in local STANDARD time (hours) for a longitude and day of year.
.solar_noon_lst <- function(lon, std_offset_h, doy) {
  B   <- 2 * pi * (doy - 81) / 364
  eot <- 9.87 * sin(2 * B) - 7.53 * cos(B) - 1.5 * sin(B)   # minutes
  12 - eot / 60 - (lon - 15 * std_offset_h) / 15
}
# Sunrise in local standard time (hours), ignoring refraction.
.sunrise_lst <- function(lat, lon, std_offset_h, doy) {
  decl <- 23.44 * sin(2 * pi * (284 + doy) / 365) * pi / 180
  x <- -tan(lat * pi / 180) * tan(decl)
  half <- acos(pmin(1, pmax(-1, x))) * 180 / pi / 15
  .solar_noon_lst(lon, std_offset_h, doy) - half
}
# Solar-radiation-weighted mean hour: where the hour labels put the middle of
# the solar day. Night baseline is removed first.
.solar_centroid <- function(hour, sr) {
  m <- tapply(sr, hour, mean, na.rm = TRUE)
  h <- as.numeric(names(m)); m <- as.numeric(m)
  ok <- is.finite(m)
  if (sum(ok) < 12) return(NA_real_)
  w <- pmax(m[ok] - min(m[ok]), 0)
  if (sum(w) <= 0) return(NA_real_)
  sum(h[ok] * w) / sum(w)
}

# ---- data helpers -----------------------------------------------------------
.fmt_q <- function(x, probs = c(0, 0.01, 0.5, 0.99, 1), digits = 1) {
  q <- stats::quantile(x, probs, na.rm = TRUE, names = FALSE)
  paste0(paste0("p", probs * 100, "=", round(q, digits)), collapse = " ")
}
.pct <- function(x, digits = 1) round(100 * x, digits)
# Longest run of identical, non-missing values: length, value, start index.
.longest_run <- function(x) {
  if (!length(x)) return(list(len = 0L, value = NA, start = NA_integer_))
  r <- rle(x)
  keep <- !is.na(r$values)
  if (!any(keep)) return(list(len = 0L, value = NA, start = NA_integer_))
  lens <- ifelse(keep, r$lengths, 0L)
  i <- which.max(lens)
  list(len = lens[i], value = r$values[i], start = sum(r$lengths[seq_len(i - 1)]) + 1L)
}

# Best single step in a yearly series: the split year that minimises the
# squared error of two flat levels (at least `min_side` years each side).
# Returns the first year of the later level, the step (later - earlier) and
# the residual spread (sd around the two levels).
.step_year <- function(years, x, min_side = 3) {
  ok <- is.finite(x); years <- years[ok]; x <- x[ok]
  n <- length(x)
  if (n < 2 * min_side) return(list(year = NA_integer_, step = NA_real_, spread = NA_real_))
  best <- NULL
  for (k in seq(min_side, n - min_side)) {
    a <- x[seq_len(k)]; b <- x[(k + 1):n]
    sse <- sum((a - mean(a))^2) + sum((b - mean(b))^2)
    if (is.null(best) || sse < best$sse) best <- list(k = k, sse = sse, step = mean(b) - mean(a))
  }
  list(year = years[best$k + 1], step = best$step, spread = sqrt(best$sse / max(n - 2, 1)))
}

# ---- weather download contract ----------------------------------------------
.WX_COLS <- c("station_id", "date", "hour", "record_type", "obs_minute",
              "temperature", "relativeHumidity", "precipitation", "windSpeed",
              "windDirection", "gustSpeed", "gustDirection", "solarRadiation",
              "vpd", "hdw", "dewpoint")

# Structural checks on a fems_download_weather() result. Returns TRUE if all pass.
.check_wx_contract <- function(wx, prefix, station_id) {
  miss <- setdiff(.WX_COLS, names(wx)); extra <- setdiff(names(wx), .WX_COLS)
  c1 <- !length(miss) && !length(extra)
  .check(paste0(prefix, " columns match the download contract"), c1,
         paste0(if (length(miss)) paste0("missing: ", paste(miss, collapse = ", "), " ") else "",
                if (length(extra)) paste0("extra: ", paste(extra, collapse = ", ")) else ""))
  if (!c1 && length(miss)) return(FALSE)
  types <- vapply(wx[c("station_id", "date", "hour", "record_type")], function(v) class(v)[1], character(1))
  c2 <- identical(unname(types), c("character", "Date", "integer", "character"))
  .check(paste0(prefix, " key types chr/Date/int/chr"), c2,
         if (c2) "" else paste(names(types), types, sep = "=", collapse = ", "))
  num <- setdiff(.WX_COLS, c("station_id", "date", "hour", "record_type"))
  isnum <- vapply(wx[num], is.numeric, logical(1))
  c3 <- all(isnum)
  .check(paste0(prefix, " value columns are numeric"), c3,
         if (c3) "" else paste("not numeric:", paste(num[!isnum], collapse = ", ")))
  c4 <- all(wx$station_id == station_id)
  .check(paste0(prefix, " station_id is \"", station_id, "\""), c4)
  c5 <- all(wx$hour %in% 0:23)
  .check(paste0(prefix, " hour in 0..23"), c5)
  dups <- sum(duplicated(wx[c("station_id", "date", "hour", "record_type")]))
  c6 <- dups == 0
  .check(paste0(prefix, " no duplicate station/date/hour/record_type"), c6, paste(dups, "duplicates"))
  c1 && c2 && c3 && c4 && c5 && c6
}
