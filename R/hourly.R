# ==============================================================================
# R/hourly.R -- the standard hourly table, and loading a station into it
#
# Every data source becomes the same table: one row per station-local
# (LST) hour, no gaps, with calendar and display-only clock-time columns,
# after lenient screening. Nothing downstream knows where the data came from.
#
#   hourly_load_station(id)     raw weather (bundled snapshot or FEMS)
#     -> hourly_from_fems()     observed rows, standard columns, FULL grid
#     -> hourly_add_calendar()  year, month, week, doy, time_clock, hour_clock
#     -> hourly_qc()            clear sensor faults -> NA, with a log
#     -> hourly_validate()      stop() if the table breaks the contract
#
# hourly_load_station() returns a *station object* (a list), not a bare data
# frame, so the screening log and source travel with the data:
#   st$meta     station metadata (name, lat/lon, tz, std offset, ...)
#   st$hourly   the hourly table
#   st$qc_log   one row per screening action
#   st$grid_added  rows added because FEMS sent nothing for that hour
#   st$source   list(type = "bundled" | "FEMS live", detail = "...")
#
# Requires R/constants.R and R/sun.R; the FEMS fallback also needs
# R/fems_download.R.
# ==============================================================================

# ---- station metadata -----------------------------------------------------------
.hourly_meta_env <- new.env()

# One station's metadata from the two metadata CSVs. Cached for the session.
hourly_station_meta <- function(station_id) {
  station_id <- as.character(station_id)
  if (is.null(.hourly_meta_env$a)) {
    .hourly_meta_env$a <- utils::read.csv(STATION_META_FILE, colClasses = c(station_id = "character"),
                                          stringsAsFactors = FALSE)
    .hourly_meta_env$b <- if (file.exists(STATION_MASTER_FILE))
      utils::read.csv(STATION_MASTER_FILE, colClasses = c(station_id = "character"), stringsAsFactors = FALSE)
      else NULL
  }
  a <- .hourly_meta_env$a[.hourly_meta_env$a$station_id == station_id, ]
  if (!nrow(a)) stop("station ", station_id, " is not in ", STATION_META_FILE, call. = FALSE)
  b <- .hourly_meta_env$b
  b <- if (is.null(b)) b else b[b$station_id == station_id, ]
  has_b <- !is.null(b) && nrow(b) > 0
  tz <- a$tz[1]
  list(station_id   = station_id,
       name         = a$station_name[1],
       lat          = as.numeric(a$latitude[1]),
       lon          = as.numeric(a$longitude[1]),
       elev_ft      = as.numeric(a$elevation[1]),
       tz           = tz,
       std_offset_h = sun_std_offset_h(tz),
       has_dst      = sun_has_dst(tz),
       state        = if (has_b) b$state[1] else NA_character_,
       avg_precip_in = if (has_b) suppressWarnings(as.numeric(b$avg_annual_precip[1])) else NA_real_)
}

# ---- raw download -> standard table ---------------------------------------------
# Keeps observed records, standard columns, sorted by date/hour, and completes
# the hourly grid: every LST hour from the first to the last date gets a row,
# NA where FEMS sent nothing. FEMS gap-fills older records but, from about
# Nov 2025, simply leaves missing hours out; without this, two hours either
# side of a gap would look consecutive.
hourly_from_fems <- function(wx, source = "FEMS") {
  need <- c("station_id", "date", "hour", "record_type", "obs_minute", HOURLY_VALUE_COLS)
  miss <- setdiff(need, names(wx))
  if (length(miss)) stop("weather data is missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  wx <- wx[toupper(wx$record_type) == "O" & !is.na(wx$date) & !is.na(wx$hour), , drop = FALSE]
  if (!nrow(wx)) stop("no observed weather rows", call. = FALSE)
  ids <- unique(wx$station_id)
  if (length(ids) != 1) stop("hourly_from_fems() takes one station at a time", call. = FALSE)

  key  <- as.integer(wx$date) * 24L + as.integer(wx$hour)
  if (anyDuplicated(key)) stop("duplicate station/date/hour rows", call. = FALSE)
  d0 <- min(wx$date); d1 <- max(wx$date)
  full_key <- (as.integer(d0) * 24L):(as.integer(d1) * 24L + 23L)
  idx <- match(full_key, key)

  out <- data.frame(station_id = rep(as.character(ids), length(full_key)),
                    source     = source,
                    date       = as.Date(full_key %/% 24L, origin = "1970-01-01"),
                    hour       = as.integer(full_key %% 24L),
                    obs_minute = as.integer(wx$obs_minute[idx]),
                    stringsAsFactors = FALSE)
  for (v in HOURLY_VALUE_COLS) out[[v]] <- as.numeric(wx[[v]][idx])
  attr(out, "grid_added") <- sum(is.na(idx))
  out
}

# ---- calendar and clock time ---------------------------------------------------
# Adds year, month, doy (365-day calendar, Feb 29 -> Feb 28), week (1..52),
# and two display-only columns from the station's IANA zone:
#   time_clock  POSIXct instant, shown in the station's civil (clock) time
#   hour_clock  clock hour, for hour-of-day grouping when the toggle is on
# date/hour stay LST; analysis never uses the clock columns unless asked.
hourly_add_calendar <- function(h, tz) {
  ga <- attr(h, "grid_added")
  lt <- as.POSIXlt(h$date)
  yr <- lt$year + 1900L
  leap <- (yr %% 4L == 0L & yr %% 100L != 0L) | yr %% 400L == 0L
  doy <- lt$yday + 1L
  doy <- ifelse(leap & doy >= 60L, doy - 1L, doy)    # Feb 29 (60) -> 59; Mar 1 (61) -> 60
  h$year  <- as.integer(yr)
  h$month <- as.integer(lt$mon + 1L)
  h$doy   <- as.integer(doy)
  h$week  <- pmin(as.integer(ceiling(doy / 7)), 52L)

  std <- sun_std_offset_h(tz)
  inst <- as.POSIXct(h$date) + h$hour * 3600 - std * 3600    # LST wall time -> UTC instant
  attr(inst, "tzone") <- tz
  h$time_clock <- inst
  h$hour_clock <- as.integer(as.POSIXlt(inst, tz = tz)$hour)
  attr(h, "grid_added") <- ga
  h
}

# ---- lenient screening ------------------------------------------------------------
# Sets clear sensor faults to NA and returns the table with attr "qc_log":
# one row per action (rule, variable, start_date, start_hour, hours, days, value).
# Order: physical limits, one-hour spikes, stuck values, long calm.
# A reading past a physical limit means the sensor was failing (BLOXHAM,
# Feb 2005: 112 F the day before it passed 130 F, then a week at 126-140 F),
# so that variable is removed for the whole local day plus QC_LIMITS_PAD_DAYS
# either side, not just the impossible readings.
# Derived columns follow their inputs: dewpoint/vpd when temperature or RH is
# removed, hdw when wind or vpd is removed.
hourly_qc <- function(h) {
  ga <- attr(h, "grid_added")
  log <- list()
  add_log <- function(rule, var, i0, n, value, days = NA_integer_) {
    log[[length(log) + 1L]] <<- data.frame(rule = rule, variable = var,
                                           start_date = h$date[i0], start_hour = h$hour[i0],
                                           hours = as.integer(n), days = as.integer(days), value = value,
                                           stringsAsFactors = FALSE)
  }
  removed <- list()   # var -> logical index of values this function set to NA
  set_na <- function(var, idx) {
    h[[var]][idx] <<- NA
    removed[[var]] <<- if (is.null(removed[[var]])) idx else (removed[[var]] | idx)
  }

  # 1. physical limits: remove the whole local day of any day with a reading
  #    past the limit (one log row per variable; hours = values removed)
  for (i in seq_len(nrow(QC_LIMITS))) {
    v <- QC_LIMITS$var[i]; x <- h[[v]]
    bad <- !is.na(x) & (x < QC_LIMITS$min[i] | x > QC_LIMITS$max[i])
    if (any(bad)) {
      bad_days <- unique(h$date[bad])
      pad <- unique(do.call(c, lapply(-QC_LIMITS_PAD_DAYS:QC_LIMITS_PAD_DAYS, function(k) bad_days + k)))
      day_idx <- h$date %in% pad & !is.na(x)
      add_log("limits", v, which(bad)[1], sum(day_idx), NA_real_, days = length(bad_days))
      set_na(v, day_idx)
    }
  }

  # 2. one-hour spikes: differs from BOTH neighbours, in the same direction,
  #    by more than the threshold (rows are consecutive hours: the grid is full)
  n <- nrow(h)
  for (v in names(QC_SPIKE)) {
    x <- h[[v]]; thr <- QC_SPIKE[[v]]
    up   <- c(NA, x[-1] - x[-n]); down <- c(x[-1] - x[-n], NA)    # x[i]-x[i-1], x[i+1]-x[i]
    spike <- !is.na(up) & !is.na(down) & ((up > thr & down < -thr) | (up < -thr & down > thr))
    if (any(spike)) { add_log("spike", v, which(spike)[1], sum(spike), NA_real_); set_na(v, spike) }
  }

  # 3. stuck values: runs of an identical value >= QC_STUCK_HOURS (daylight
  #    solar: >= QC_SOLAR_STUCK_HOURS), ignoring values that are legitimately
  #    constant (RH 100, calm wind, night solar)
  for (v in QC_STUCK_VARS) {
    x <- h[[v]]
    if (v == "relativeHumidity") x[x >= 100] <- NA
    if (v %in% c("windSpeed", "gustSpeed")) x[x == 0] <- NA
    if (v == "windDirection") x[is.na(h$windSpeed) | h$windSpeed == 0] <- NA
    if (v == "solarRadiation") x[x <= QC_SOLAR_DAY_MIN] <- NA
    r <- rle(x)
    vals <- r$values; lens <- r$lengths
    ends <- cumsum(lens); starts <- ends - lens + 1L
    # Daylight solar is broken up by night, so it can never reach 24 h; a
    # run of identical daylight readings uses its own, shorter threshold.
    min_len <- if (v == "solarRadiation") QC_SOLAR_STUCK_HOURS else QC_STUCK_HOURS
    hit <- which(!is.na(vals) & lens >= min_len)
    for (k in hit) {
      idx <- seq(starts[k], ends[k])
      add_log("stuck", v, starts[k], lens[k], vals[k])
      set_na(v, seq_len(n) %in% idx)
    }
  }

  # 4. long calm: wind exactly 0 for more than QC_CALM_HOURS
  calm <- !is.na(h$windSpeed) & h$windSpeed == 0
  r <- rle(calm); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
  for (k in which(r$values & r$lengths > QC_CALM_HOURS)) {
    idx <- seq_len(n) %in% seq(starts[k], ends[k])
    add_log("long calm", "windSpeed", starts[k], r$lengths[k], 0)
    for (v in c("windSpeed", "gustSpeed", "windDirection", "gustDirection")) set_na(v, idx)
  }

  # derived columns follow their inputs
  rm_any <- function(vars) {
    out <- rep(FALSE, n)
    for (v in vars) if (!is.null(removed[[v]])) out <- out | removed[[v]]
    out
  }
  th <- rm_any(c("temperature", "relativeHumidity"))
  if (any(th)) { h$dewpoint[th] <- NA; h$vpd[th] <- NA }
  wv <- rm_any(c("windSpeed")) | th
  if (any(wv)) h$hdw[wv] <- NA

  qlog <- if (length(log)) do.call(rbind, log) else
    data.frame(rule = character(0), variable = character(0), start_date = as.Date(character(0)),
               start_hour = integer(0), hours = integer(0), days = integer(0), value = numeric(0),
               stringsAsFactors = FALSE)
  attr(h, "qc_log") <- qlog
  attr(h, "grid_added") <- ga
  h
}

# ---- contract ------------------------------------------------------------------------
HOURLY_COLS <- c("station_id", "source", "date", "hour", "obs_minute", HOURLY_VALUE_COLS,
                 "year", "month", "doy", "week", "time_clock", "hour_clock")

# Problems with an hourly table (character(0) if none).
hourly_problems <- function(h) {
  p <- character(0)
  miss <- setdiff(HOURLY_COLS, names(h))
  if (length(miss)) return(paste("missing columns:", paste(miss, collapse = ", ")))
  if (!inherits(h$date, "Date")) p <- c(p, "date is not a Date")
  if (!is.integer(h$hour) || any(!h$hour %in% 0:23)) p <- c(p, "hour is not an integer 0..23")
  if (length(unique(h$station_id)) != 1) p <- c(p, "more than one station")
  key <- as.integer(h$date) * 24L + h$hour
  if (anyDuplicated(key)) p <- c(p, "duplicate date/hour rows")
  if (length(key) > 1 && any(diff(key) != 1L)) p <- c(p, "hours are not consecutive (grid incomplete or unsorted)")
  if (any(!h$doy %in% seq_len(CLIM_DOY_N), na.rm = TRUE)) p <- c(p, "doy outside 1..365")
  if (any(!h$week %in% 1:52)) p <- c(p, "week outside 1..52")
  for (v in HOURLY_VALUE_COLS) if (!is.numeric(h[[v]])) p <- c(p, paste(v, "is not numeric"))
  for (i in seq_len(nrow(QC_LIMITS))) {
    x <- h[[QC_LIMITS$var[i]]]
    if (any(x < QC_LIMITS$min[i] | x > QC_LIMITS$max[i], na.rm = TRUE))
      p <- c(p, paste(QC_LIMITS$var[i], "outside physical limits after screening"))
  }
  p
}

hourly_validate <- function(h) {
  p <- hourly_problems(h)
  if (length(p)) stop("hourly table fails its contract:\n  - ", paste(p, collapse = "\n  - "), call. = FALSE)
  invisible(h)
}

# ---- bundled snapshots ---------------------------------------------------------------
# The manifest of bundled snapshots (empty data frame if none).
hourly_bundled_manifest <- function() {
  if (!file.exists(BUNDLED_MANIFEST)) return(data.frame(station_id = character(0)))
  utils::read.csv(BUNDLED_MANIFEST, colClasses = c(station_id = "character", md5 = "character"),
                  stringsAsFactors = FALSE)
}

# Raw weather from a bundled snapshot, or NULL (with a warning) if the station
# isn't bundled, the file is missing, or its checksum doesn't match.
hourly_read_bundled <- function(station_id) {
  station_id <- as.character(station_id)
  mf <- hourly_bundled_manifest()
  m <- mf[mf$station_id == station_id, ]
  if (!nrow(m)) return(NULL)
  path <- file.path(BUNDLED_DIR, m$file[1])
  if (!file.exists(path)) { warning("bundled snapshot missing: ", path, call. = FALSE); return(NULL) }
  if (!identical(unname(tools::md5sum(path)), m$md5[1])) {
    warning("bundled snapshot checksum mismatch: ", path, call. = FALSE); return(NULL)
  }
  wx <- tryCatch(readRDS(path), error = function(e) {
    warning("could not read ", path, ": ", conditionMessage(e), call. = FALSE); NULL })
  if (is.null(wx)) return(NULL)
  attr(wx, "bundled") <- list(downloaded_at = m$downloaded_at[1], start = m$start[1], end = m$end[1])
  wx
}

# ---- the whole pipeline -----------------------------------------------------------------
# Returns a station object (see the header). prefer_bundled = FALSE is the
# app's "Refresh from FEMS" button. `progress`, if given, is called as
# progress(fraction, message).
hourly_load_station <- function(station_id, prefer_bundled = TRUE,
                                start_date = FEMS_RECORD_START, end_date = Sys.Date() - 1,
                                progress = NULL) {
  tick <- function(f, msg) if (is.function(progress)) progress(f, msg)
  station_id <- as.character(station_id)
  meta <- hourly_station_meta(station_id)

  raw <- NULL; src <- NULL
  if (prefer_bundled) {
    tick(0.1, "Reading bundled snapshot...")
    raw <- hourly_read_bundled(station_id)
    if (!is.null(raw)) {
      b <- attr(raw, "bundled")
      src <- list(type = "bundled", detail = paste0("bundled snapshot of ", substr(b$downloaded_at, 1, 10)),
                  downloaded_at = b$downloaded_at)
    }
  }
  if (is.null(raw)) {
    if (!exists("fems_download_weather", mode = "function"))
      stop("no bundled snapshot for ", station_id, " and the FEMS download layer is not loaded", call. = FALSE)
    tick(0.1, "Downloading from FEMS (about 30 s)...")
    raw <- fems_download_weather(station_id, start_date, end_date, verbose = FALSE)
    if (is.null(raw) || !nrow(raw)) stop("FEMS returned no weather for station ", station_id, call. = FALSE)
    src <- list(type = "FEMS live", detail = paste0("downloaded from FEMS ", format(Sys.time(), "%Y-%m-%d %H:%M")),
                downloaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  }

  tick(0.7, "Preparing hourly data...")
  h <- hourly_from_fems(raw, source = if (src$type == "bundled") "FEMS (bundled)" else "FEMS")
  h <- hourly_add_calendar(h, meta$tz)
  tick(0.85, "Screening for sensor faults...")
  h <- hourly_qc(h)
  hourly_validate(h)
  tick(1, "Done.")

  qc_log <- attr(h, "qc_log"); grid_added <- attr(h, "grid_added")
  attr(h, "qc_log") <- NULL; attr(h, "grid_added") <- NULL
  list(meta = meta, hourly = h, qc_log = qc_log, grid_added = grid_added, source = src)
}

# Rows of the hourly table inside the climatology baseline.
hourly_baseline <- function(h, years = RX_BASELINE_YEARS) h[h$year %in% years, , drop = FALSE]

# ---- one window of the year, every year (Same dates tab) ---------------------------
# Rows of `h` whose month-day falls in the window start..end (the years of the
# two dates are ignored). A window may wrap through New Year (20 Dec to 10 Jan);
# its days then belong to the year it starts in. Adds wyear (the window's year)
# and wday ("15 Mar"). attr "window_days" gives every day label in order,
# 29 Feb included when the window spans it (non-leap years have no row for it).
.hourly_md <- function(d) { lt <- as.POSIXlt(d); (lt$mon + 1L) * 100L + lt$mday }

hourly_window <- function(h, start, end) {
  smd <- .hourly_md(start); emd <- .hourly_md(end)
  wrap <- smd > emd
  md <- .hourly_md(h$date)
  keep <- if (wrap) md >= smd | md <= emd else md >= smd & md <= emd
  out <- h[keep, , drop = FALSE]
  md <- md[keep]
  out$wyear <- as.integer(out$year - (wrap & md <= emd))
  out$wday <- format(out$date, "%d %b")
  # day labels from leap-capable reference years (2003-2004 when wrapping, so Feb 29 can appear after New Year)
  ref <- function(y, x) as.Date(sprintf("%d-%02d-%02d", y, x %/% 100, x %% 100))
  days <- if (wrap) seq(ref(2003, smd), ref(2004, emd), by = "day") else seq(ref(2004, smd), ref(2004, emd), by = "day")
  attr(out, "window_days") <- format(days, "%d %b")
  attr(out, "window_wrap") <- wrap
  out
}

# Number of days the window has in a given window-year (leap years can have one more).
hourly_window_ndays <- function(wyear, start, end) {
  smd <- .hourly_md(start); emd <- .hourly_md(end); wrap <- smd > emd
  one <- function(y) {
    a <- as.Date(sprintf("%d-%02d-%02d", y, smd %/% 100, smd %% 100))
    b <- as.Date(sprintf("%d-%02d-%02d", y + wrap, emd %/% 100, emd %% 100))
    as.integer(b - a) + 1L
  }
  vapply(wyear, one, integer(1))
}
