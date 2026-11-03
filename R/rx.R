# ==============================================================================
# R/rx.R -- prescriptions and the one place hours are flagged in or out
#
# A prescription is a small data frame: var, min, max (NA = no limit on that
# side). Variables and their kinds come from RX_VARS (R/constants.R).
#   linear    min <= x <= max, either side optional
#   circular  wind direction: a clockwise sector from min to max that may
#             wrap through north (315 -> 45). Needs both ends. 360 == 0.
#             A calm hour (wind speed 0) passes, whatever direction it reports.
#
# rx_flag() adds pass_<var> for each constrained variable and in_rx:
#   TRUE  every constrained variable passes
#   FALSE at least one fails and none is missing
#   NA    any constrained variable is missing -> the hour is excluded
# ==============================================================================

# Build a prescription: rx_new(temperature = c(40, 85), relativeHumidity = c(NA, 55),
#                              windDirection = c(315, 45))
rx_new <- function(...) {
  lims <- list(...)
  if (!length(lims)) return(data.frame(var = character(0), min = numeric(0), max = numeric(0),
                                       stringsAsFactors = FALSE))
  if (is.null(names(lims)) || any(!nzchar(names(lims)))) stop("rx_new() needs named limits", call. = FALSE)
  rx <- data.frame(var = names(lims),
                   min = vapply(lims, function(x) as.numeric(x[1]), numeric(1)),
                   max = vapply(lims, function(x) as.numeric(if (length(x) > 1) x[2] else NA), numeric(1)),
                   stringsAsFactors = FALSE, row.names = NULL)
  rx_validate(rx)
}

# Variables a prescription actually constrains (at least one bound set).
rx_constrained <- function(rx) rx$var[!is.na(rx$min) | !is.na(rx$max)]

# Problems with a prescription, as user-facing sentences (character(0) if none).
# The prescription module shows these inline next to the inputs.
rx_problems <- function(rx) {
  p <- character(0)
  if (!all(c("var", "min", "max") %in% names(rx))) return("prescription needs columns var, min, max")
  unknown <- setdiff(rx$var, RX_VARS$var)
  if (length(unknown)) p <- c(p, paste("unknown variable:", paste(unknown, collapse = ", ")))
  if (anyDuplicated(rx$var)) p <- c(p, "a variable appears twice")
  for (i in seq_len(nrow(rx))) {
    v <- rx$var[i]; meta <- RX_VARS[RX_VARS$var == v, ]
    if (!nrow(meta)) next
    lo <- rx$min[i]; hi <- rx$max[i]; lab <- meta$label
    if (meta$kind == "circular") {
      if (xor(is.na(lo), is.na(hi))) p <- c(p, paste0(lab, ": set both ends of the sector, or neither"))
      for (b in c(lo, hi)) if (!is.na(b) && (b < 0 || b > 360)) p <- c(p, paste0(lab, " must be between 0 and 360"))
    } else {
      if (!is.na(lo) && !is.na(hi) && lo > hi) p <- c(p, paste0(lab, ": the minimum is above the maximum"))
      for (b in c(lo, hi)) if (!is.na(b) && (b < meta$slider_min || b > meta$slider_max))
        p <- c(p, paste0(lab, " must be between ", meta$slider_min, " and ", meta$slider_max, " ", meta$units))
    }
  }
  unique(p)
}

rx_validate <- function(rx) {
  p <- rx_problems(rx)
  if (length(p)) stop("prescription problems:\n  - ", paste(p, collapse = "\n  - "), call. = FALSE)
  rx
}

# Is direction d inside the clockwise sector from a to b? (degrees; 360 == 0)
.rx_in_sector <- function(d, a, b) {
  d <- d %% 360; a <- a %% 360; b <- b %% 360
  # a and b are single numbers: plain if, NOT ifelse() (which would return
  # one value and recycle it over every hour)
  if (a <= b) d >= a & d <= b else d >= a | d <= b
}

# Add pass_<var> and only_<var> for each constrained variable, and in_rx.
rx_flag <- function(h, rx) {
  rx <- rx_validate(rx)
  vars <- rx_constrained(rx)
  n <- nrow(h)
  pass_cols <- character(0)
  for (v in vars) {
    i <- which(rx$var == v); lo <- rx$min[i]; hi <- rx$max[i]
    kind <- RX_VARS$kind[RX_VARS$var == v]
    x <- h[[v]]
    if (is.null(x)) stop("hourly table has no column ", v, call. = FALSE)
    if (kind == "circular") {
      p <- .rx_in_sector(x, lo, hi)
      ws <- h$windSpeed
      p[!is.na(ws) & ws == 0] <- TRUE      # calm passes a direction limit
      p[is.na(ws)] <- NA                   # unknown wind speed -> unknown
    } else {
      p <- (is.na(lo) | x >= lo) & (is.na(hi) | x <= hi)
    }
    p[is.na(x) & !(kind == "circular" & !is.na(h$windSpeed) & h$windSpeed == 0)] <- NA
    col <- paste0("pass_", v)
    h[[col]] <- as.logical(p)
    pass_cols <- c(pass_cols, col)
  }
  if (!length(pass_cols)) {
    h$in_rx <- rep(TRUE, n)
  } else {
    m <- as.matrix(h[pass_cols])
    any_na <- rowSums(is.na(m)) > 0
    all_ok <- rowSums(m, na.rm = TRUE) == length(pass_cols)
    h$in_rx <- ifelse(any_na, NA, all_ok)
    # only_<var>: this limit fails and every other constrained limit passes
    # (the one limit holding the hour out). NA wherever in_rx is NA.
    n_pass <- rowSums(m, na.rm = TRUE)
    for (k in seq_along(pass_cols)) {
      o <- !m[, k] & n_pass == length(pass_cols) - 1
      o[any_na] <- NA
      h[[paste0("only_", vars[k])]] <- as.logical(o)
    }
  }
  attr(h, "rx_vars") <- vars
  h
}

# ---- ignition period and minimum window ----------------------------------------------
# mod_rx_server() returns the prescription data frame with two attributes:
#   "ignition"    c(start, end) clock hours (start inclusive, end exclusive), or NULL for all day
#   "min_window"  hours of consecutive in-prescription time that make a usable day
# The ignition period is in local CLOCK time (what a burn plan uses), so at a
# daylight-saving station it is an hour earlier in LST in summer.
rx_ignition <- function(rx) attr(rx, "ignition")
rx_min_window <- function(rx) { m <- attr(rx, "min_window"); if (is.null(m)) SAME_MIN_RUN_H else m }

rx_with_settings <- function(rx, ignition = NULL, min_window = SAME_MIN_RUN_H, preset = NA_character_) {
  if (!is.null(ignition)) {
    ignition <- as.integer(ignition)
    if (length(ignition) != 2 || anyNA(ignition) || ignition[1] < 0 || ignition[2] > 24 || ignition[1] >= ignition[2])
      stop("ignition period must be c(start, end) clock hours with 0 <= start < end <= 24", call. = FALSE)
    if (ignition[1] == 0 && ignition[2] == 24) ignition <- NULL     # all day is no limit
  }
  attr(rx, "ignition") <- ignition
  attr(rx, "min_window") <- as.integer(min_window)
  attr(rx, "preset") <- preset
  rx
}

# TRUE for hours inside the ignition period (all TRUE when there is none).
rx_in_ignition <- function(h, ignition) {
  if (is.null(ignition)) return(rep(TRUE, nrow(h)))
  hc <- h$hour_clock
  !is.na(hc) & hc >= ignition[1] & hc < ignition[2]
}

rx_ignition_text <- function(ignition)
  if (is.null(ignition)) "all day" else sprintf("%02d:00-%02d:00 clock time", ignition[1], ignition[2])
