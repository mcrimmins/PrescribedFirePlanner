# ==============================================================================
# R/shared_cache.R -- small caches shared by every session in one R process
#
# On Posit Connect one R process serves several users (up to "Max connections
# per process"). Without sharing, each session reads and screens its own copy
# of a station and makes its own FEMS call, so a class of 30 on one station
# means 30 copies and 30 identical requests. These caches let sessions in the
# same process reuse one copy. Each process has its own caches; they start
# empty and vanish when Connect stops the process.
#
#   shared_cache_peek(name, key, now)            the value, or NULL if absent/expired
#   shared_cache_set(name, key, value, max_age, max_n, now)
#   shared_cache_get(name, key, compute, max_age, max_n, now)
#                                                peek, else compute() and set
#   shared_cache_info(name)                      data.frame(key, made, used, max_age)
#   shared_cache_clear(name = NULL)              empty one cache, or all
#
# max_age: seconds an entry stays fresh (Inf = until evicted), or a function of
# the value returning seconds. max_n: entries kept; when exceeded, the least
# recently used go first. Errors in compute() propagate and are never cached.
# Values are only read by sessions; R copies on modify, so one session's
# changes never reach another.
#
# Used by mod_station (screened stations, SHARED_STATION_*) and mod_season
# (live top-up + FEMS forecast, SHARED_LIVE_*). Requires R/constants.R.
# ==============================================================================

.shared_caches <- new.env()

.shared_cache_env <- function(name) {
  if (is.null(.shared_caches[[name]])) assign(name, new.env(), envir = .shared_caches)
  .shared_caches[[name]]
}

shared_cache_peek <- function(name, key, now = Sys.time()) {
  e <- .shared_cache_env(name); key <- as.character(key)
  x <- e[[key]]
  if (is.null(x)) return(NULL)
  if (as.numeric(difftime(now, x$made, units = "secs")) > x$max_age) {
    rm(list = key, envir = e); return(NULL)
  }
  x$used <- now; assign(key, x, envir = e)
  x$value
}

shared_cache_set <- function(name, key, value, max_age = Inf, max_n = Inf, now = Sys.time()) {
  e <- .shared_cache_env(name); key <- as.character(key)
  if (is.function(max_age)) max_age <- max_age(value)
  assign(key, list(value = value, made = now, used = now, max_age = as.numeric(max_age)), envir = e)
  # drop expired entries, then the least recently used beyond max_n
  for (k in ls(e)) {
    x <- e[[k]]
    if (as.numeric(difftime(now, x$made, units = "secs")) > x$max_age) rm(list = k, envir = e)
  }
  ks <- ls(e)
  if (length(ks) > max_n) {
    used <- vapply(ks, function(k) as.numeric(e[[k]]$used), numeric(1))
    ord <- ks[order(used, ks == key)]                 # oldest first; the new entry last on ties
    rm(list = ord[seq_len(length(ks) - max_n)], envir = e)
  }
  invisible(value)
}

shared_cache_get <- function(name, key, compute, max_age = Inf, max_n = Inf, now = Sys.time()) {
  v <- shared_cache_peek(name, key, now)
  if (!is.null(v)) return(v)
  v <- compute()
  if (!is.null(v)) shared_cache_set(name, key, v, max_age = max_age, max_n = max_n, now = now)
  v
}

shared_cache_info <- function(name) {
  e <- .shared_cache_env(name); ks <- ls(e)
  if (!length(ks)) return(data.frame(key = character(0), made = as.POSIXct(character(0)),
                                     used = as.POSIXct(character(0)), max_age = numeric(0)))
  data.frame(key = ks,
             made = do.call(c, lapply(ks, function(k) e[[k]]$made)),
             used = do.call(c, lapply(ks, function(k) e[[k]]$used)),
             max_age = vapply(ks, function(k) e[[k]]$max_age, numeric(1)),
             row.names = NULL, stringsAsFactors = FALSE)
}

shared_cache_clear <- function(name = NULL) {
  if (is.null(name)) { rm(list = ls(.shared_caches), envir = .shared_caches); return(invisible()) }
  e <- .shared_cache_env(name); rm(list = ls(e), envir = e); invisible()
}

# ---- what the app shares ------------------------------------------------------------
# A loaded station is fresh forever if it came from the bundled snapshot (the
# file never changes while the app runs); a station downloaded from FEMS
# because it isn't bundled is kept SHARED_STATION_FEMS_HOURS.
shared_station_max_age <- function(st) {
  if (identical(st$source$type, "bundled")) Inf else SHARED_STATION_FEMS_HOURS * 3600
}

# Key for a station's live top-up: the station and the last hour it already
# has, so a session holding a different version of the station (e.g. after
# "Refresh from FEMS") never gets a top-up built for another.
shared_live_key <- function(st) {
  h <- st$hourly
  has <- rowSums(!is.na(h[, c("temperature", "relativeHumidity", "windSpeed")])) > 0
  paste0(st$meta$station_id, "_", max(as.integer(h$date[has]) * 24L + as.integer(h$hour[has])))
}
