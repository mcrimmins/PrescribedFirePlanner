# ==============================================================================
# tests/test_shared_cache.R -- caches shared by the sessions in one R process
#
#   A  R/shared_cache.R on its own: hits, expiry, least-recently-used cap,
#      errors and NULLs not cached, per-value max age, clearing
#   B  Station tab: a second session loading the same bundled station reuses
#      the first one's copy (no second load); "Refresh from FEMS" isn't shared
#   C  Season tracker: a second session on the same station reuses the live
#      top-up (one fake FEMS call for both); Refresh fetches again; with a fake
#      fetch and no share = TRUE nothing is shared (the app default)
#   D  Memory: size of one shared station, and the worst case per process
#
# No network (C uses a fake FEMS fetch). Run from the project root:
#     source("tests/test_shared_cache.R")
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_shared_cache")

  tryCatch({
    .env_header("Shared caches (no network)")
    .section("A. R/shared_cache.R")
    suppressPackageStartupMessages({ library(shiny); library(bslib) })
    .source_core("A")
    for (f in c("fems_download", "tooltips", "plots", "mod_station", "mod_season")) {
      r <- .run(paste("source", f), source(file.path("R", paste0(f, ".R"))))
      .check(paste0("A4 R/", f, ".R loads"), r$ok)
    }
    shared_cache_clear()
    t0 <- as.POSIXct("2026-11-12 08:00:00", tz = "UTC")
    n <- 0; mk <- function() { n <<- n + 1; n }
    a <- shared_cache_get("t", "k1", mk, max_age = 60, now = t0)
    b <- shared_cache_get("t", "k1", mk, max_age = 60, now = t0 + 30)
    .check("A5.1 a second get within max_age is a hit (compute runs once)", a == 1 && b == 1 && n == 1)
    c3 <- shared_cache_get("t", "k1", mk, max_age = 60, now = t0 + 61)
    .check("A5.2 after max_age the value is computed again", c3 == 2 && n == 2)
    .check("A5.3 peek of a missing key is NULL", is.null(shared_cache_peek("t", "nope", now = t0)))

    shared_cache_clear("t")
    for (i in 1:3) shared_cache_set("t", paste0("s", i), i, max_n = 3, now = t0 + i)
    shared_cache_peek("t", "s1", now = t0 + 10)                 # s1 is now the most recently used
    shared_cache_set("t", "s4", 4, max_n = 3, now = t0 + 11)
    .check("A5.4 over max_n the least recently used entry goes (s2), not the oldest made (s1)",
           setequal(shared_cache_info("t")$key, c("s1", "s3", "s4")), paste(shared_cache_info("t")$key, collapse = ", "))

    shared_cache_clear("t")
    e <- tryCatch(shared_cache_get("t", "bad", function() stop("FEMS down"), now = t0), error = function(e) "err")
    .check("A5.5 an error in compute propagates and is not cached", identical(e, "err") && !nrow(shared_cache_info("t")))
    shared_cache_get("t", "null", function() NULL, now = t0)
    .check("A5.6 a NULL result is not cached", !nrow(shared_cache_info("t")))
    shared_cache_set("t", "f", list(kind = "x"), max_age = function(v) if (v$kind == "x") 5 else Inf, now = t0)
    .check("A5.7 max_age can depend on the value", shared_cache_info("t")$max_age == 5 &&
             is.null(shared_cache_peek("t", "f", now = t0 + 6)))
    .check("A5.8 bundled stations never expire; FEMS-downloaded ones last SHARED_STATION_FEMS_HOURS",
           shared_station_max_age(list(source = list(type = "bundled"))) == Inf &&
             shared_station_max_age(list(source = list(type = "FEMS live"))) == SHARED_STATION_FEMS_HOURS * 3600)
    shared_cache_set("u", "x", 1); shared_cache_clear()
    .check("A5.9 shared_cache_clear() empties every cache", !nrow(shared_cache_info("u")) && !nrow(shared_cache_info("t")))

    .section("B. Station tab: two sessions, one load")
    mf <- hourly_bundled_manifest()
    if (!nrow(mf)) stop("no bundled stations")
    feat <- utils::read.csv("data/stations_featured.csv", colClasses = c(station_id = "character"), stringsAsFactors = FALSE)
    sid0 <- intersect(feat$station_id, mf$station_id)[1]   # not `sid`: testServer code runs inside the module, which has its own `sid`
    if (is.na(sid0)) stop("no featured station is bundled")
    loads <- 0
    real_load <- get("hourly_load_station", envir = globalenv())
    assign("hourly_load_station", function(...) { loads <<- loads + 1; real_load(...) }, envir = globalenv())
    st_a <- st_b <- NULL; secs <- c(NA, NA)
    shiny::testServer(mod_station_server, {
      t1 <- Sys.time(); do.call(session$setInputs, stats::setNames(list(1), paste0("feat_", sid0)))
      secs[1] <<- as.numeric(difftime(Sys.time(), t1, units = "secs")); st_a <<- session$returned$st()
    })
    shiny::testServer(mod_station_server, {
      t1 <- Sys.time(); do.call(session$setInputs, stats::setNames(list(1), paste0("feat_", sid0)))
      secs[2] <<- as.numeric(difftime(Sys.time(), t1, units = "secs")); st_b <<- session$returned$st()
    })
    .check("B1 the first session loads the station", !is.null(st_a) && identical(st_a$meta$station_id, sid0))
    .check("B2 the second session gets it without loading again", loads == 1 && identical(st_a, st_b),
           sprintf("%d load(s); %.2f s then %.2f s", loads, secs[1], secs[2]))
    .check("B3 the shared entry is keyed by station and never expires (bundled)",
           sid0 %in% shared_cache_info("station")$key && shared_cache_info("station")$max_age[shared_cache_info("station")$key == sid0] == Inf)
    shiny::testServer(mod_station_server, {
      do.call(session$setInputs, stats::setNames(list(1), paste0("feat_", sid0)))
      # Refresh from FEMS: stub the loader so no network is used
      assign("hourly_load_station", function(...) { loads <<- loads + 1; s <- real_load(sid0); s$source$type <- "FEMS live"; s },
             envir = globalenv())
      session$setInputs(refresh = 1)
      .check("B4 'Refresh from FEMS' loads afresh and doesn't replace the shared copy",
             loads == 2 && identical(session$returned$st()$source$type, "FEMS live") &&
               identical(shared_cache_peek("station", sid0)$source$type, "bundled"))
    })
    assign("hourly_load_station", real_load, envir = globalenv())

    .section("C. Season tracker: two sessions, one FEMS call")
    st0 <- st_a
    now0 <- as.POSIXct(paste(max(st0$hourly$date) + 2, "08:30"), tz = st0$meta$tz)
    calls <- 0
    fake <- function(id, start, end, verbose = FALSE, ...) {
      calls <<- calls + 1
      d <- .fake_wx(seq(as.Date(start), as.Date(end), by = "day"), station_id = id)
      k <- as.integer(d$date) * 24 + d$hour; cut <- as.integer(as.Date(format(now0, tz = st0$meta$tz))) * 24 + 8
      d$record_type <- ifelse(k <= cut, "O", "F"); d[k <= cut + 7 * 24 + 3, ]
    }
    ex_on <- RX_EXAMPLE[RX_EXAMPLE$on, ]
    rx0 <- rx_with_settings(rx_validate(data.frame(var = ex_on$var, min = ex_on$min, max = ex_on$max, stringsAsFactors = FALSE)),
                            ignition = c(10, 17), min_window = 4)
    args <- list(st = shiny::reactive(st0), rx = shiny::reactive(rx0), active = shiny::reactive(TRUE),
                 fetch = fake, now = function() now0, share = TRUE)
    n_a <- n_b <- NA
    shiny::testServer(mod_season_server, args = args, { session$flushReact(); n_a <<- nrow(st_full()$hourly) })
    shiny::testServer(mod_season_server, args = args, { session$flushReact(); n_b <<- nrow(st_full()$hourly) })
    .check("C1 two sessions on the same station make one FEMS call", calls == 1, paste(calls, "call(s)"))
    .check("C2 ... and both get the same observations appended", identical(n_a, n_b) && n_a > nrow(st0$hourly),
           paste(n_a - nrow(st0$hourly), "hours added"))
    .check("C3 the live entry expires after SHARED_LIVE_MINUTES",
           shared_cache_info("live")$max_age[1] == SHARED_LIVE_MINUTES * 60)
    shiny::testServer(mod_season_server, args = args, {
      session$flushReact(); session$setInputs(refresh = 1)
      .check("C4 Refresh on the Season tracker fetches again", calls == 2, paste(calls, "call(s)"))
    })
    shared_cache_clear("live"); calls <- 0
    args$share <- NULL
    for (i in 1:2) shiny::testServer(mod_season_server, args = args, session$flushReact())
    .check("C5 with a fake fetch and no share argument nothing is shared (one call per session)",
           calls == 2 && !nrow(shared_cache_info("live")), paste(calls, "call(s)"))
    st_other <- st0; st_other$hourly <- st0$hourly[st0$hourly$date < max(st0$hourly$date) - 5, ]
    .check("C6 a station holding fewer hours gets a different live key",
           shared_live_key(st_other) != shared_live_key(st0))

    .section("D. Memory")
    mb <- as.numeric(utils::object.size(st0)) / 2^20
    .info("D1 one shared station", sprintf("%.0f MB; up to %d per process = %.0f MB; with 6 processes up to %.1f GB",
                                           mb, SHARED_STATION_MAX, mb * SHARED_STATION_MAX, 6 * mb * SHARED_STATION_MAX / 1024))
    shared_cache_clear()
  }, error = function(e) {
    if (exists("real_load")) assign("hourly_load_station", real_load, envir = globalenv())
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  .test_finish()
})
