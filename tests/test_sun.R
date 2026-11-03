# ==============================================================================
# tests/test_sun.R -- R/sun.R against known sun times (no network)
#
# Reference times are published almanac values (NOAA solar calculator),
# converted to local STANDARD time. Tolerance is 8 minutes: plenty for
# hour-of-day climatology and for spotting a wrong sign or offset (which
# would be off by an hour or more).
#
#     source("tests/test_sun.R")
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_sun")
  tol_min <- 8

  tryCatch({
    .env_header("Sun times and UTC offsets")
    .section("A. Setup")
    .source_core("A")

    .section("B. Standard offsets and DST")
    cases <- data.frame(tz = c("America/Phoenix", "America/Denver", "America/New_York", "America/Los_Angeles"),
                        std = c(-7, -7, -5, -8), dst = c(FALSE, TRUE, TRUE, TRUE), stringsAsFactors = FALSE)
    for (i in seq_len(nrow(cases))) {
      o <- sun_std_offset_h(cases$tz[i]); d <- sun_has_dst(cases$tz[i])
      .check(paste0("B", i, " ", cases$tz[i], ": standard offset ", cases$std[i], ", DST ", cases$dst[i]),
             o == cases$std[i] && d == cases$dst[i], paste0("got ", o, ", DST ", d))
    }

    .section("C. Sun times vs almanac (LST)")
    hm <- function(s) { p <- as.numeric(strsplit(s, ":")[[1]]); p[1] + p[2] / 60 }
    ref <- data.frame(
      place = c("Tucson", "Tucson", "Tucson", "Tallahassee", "Tallahassee"),
      lat = c(32.22, 32.22, 32.22, 30.44, 30.44), lon = c(-110.97, -110.97, -110.97, -84.28, -84.28),
      std = c(-7, -7, -7, -5, -5), doy = c(172, 172, 355, 172, 355),
      what = c("sunrise", "sunset", "sunrise", "sunrise", "sunset"),
      expect = c("05:19", "19:33", "07:18", "05:31", "17:39"),
      stringsAsFactors = FALSE)
    for (i in seq_len(nrow(ref))) {
      st <- sun_times(ref$lat[i], ref$lon[i], ref$std[i], ref$doy[i])
      got <- st[[ref$what[i]]]; err <- (got - hm(ref$expect[i])) * 60
      .check(sprintf("C%d %s %s, doy %d: %s LST", i, ref$place[i], ref$what[i], ref$doy[i], ref$expect[i]),
             abs(err) <= tol_min, sprintf("got %s (%+.0f min)", sun_fmt_hm(got), err))
    }
    noon <- sun_noon_lst(-110.97, -7, 172)
    .check("C6 Tucson solar noon, 21 Jun, ~12:26 LST", abs((noon - hm("12:26")) * 60) <= tol_min,
           paste("got", sun_fmt_hm(noon)))

    .section("D. Shape")
    st <- sun_times(32.31, -110.82, -7)
    .check("D1 one row per day of the year", nrow(st) == 365 && identical(st$doy, 1:365))
    .check("D2 civil dawn < sunrise < noon < sunset < civil dusk, every day",
           all(st$civil_dawn < st$sunrise & st$sunrise < st$noon & st$noon < st$sunset & st$sunset < st$civil_dusk))
    .check("D3 longest day near 21 Jun, shortest near 21 Dec",
           abs(which.max(st$sunset - st$sunrise) - 172) <= 3 && abs(which.min(st$sunset - st$sunrise) - 355) <= 3)
    .check("D4 sun_fmt_hm formats hours", identical(sun_fmt_hm(c(12.5, 5.25, 23.999)), c("12:30", "05:15", "00:00")))

    .section("E. Night spans (Past event shading)")
    nn <- sun_nights(32.2, -110.9, -7, as.Date(c("2024-03-10", "2024-03-12")))
    .check("E1 one night per day plus the night before the first day (4 spans for 3 days)", nrow(nn) == 4, nrow(nn))
    .check("E2 each night runs sunset to next sunrise: 10-14 h long in March at 32 N",
           all(nn$to - nn$from > 10 & nn$to - nn$from < 14), paste(round(nn$to - nn$from, 1), collapse = ", "))
    .check("E3 spans are hours from the first day's midnight: the first ends near 06-07 LST (sunrise)",
           nn$to[1] > 6 && nn$to[1] < 7.2, round(nn$to[1], 2))
  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  .test_finish()
})
