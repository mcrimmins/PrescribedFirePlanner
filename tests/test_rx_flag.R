# ==============================================================================
# tests/test_rx_flag.R -- prescriptions and flagging, no network
#
# Hand-built hours with hand-checked answers: limit edges (inclusive),
# one-sided limits, wind sectors that wrap through north, 360 == 0, calm
# hours passing a direction limit, how missing values propagate, and only_<var>
# (the one limit holding an hour out).
#
#     source("tests/test_rx_flag.R")
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_rx_flag")

  tryCatch({
    .env_header("Prescription flagging (no network)")
    .section("A. Setup")
    .source_core("A")

    # 12 hand-made hours
    h <- data.frame(
      temperature      = c(40, 85, 39.9, 85.1, 60, NA, 60, 60, 60, 60, 60, 60),
      relativeHumidity = c(25, 55, 30, 30, 24, 30, NA, 30, 30, 30, 30, 30),
      windSpeed        = c(5, 5, 5, 5, 5, 5, 5, 0, NA, 5, 5, 0),
      windDirection    = c(0, 360, 20, 45, 46, 314, 315, 200, 10, NA, 180, NA),
      precipitation    = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.02, 0),
      solarRadiation   = NA)

    .section("B. Linear limits")
    f <- rx_flag(h, rx_new(temperature = c(40, 85)))
    .check("B1 limits are inclusive: 40 and 85 pass, 39.9 and 85.1 fail",
           identical(f$pass_temperature[1:4], c(TRUE, TRUE, FALSE, FALSE)))
    .check("B2 missing temperature -> pass NA and in_rx NA", is.na(f$pass_temperature[6]) && is.na(f$in_rx[6]))
    f <- rx_flag(h, rx_new(relativeHumidity = c(NA, 55)))
    .check("B3 max only: RH 24 and 55 pass, nothing below is rejected", isTRUE(f$pass_relativeHumidity[5]) && isTRUE(f$pass_relativeHumidity[2]))
    f <- rx_flag(h, rx_new(relativeHumidity = c(25, NA)))
    .check("B4 min only: RH 24 fails, 25 passes", identical(f$pass_relativeHumidity[c(5, 1)], c(FALSE, TRUE)))
    f <- rx_flag(h, rx_new(precipitation = c(NA, 0)))
    .check("B5 no-rain limit: 0.02 in fails", identical(f$pass_precipitation[c(1, 11)], c(TRUE, FALSE)))

    .section("C. Wind-direction sectors")
    f <- rx_flag(h, rx_new(windDirection = c(315, 45)))
    .check("C1 315 -> 45 wraps through north: 0, 360, 20, 45 pass",
           identical(f$pass_windDirection[1:4], c(TRUE, TRUE, TRUE, TRUE)))
    .check("C2 ... 46 and 314 fail, 315 passes", identical(f$pass_windDirection[5:7], c(FALSE, FALSE, TRUE)))
    .check("C3 calm hour passes whatever direction it reports (200 deg, 0 mph)", isTRUE(f$pass_windDirection[8]))
    .check("C4 calm hour with no direction still passes", isTRUE(f$pass_windDirection[12]))
    .check("C5 unknown wind speed -> direction NA", is.na(f$pass_windDirection[9]))
    .check("C6 missing direction with wind blowing -> NA", is.na(f$pass_windDirection[10]))
    f <- rx_flag(h, rx_new(windDirection = c(180, 270)))
    .check("C7 non-wrapping sector 180 -> 270: 180 passes, 0 and 46 fail",
           identical(f$pass_windDirection[c(11, 1, 5)], c(TRUE, FALSE, FALSE)))

    .section("D. Combining limits into in_rx")
    rx <- rx_new(temperature = c(40, 85), relativeHumidity = c(25, 55))
    f <- rx_flag(h, rx)
    .check("D1 all pass -> TRUE (rows 1, 2)", identical(f$in_rx[1:2], c(TRUE, TRUE)))
    .check("D2 one fails, none missing -> FALSE (rows 3, 5)", identical(f$in_rx[c(3, 5)], c(FALSE, FALSE)))
    .check("D3 a constrained value missing -> NA, even if another fails (row 7: RH missing)", is.na(f$in_rx[7]))
    .check("D4 unconstrained variables are ignored, even when missing (solar all NA)", !anyNA(f$in_rx[-c(6, 7)]))
    .check("D5 pass_ columns only for constrained variables",
           setequal(grep("^pass_", names(f), value = TRUE), c("pass_temperature", "pass_relativeHumidity")))
    .check("D6 an empty prescription puts every hour in", all(rx_flag(h, rx_new())$in_rx))
    .check("D7 rx_constrained() lists variables with a bound", identical(rx_constrained(rx), c("temperature", "relativeHumidity")))
    .check("D8 only_<var>: the one failing limit is flagged (row 3 temperature only, row 5 RH only), not a passing hour",
           identical(f$only_temperature[c(1, 3, 5)], c(FALSE, TRUE, FALSE)) &&
             identical(f$only_relativeHumidity[c(1, 3, 5)], c(FALSE, FALSE, TRUE)))
    .check("D9 only_<var> is NA wherever in_rx is NA (rows 6, 7)",
           all(is.na(f$only_temperature[6:7])) && all(is.na(f$only_relativeHumidity[6:7])))
    f2 <- rx_flag(h, rx_new(temperature = c(40, 85), relativeHumidity = c(31, 55)))
    .check("D10 both limits failing -> neither only_ column is TRUE (row 3: 39.9 F, RH 30)",
           identical(c(f2$only_temperature[3], f2$only_relativeHumidity[3]), c(FALSE, FALSE)))
    f1 <- rx_flag(h, rx_new(temperature = c(40, 85)))
    .check("D11 with one limit on, only_<var> is !pass_<var>", identical(f1$only_temperature, !f1$pass_temperature))
    .check("D12 in_rx + every only_ + 'two or more failing' account for every valid hour",
           { ok <- !is.na(f$in_rx); two <- ok & !f$in_rx & !f$only_temperature & !f$only_relativeHumidity
             all(f$in_rx[ok] + f$only_temperature[ok] + f$only_relativeHumidity[ok] + two[ok] == 1) })

    .section("D2. Ignition period and minimum window")
    hh <- data.frame(hour_clock = c(9L, 10L, 16L, 17L, NA))
    .check("D2.1 ignition 10-17 clock: 10 and 16 are in, 9 and 17 are not (end exclusive), unknown is not",
           identical(rx_in_ignition(hh, c(10L, 17L)), c(FALSE, TRUE, TRUE, FALSE, FALSE)))
    .check("D2.2 no ignition period: every hour counts", all(rx_in_ignition(hh, NULL)))
    rs <- rx_with_settings(rx_new(temperature = c(40, 85)), ignition = c(10, 17), min_window = 5, preset = "Grass")
    .check("D2.3 settings ride on the prescription as attributes", identical(rx_ignition(rs), c(10L, 17L)) && rx_min_window(rs) == 5 &&
             identical(attr(rs, "preset"), "Grass"))
    .check("D2.4 0-24 is the same as no ignition period", is.null(rx_ignition(rx_with_settings(rx_new(), ignition = c(0, 24)))))
    .check("D2.5 a backwards period is refused", inherits(tryCatch(rx_with_settings(rx_new(), ignition = c(17, 10)), error = identity), "error"))
    .check("D2.6 rx_flag ignores the attributes (same flags with or without)",
           identical(rx_flag(h, rs)$in_rx, rx_flag(h, rx_new(temperature = c(40, 85)))$in_rx))
    .check("D2.7 every preset is valid and has a label, an ignition period (or none) and a window",
           all(vapply(unique(RX_PRESETS$preset), function(p) { r <- RX_PRESETS[RX_PRESETS$preset == p & RX_PRESETS$on, ]
             !length(rx_problems(data.frame(var = r$var, min = r$min, max = r$max))) }, logical(1))) &&
             setequal(RX_PRESET_META$preset, unique(RX_PRESETS$preset)) && all(RX_PRESET_META$min_window >= 1))

    .section("E. Problems are reported in plain words")
    pr <- function(...) rx_problems(data.frame(..., stringsAsFactors = FALSE))
    .check("E1 min above max", any(grepl("minimum is above the maximum", pr(var = "temperature", min = 90, max = 40), fixed = TRUE)))
    .check("E2 a sector needs both ends", any(grepl("both ends", pr(var = "windDirection", min = 90, max = NA), fixed = TRUE)))
    .check("E3 out of the slider range", any(grepl("between", pr(var = "relativeHumidity", min = -5, max = 50), fixed = TRUE)))
    .check("E4 unknown variable", any(grepl("unknown variable", pr(var = "erc", min = 1, max = 2), fixed = TRUE)))
    .check("E5 a wrapping sector (min > max) is fine", !length(pr(var = "windDirection", min = 315, max = 45)))
    e <- tryCatch(rx_flag(h, data.frame(var = "temperature", min = 90, max = 40)), error = function(e) conditionMessage(e))
    .check("E6 rx_flag() refuses an invalid prescription", is.character(e) && grepl("prescription problems", e, fixed = TRUE))
  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })
  .test_finish()
})
