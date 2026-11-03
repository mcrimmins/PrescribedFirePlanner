# ==============================================================================
# R/constants.R -- every tunable number and path in the app, in one place
#
# Rule (CLAUDE.md): no magic years, windows or thresholds anywhere else. The
# download layer's own FEMS_* constants are the one exception; they live in
# R/fems_api.R and R/fems_download.R so tests can source that layer alone.
#
# Thresholds marked DRAFT were chosen by Claude and await review by Mike/PFTC.
# ==============================================================================

# ---- climatology -------------------------------------------------------------
RX_BASELINE_YEARS <- 2005:2025     # the climatology baseline (trimmed to each station's record)
CLIM_SMOOTH_DAYS  <- 7             # +/- days for day-of-year smoothing (a 15-day window)
CLIM_DOY_N        <- 365           # 365-day calendar: Feb 29 is merged into Feb 28

# ---- prescription variables --------------------------------------------------
# One row per variable a prescription can constrain. `kind` is "linear" (min
# and max) or "circular" (a clockwise sector from min to max that may wrap
# through north). slider_min/max/step drive the UI controls. NFDRS variables
# join later as more rows here (plus their columns in R/hourly.R).
RX_VARS <- data.frame(
  var        = c("temperature", "relativeHumidity", "windSpeed", "windDirection", "precipitation", "solarRadiation"),
  label      = c("Temperature", "Relative humidity", "20-ft wind speed", "Wind direction", "Precipitation", "Solar radiation"),
  units      = c("\u00b0F", "%", "mph", "\u00b0", "in/hr", "W/m\u00b2"),
  kind       = c("linear", "linear", "linear", "circular", "linear", "linear"),
  slider_min = c(-20, 0, 0, 0, 0, 0),
  slider_max = c(120, 100, 40, 360, 1, 1300),
  step       = c(1, 1, 1, 5, 0.01, 10),
  stringsAsFactors = FALSE)

# Starting prescription shown before presets exist (v0.1). DRAFT example
# values, not a recommendation; labelled as an example in the UI.
RX_EXAMPLE <- data.frame(
  var = c("temperature", "relativeHumidity", "windSpeed", "windDirection", "precipitation", "solarRadiation"),
  on  = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
  min = c(40, 20, 2, 180, 0, 0),
  max = c(85, 55, 15, 270, 0, 1300),
  stringsAsFactors = FALSE)
# Presets: DRAFT starting points for training, written by Claude for PFTC to
# review -- not recommendations. Choosing one fills the sidebar; every value
# stays editable. One row per preset x variable (on, min, max as in
# RX_EXAMPLE; NA = no limit on that side), plus RX_PRESET_META for the
# ignition period (clock time, start inclusive, end exclusive; NA = all day)
# and the minimum usable window. "example" is RX_EXAMPLE itself.
.rx_preset <- function(id, on, min, max)
  data.frame(preset = id, var = RX_EXAMPLE$var, on = on, min = min, max = max, stringsAsFactors = FALSE)
RX_PRESETS <- rbind(
  .rx_preset("example", RX_EXAMPLE$on, RX_EXAMPLE$min, RX_EXAMPLE$max),
  #                     temp   RH     wind   wdir   precip  solar
  .rx_preset("grass",   on  = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
                        min = c(40,    15,    4,   180,    0,     0),
                        max = c(85,    40,   15,   270,    0,  1300)),
  .rx_preset("pine",    on  = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
                        min = c(35,    30,    4,   180,    0,     0),
                        max = c(80,    55,   12,   270,    0,  1300)),
  .rx_preset("conifer", on  = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
                        min = c(35,    20,    2,   180,    0,     0),
                        max = c(80,    45,   12,   270,    0,  1300)),
  .rx_preset("piles",   on  = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
                        min = c(-20,   25,    0,   180,    0,     0),
                        max = c(60,   100,   10,   270,    1,  1300)))
RX_PRESET_META <- data.frame(stringsAsFactors = FALSE,
  preset     = c("example", "grass", "pine", "conifer", "piles"),
  label      = c("Example (all day)", "Grass / rangeland", "Southern pine understory",
                 "Western conifer understory", "Pile burning"),
  ign_start  = c(NA, 10, 9, 9, 8),
  ign_end    = c(NA, 17, 16, 17, 15),
  min_window = c(4, 4, 5, 6, 3),
  note       = c("The starting example: no ignition limit.",
                 "Fine fuels: drier and windier is still workable, ignition mid-morning to late afternoon.",
                 "Moister understory burns, moderate winds, a longer window.",
                 "Heavier fuels: cooler, moderate RH, a long window to finish a unit.",
                 "Winter piles: wet conditions and rain are fine, light winds."))

RX_DEBOUNCE_MS <- 400   # wait this long after the last edit before recomputing

# ---- hourly table --------------------------------------------------------------
# Value columns carried from the weather download into the hourly table.
HOURLY_VALUE_COLS <- c("temperature", "relativeHumidity", "windSpeed", "windDirection",
                       "gustSpeed", "gustDirection", "precipitation", "solarRadiation",
                       "vpd", "dewpoint", "hdw")

# ---- screening (hourly_qc), lenient -- DRAFT ------------------------------------
QC_STUCK_HOURS     <- 24     # same value for >= this many hours => stuck sensor
QC_CALM_HOURS      <- 24     # calm (0 mph) for MORE than this many hours => suspect anemometer
QC_SOLAR_DAY_MIN   <- 50     # W/m2; stuck-solar check only looks at values above this (daylight)
QC_LIMITS_PAD_DAYS <- 1      # days either side of a day past a physical limit also removed (failing sensors drift first)
QC_SOLAR_STUCK_HOURS <- 6    # identical daylight solar readings in a row (SAGUARO's longest real run: 3)
QC_SPIKE           <- c(temperature = 20, relativeHumidity = 40)   # one-hour spike size
QC_LIMITS <- data.frame(     # physical limits; values outside are set to missing
  var = c("temperature", "relativeHumidity", "windSpeed", "gustSpeed", "precipitation", "solarRadiation"),
  min = c(-40, 0, 0, 0, 0, 0),
  max = c(130, 100, 120, 200, 6, 1500),
  stringsAsFactors = FALSE)
# Variables checked for stuck values (exemptions are in hourly_qc()).
QC_STUCK_VARS <- c("temperature", "relativeHumidity", "windSpeed", "windDirection",
                   "gustSpeed", "solarRadiation")

# ---- data-quality report -- DRAFT ----------------------------------------------
QUALITY_STEP_H          <- 0.3   # h; smallest timing step worth reporting
QUALITY_STEP_SPREAD     <- 4     # ... and it must be >= this many times the year-to-year spread
QUALITY_STEP_MIN_SIDE   <- 3     # years needed on each side of a step
QUALITY_TIMING_VARS     <- c("temperature", "relativeHumidity", "windSpeed", "solarRadiation")
QUALITY_COVERAGE_MIN    <- 80    # % of hours with a value; a baseline year below this "affects results"

APP_VERSION <- "0.2.0"   # written into CSV exports; bump with each deploy tag

# ---- hour totals (R/totals.R) ---------------------------------------------------
# A year-month (or year) with fewer of its hours reporting than this is left
# out of the median year and the range: missing hours would read as fewer
# burnable hours. It still counts toward the total. DRAFT -- PFTC to review.
TOTALS_COVERAGE_MIN <- 80   # %
QUALITY_PRECIP_RATIO    <- c(0.4, 2.5)   # median annual total / metadata average, outside => units suspect
QUALITY_PRECIP_UNDERCATCH <- 0.7         # ratio below this (but above 0.4) => gauge likely misses snow
QUALITY_PRECIP_YEAR     <- c(0.5, 2.0)   # a year outside this fraction of the station's OWN median is noted (info)
QUALITY_CALM_SHARE      <- 20    # % of baseline hours calm; above this the anemometer/site is flagged

# ---- files ---------------------------------------------------------------------
STATION_META_FILE   <- file.path("data", "station_metadata_FEMS3_042225.csv")
STATION_MASTER_FILE <- file.path("data", "FEMS_3.0_RAWS_Master_Station_List_and_Metadata.csv")
STATIONS_FEATURED   <- file.path("data", "stations_featured.csv")
BUNDLED_DIR         <- file.path("data", "stations_bundled")
BUNDLED_MANIFEST    <- file.path(BUNDLED_DIR, "manifest.csv")
FEMS_RECORD_START   <- as.Date("2005-01-01")   # start of a live download

# ---- charts ----------------------------------------------------------------------
# Sequential ramps for "% of hours in prescription" and "% reporting", low ->
# high. The scale is always 0-100% so a colour means the same share for every
# prescription. CHART_RAMP picks one; each ramp has a highlight colour for the
# threshold outline that stands out against the ramp's high end.
CHART_RAMPS <- list(
  blues_v01 = list(colors = c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b"),
                   highlight = "#eb6834", label = "v0.1 blues (dataviz reference, steps 100-700)"),
  blues     = list(colors = c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b"),
                   highlight = "#eb6834", label = "Blues, white to navy (ColorBrewer)"),
  ylgnbu    = list(colors = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"),
                   highlight = "#eb6834", label = "Yellow-green-blue (ColorBrewer YlGnBu)"),
  viridis   = list(colors = c("#440154", "#482878", "#3e4989", "#31688e", "#26828e", "#1f9e89", "#35b779", "#6ece58", "#b5de2b", "#fde725"),
                   highlight = "#000000", label = "Viridis, dark purple to yellow"))
CHART_RAMP <- "blues"
CHART_SEQ <- CHART_RAMPS[[CHART_RAMP]]$colors
CHART_HIGHLIGHT <- CHART_RAMPS[[CHART_RAMP]]$highlight
CHART_CELL_GAP <- 2    # px between heatmap cells
CLIM_HIGHLIGHT_PCT <- 50   # default outline threshold: cells at or above this % of hours in prescription

# ---- Past event --------------------------------------------------------------------
EVENT_MAX_DAYS     <- 14    # longest stretch the Past event tab shows
EVENT_DEFAULT_DAYS <- 7
CHART_IN_RX  <- "#0b5e80"                  # an hour in prescription (the RxWx blue)
CHART_OUT    <- "#c3ccd3"                  # out of prescription
CHART_FAIL   <- CHART_HIGHLIGHT            # this limit failed at this hour
CHART_NIGHT  <- "rgba(25, 45, 70, 0.07)"   # sunset to sunrise shading
CHART_BAND   <- "rgba(11, 94, 128, 0.08)"  # the prescription range on a variable panel
CHART_OUTSIDE_IGN <- "rgba(255, 255, 255, 0.65)"   # wash over cells / hours outside the ignition period
CHART_IN_RX_OUT_IGN <- "#93b6c6"           # in prescription but outside the ignition period

# ---- Same dates --------------------------------------------------------------------
SAME_MAX_DAYS   <- 92                    # longest window of the year
SAME_DEFAULT    <- c("03-15", "04-15")   # month-day, until a Past event range is carried over
SAME_MIN_RUN_H  <- 4                     # a "usable day" has at least this many consecutive hours in prescription
