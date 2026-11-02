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
