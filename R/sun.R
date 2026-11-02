# ==============================================================================
# R/sun.R -- sun times in local STANDARD time (LST), and standard UTC offsets
#
# Used by the Data quality checks (solar timing) and, later, the sun lines on
# charts. NOAA-style approximations: good to a few minutes, which is plenty
# for hour-of-day climatology. All times are decimal hours LST.
# ==============================================================================

# Standard (winter) UTC offset of an IANA zone in hours, e.g. -7 for
# America/Phoenix and America/Denver, -5 for America/New_York.
# The smaller of the January and July offsets is the standard one in either
# hemisphere's sense of "no daylight saving".
sun_std_offset_h <- function(tz, year = 2024) {
  off <- function(d) {
    z <- format(as.POSIXct(paste(d, "12:00:00"), tz = tz), "%z")
    s <- ifelse(substr(z, 1, 1) == "-", -1, 1)
    s * (as.numeric(substr(z, 2, 3)) + as.numeric(substr(z, 4, 5)) / 60)
  }
  min(off(sprintf("%d-01-15", year)), off(sprintf("%d-07-15", year)))
}

# Does the zone observe daylight saving time?
sun_has_dst <- function(tz, year = 2024) {
  z <- function(d) format(as.POSIXct(paste(d, "12:00:00"), tz = tz), "%z")
  z(sprintf("%d-01-15", year)) != z(sprintf("%d-07-15", year))
}

# Equation of time, minutes.
.sun_eot <- function(doy) {
  B <- 2 * pi * (doy - 81) / 364
  9.87 * sin(2 * B) - 7.53 * cos(B) - 1.5 * sin(B)
}

# Solar declination, radians.
.sun_decl <- function(doy) 23.44 * sin(2 * pi * (284 + doy) / 365) * pi / 180

# Solar noon (LST hours) for a longitude (deg E, negative west).
sun_noon_lst <- function(lon, std_offset_h, doy) {
  12 - .sun_eot(doy) / 60 - (lon - 15 * std_offset_h) / 15
}

# Sun times for each day of year. Returns a data frame of decimal LST hours:
# doy, noon, sunrise, sunset, civil_dawn, civil_dusk. NA where the sun never
# reaches the altitude (polar day/night; not an issue for FEMS stations).
sun_times <- function(lat, lon, std_offset_h, doy = 1:365) {
  phi  <- lat * pi / 180
  decl <- .sun_decl(doy)
  noon <- sun_noon_lst(lon, std_offset_h, doy)
  half <- function(alt_deg) {
    x <- (sin(alt_deg * pi / 180) - sin(phi) * sin(decl)) / (cos(phi) * cos(decl))
    ifelse(abs(x) > 1, NA_real_, acos(x) * 180 / pi / 15)
  }
  h_rise  <- half(-0.833)   # refraction + solar radius
  h_civil <- half(-6)
  data.frame(doy = doy, noon = noon,
             sunrise = noon - h_rise, sunset = noon + h_rise,
             civil_dawn = noon - h_civil, civil_dusk = noon + h_civil)
}

# Format decimal hours as "HH:MM" (for tooltips and reports).
sun_fmt_hm <- function(h) {
  m <- round(h * 60)
  ifelse(is.na(m), NA_character_, sprintf("%02d:%02d", (m %/% 60) %% 24, m %% 60))
}
