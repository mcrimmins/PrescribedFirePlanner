# ==============================================================================
# scripts/build_static.R -- build the offline (Shinylive) copy of RxWx
#
# Builds a static website that runs the app in the visitor's browser (webR):
# no R server, no FEMS calls, bundled stations only, data up to the date they
# were bundled (RXWX_STATIC in R/constants.R). A backup for when Connect is
# down or busy; copy the output folder to any web server.
#
# Run from the project root, in the console, with PrescribedFirePlanner.Rproj open:
#     source("scripts/build_static.R")
# Needs internet (shinylive downloads its assets once) and the packages
# shinylive and httpuv. Output: static_site/ (gitignored). Preview with
#     httpuv::runStaticServer("static_site")
# then work through the checklist printed at the end.
#
# What ships: app.R, R/*.R except the FEMS layer (fems_api.R, fems_download.R),
# data/ (station metadata and the bundled snapshots) and www/. Never .Renviron,
# tests, scripts or docs. The script stops if any shipped file contains your
# FEMS key.
# ==============================================================================

local({
  out_dir <- "static_site"

  at_root <- file.exists("app.R") && dir.exists("R") && file.exists("R/constants.R")   # also on GitHub Actions
  if (!at_root) stop("run from the project root (open PrescribedFirePlanner.Rproj)")
  for (p in c("shinylive", "httpuv"))
    if (!requireNamespace(p, quietly = TRUE)) stop("install.packages(\"", p, "\") first")
  cat("shinylive", as.character(utils::packageVersion("shinylive")), "\n")

  # ---- stage only what the static app needs ------------------------------------------
  stage <- file.path(tempdir(), "rxwx_static_app")
  unlink(stage, recursive = TRUE); dir.create(stage)
  r_files <- setdiff(list.files("R", pattern = "\\.R$"), c("fems_api.R", "fems_download.R"))
  dir.create(file.path(stage, "R"))
  file.copy("app.R", stage)
  file.copy(file.path("R", r_files), file.path(stage, "R"))
  file.copy("data", stage, recursive = TRUE)
  file.copy("www", stage, recursive = TRUE)
  shipped <- list.files(stage, recursive = TRUE, all.files = TRUE)
  cat(length(shipped), "files staged,", round(sum(file.size(file.path(stage, shipped))) / 2^20, 1), "MB\n")

  # ---- safety: nothing private ships ---------------------------------------------------
  if (any(grepl("(^|/)\\.Renviron$|(^|/)rsconnect/", shipped))) stop("a private file was staged")
  if (any(grepl("^R/fems_", shipped))) stop("the FEMS layer was staged")
  # the key is the secret (the user name can legitimately appear, e.g. a contact email on the About page)
  key <- Sys.getenv("FEMS_KEY")
  text <- shipped[grepl("\\.(R|csv|txt|md|html|js|css)$", shipped)]
  if (nzchar(key)) for (f in text) {
    x <- paste(readLines(file.path(stage, f), warn = FALSE), collapse = "\n")
    if (grepl(key, x, fixed = TRUE)) stop("your FEMS key appears in ", f, "; not building")
  }
  cat("Safety checks passed: no .Renviron, no FEMS layer, FEMS key in none of the shipped files\n")

  # ---- export ---------------------------------------------------------------------------
  unlink(out_dir, recursive = TRUE)
  t0 <- Sys.time()
  shinylive::export(stage, out_dir)
  site <- list.files(out_dir, recursive = TRUE)
  cat(sprintf("\nBuilt %s/ in %.0f s: %d files, %.0f MB\n", out_dir, as.numeric(difftime(Sys.time(), t0, units = "secs")),
              length(site), sum(file.size(file.path(out_dir, site))) / 2^20))

  cat("\nPreview:  httpuv::runStaticServer(\"", out_dir, "\")   (then open the address it prints)\n", sep = "")
  cat("\nChecklist (note the times; paste the results back):
  1  first load: seconds until the app appears (browser cache cleared / private window)
  2  second load (normal window, refresh): seconds until the app appears
  3  sidebar says 'Offline copy'; the map and station list show only the bundled stations
  4  load a featured station: seconds until the Climatology heatmap shows
  5  Climatology: day-of-year chart, per-limit maps, daily range, totals table, both CSV downloads
  6  change the prescription (drag RH max): seconds until the heatmap redraws
  7  Season tracker: offline note, this year's bars through the bundle date, no Refresh button
  8  Past event (7 days) and Same dates: charts, tables, CSV downloads
  9  Data quality and About tabs
  10 any errors: open the browser's developer console (F12) and copy any red lines\n")
  invisible(out_dir)
})
