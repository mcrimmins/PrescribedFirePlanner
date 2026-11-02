# ==============================================================================
# scripts/deploy.R -- check everything, then deploy to Posit Connect
#
# Replaces RStudio's Publish/Republish button (which deploys whatever is in
# the folder to whichever app you used last). Run from the project root, in
# the console, with PrescribedFirePlanner.Rproj open:
#
#     source("scripts/deploy.R")
#
# What it does, in order (any FAIL stops before deploying):
#   A  settings: app name chosen, rsconnect installed
#   G  git: on main, nothing uncommitted, nothing unpushed
#   C  Connect: your account is registered; first deploy or an update?
#   E  FEMS_USER / FEMS_KEY present locally (sent to Connect securely)
#   B  the bundle: every file the app needs is in it, nothing private is,
#      the snapshots match their checksums, size is sensible
#   P  packages installed (and from CRAN, so Connect can install them)
#   T  the network-free test suite passes
# Then it asks you to type the app name to confirm, deploys exactly the
# checked file list, and prints the URL and the git tag commands to run.
#
# For a dry run, just press Enter at the confirmation prompt: every check has
# run and nothing is deployed. (dry_run <- TRUE skips the prompt.) Nothing in
# this file needs editing between runs, so git stays clean. A transcript goes to tests/logs/deploy_<time>.txt (gitignored).
#
# The FEMS key is sent with rsconnect's envVars: values are taken from your
# session (.Renviron), sent over HTTPS, and are never written to the bundle,
# the deployment record or git. If you rotate the key, redeploy.
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)   # logging and check helpers only

  # ---- settings ------------------------------------------------------------------
  app_name  <- "rxwx"    # chosen once; never rename (it sets the URL)
  app_title <- "Prescribed Fire Weather Explorer"
  account   <- "crimmins"
  server    <- "viz.datascience.arizona.edu"
  dry_run   <- FALSE     # TRUE: checks only, no prompt (or just press Enter at the prompt)
  run_tests <- TRUE      # run the network-free tests first (about 2 minutes)
  send_env  <- c("FEMS_USER", "FEMS_KEY")   # character(0) to manage them in the Connect UI instead
  tests     <- c("sun", "hourly", "rx_flag", "climatology", "quality", "tooltips", "ui",
                 "fems_retry", "station_cache")
  git_exe   <- ""        # path to git.exe if R can't find git (e.g. "C:/Program Files/Git/cmd/git.exe")
  must_exclude <- c("^\\.Renviron$", "^CLAUDE\\.md$", "^tests/", "^docs/", "^scripts/", "^archive/",
                    "^\\.git/", "^\\.Rproj\\.user/", "^\\.Rhistory$", "^\\.RData$", "^_send/")
  pkgs <- c("shiny", "bslib", "leaflet", "plotly", "htmltools", "httr", "jsonlite", "dplyr", "tibble")

  .T <- .test_open("deploy")
  # R started from RStudio often doesn't have git on its PATH even when the
  # command prompt does, so look in the usual Windows install places too.
  find_git <- function() {
    cand <- c(git_exe, Sys.which("git"),
              file.path(c(Sys.getenv("ProgramFiles"), Sys.getenv("ProgramFiles(x86)"),
                          file.path(Sys.getenv("LOCALAPPDATA"), "Programs")),
                        "Git", "cmd", "git.exe"))
    cand <- cand[nzchar(cand) & file.exists(cand)]
    if (length(cand)) normalizePath(cand[1], winslash = "/") else ""
  }
  git_bin <- find_git()
  git <- function(...) suppressWarnings(tryCatch(system2(git_bin, c(...), stdout = TRUE, stderr = TRUE),
                                                 error = function(e) structure(character(0), status = 127)))
  git_ok <- function(out) is.null(attr(out, "status")) || attr(out, "status") == 0

  tryCatch({
    .env_header("Deploy to Posit Connect",
                c(paste0("App:      ", if (nzchar(app_name)) app_name else "<not chosen>", "  (", app_title, ")"),
                  paste0("Server:   ", server, "  account ", account),
                  paste0("Mode:     ", if (dry_run) "DRY RUN (no deploy)" else "deploy after checks")))

    # ---- A. settings ----------------------------------------------------------------
    .section("A. Settings")
    at_root <- file.exists("app.R") && file.exists("PrescribedFirePlanner.Rproj")
    .check("A1 working directory is the project root", at_root)
    if (!at_root) stop("open PrescribedFirePlanner.Rproj (or setwd() to it) and re-run")
    .check("A2 app name chosen (lowercase letters, digits, hyphens; 3-64 characters)",
           grepl("^[a-z0-9][a-z0-9-]{2,63}$", app_name),
           if (nzchar(app_name)) app_name else "set app_name at the top of scripts/deploy.R -- it becomes the URL; choose once")
    has_rsc <- requireNamespace("rsconnect", quietly = TRUE)
    .check("A3 rsconnect is installed", has_rsc,
           if (has_rsc) as.character(utils::packageVersion("rsconnect")) else "install.packages(\"rsconnect\")")
    if (!has_rsc || !grepl("^[a-z0-9][a-z0-9-]{2,63}$", app_name)) stop("fix the settings first")

    # ---- G. git --------------------------------------------------------------------------
    .section("G. Git")
    if (!nzchar(git_bin)) {
      .check("G1 git found from R", FALSE,
             "R can't find git. In a command prompt run `where git`, then set git_exe at the top of scripts/deploy.R to that path")
      stop("git not found; can't check what you're deploying")
    }
    br <- git("rev-parse", "--abbrev-ref", "HEAD")
    .check("G1 git works here", git_ok(br) && length(br) == 1,
           if (git_ok(br)) git_bin else paste(git_bin, ":", paste(br, collapse = " ")))
    .check("G2 on branch main", identical(br, "main"), paste("on", paste(br, collapse = " ")))
    st <- git("status", "--porcelain")
    .check("G3 nothing uncommitted (git status is clean)", git_ok(st) && !length(st),
           if (length(st)) paste0(length(st), " change(s): ", paste(utils::head(trimws(st), 6), collapse = "; ")) else "")
    up <- git("rev-list", "--count", "origin/main..HEAD")
    if (git_ok(up)) {
      .check("G4 nothing unpushed (what you deploy is on GitHub)", identical(up, "0"),
             if (identical(up, "0")) "" else paste(up, "commit(s) not pushed: git push first"))
    } else .info("G4 unpushed commits", "no origin/main to compare with")
    head_ <- git("log", "-1", "--format=%h%x20%s%x20%x28%ci%x29")   # %x20 space, %x28/%x29 parens: no shell quoting needed
    .info("G5 deploying commit", paste(head_, collapse = " "))

    # ---- C. Connect account and deployment records ---------------------------------------------
    .section("C. Connect")
    acc <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
    srv <- if (is.data.frame(acc)) acc$server[acc$name == account] else character(0)
    srv <- srv[grepl("viz.datascience", srv, fixed = TRUE) | srv == server]
    .check(paste0("C1 account '", account, "' is registered for ", server), length(srv) >= 1,
           if (length(srv)) srv[1] else paste0("register once: rsconnect::addServer(\"https://", server, "\", name = \"", server,
                                               "\"); rsconnect::connectUser(account = \"", account, "\", server = \"", server, "\")"))
    server_name <- if (length(srv)) srv[1] else server
    dep <- tryCatch(rsconnect::deployments("."), error = function(e) NULL)
    if (is.data.frame(dep) && nrow(dep)) {
      same <- dep[dep$name == app_name, ]
      other <- setdiff(unique(dep$name), app_name)
      .info("C2 deployment records in this folder", paste(paste0(dep$name, " -> ", dep$url), collapse = "; "))
      if (length(other)) .qc("C3 no records for a different app name", FALSE,
                             paste0("this folder was also deployed as: ", paste(other, collapse = ", "),
                                    ". Check that '", app_name, "' is the name you mean."))
      .info("C4 this deploy is", if (nrow(same)) paste("an UPDATE of", same$url[1]) else "a FIRST deploy of a new app")
    } else .info("C4 this deploy is", "a FIRST deploy (no deployment records in this folder)")

    # ---- E. FEMS credentials ---------------------------------------------------------------------
    .section("E. FEMS credentials for the server")
    if (length(send_env)) {
      have <- vapply(send_env, function(v) nzchar(Sys.getenv(v)), logical(1))
      .check("E1 FEMS_USER and FEMS_KEY are set in this session (they'll be sent to Connect)", all(have),
             if (all(have)) "values are sent securely and never written to the bundle or git"
             else paste("missing:", paste(send_env[!have], collapse = ", "), "-- edit .Renviron and restart R"))
    } else .info("E1 credentials", "not sent: set FEMS_USER and FEMS_KEY in the Connect UI (Vars tab), or live downloads return 401")

    # ---- B. the bundle -------------------------------------------------------------------------------
    .section("B. The bundle")
    files <- rsconnect::listDeploymentFiles(".")
    .info("B0 files in the bundle", length(files))
    mf <- if (file.exists("data/stations_bundled/manifest.csv"))
      utils::read.csv("data/stations_bundled/manifest.csv", colClasses = c(station_id = "character", md5 = "character"),
                      stringsAsFactors = FALSE) else data.frame(file = character(0), md5 = character(0))
    need <- c("app.R", file.path("R", list.files("R", pattern = "\\.R$")),
              "www/RxWxLogo_banner.png", "www/UAlogo_banner.png", "www/UA_CSAP_CLIMAS_logos_horiz.png",
              "data/station_metadata_FEMS3_042225.csv", "data/FEMS_3.0_RAWS_Master_Station_List_and_Metadata.csv",
              "data/stations_featured.csv", "data/stations_bundled/manifest.csv",
              file.path("data/stations_bundled", mf$file))
    miss <- setdiff(need, files)
    .check("B1 everything the app needs is in the bundle", !length(miss), if (length(miss)) paste("missing:", paste(miss, collapse = ", ")) else "")
    bad <- files[vapply(files, function(f) any(vapply(must_exclude, grepl, logical(1), x = f)), logical(1))]
    .check("B2 nothing private or dev-only is in the bundle (.Renviron, tests, docs, scripts, ...)", !length(bad),
           if (length(bad)) paste("would upload:", paste(utils::head(bad, 8), collapse = ", "), "-- add them to .rscignore") else "")
    ok_md5 <- vapply(seq_len(nrow(mf)), function(i) {
      p <- file.path("data/stations_bundled", mf$file[i]); file.exists(p) && identical(unname(tools::md5sum(p)), mf$md5[i])
    }, logical(1))
    .check("B3 every bundled snapshot matches its checksum", nrow(mf) > 0 && all(ok_md5),
           paste0(sum(ok_md5), " of ", nrow(mf), ": ", paste(mf$station_id, collapse = ", ")))
    fs <- utils::read.csv("data/stations_featured.csv", colClasses = c(station_id = "character"), stringsAsFactors = FALSE)
    .check("B4 the bundled snapshots are exactly the stations marked bundled",
           setequal(fs$station_id[as.logical(fs$bundled) %in% TRUE], mf$station_id))
    mb <- sum(file.size(files)) / 1e6
    .check("B5 bundle under 100 MB", mb < 100, sprintf("%.1f MB", mb))
    big <- files[order(-file.size(files))][1:min(5, length(files))]
    .info("B6 largest files", paste(sprintf("%s (%.1f MB)", big, file.size(big) / 1e6), collapse = ", "))

    # ---- P. packages -----------------------------------------------------------------------------------
    .section("P. Packages")
    inst <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
    .check("P1 the app's packages are installed", all(inst), if (all(inst)) "" else paste("install.packages(c(", paste0('"', pkgs[!inst], '"', collapse = ", "), "))"))
    src <- vapply(pkgs[inst], function(p) {
      d <- utils::packageDescription(p)
      if (!is.null(d$Repository)) d$Repository else if (!is.null(d$RemoteType)) d$RemoteType else "unknown"
    }, character(1))
    .info("P2 versions", paste(pkgs[inst], vapply(pkgs[inst], function(p) as.character(utils::packageVersion(p)), ""),
                               sep = " ", collapse = ", "))
    .qc("P3 installed from CRAN (Connect installs the same versions from CRAN)", all(src %in% c("CRAN", "RSPM")),
        paste(paste(names(src), src, sep = "=")[!src %in% c("CRAN", "RSPM")], collapse = ", "))

    # ---- T. tests ------------------------------------------------------------------------------------------
    .section("T. Tests (network-free suite; transcripts in tests/logs/)")
    if (!run_tests) {
      .info("T0 skipped", "run_tests is FALSE")
    } else for (t in tests) {
      f <- file.path("tests", paste0("test_", t, ".R"))
      if (!file.exists(f)) { .check(paste("T", t), FALSE, paste("missing", f)); next }
      t0 <- Sys.time(); res <- NULL
      utils::capture.output(res <- tryCatch(source(f)$value, error = function(e) FALSE))
      .check(paste0("T test_", t, ".R"), isTRUE(res), sprintf("%.0f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    }

    # ---- deploy ----------------------------------------------------------------------------------------------
    n_fail <- sum(.T$results$status == "FAIL")
    .section("Deploy")
    if (n_fail) stop(n_fail, " check(s) failed; not deploying. Fix them and run again.")
    ans <- ""
    if (!dry_run && interactive())
      ans <- readline(sprintf("All checks passed. Deploy '%s' to %s (%s)? Type the app name to deploy, or press Enter to stop (dry run): ",
                              app_name, server, if (exists("same") && nrow(same)) "update" else "new app"))
    if (!identical(trimws(ans), app_name)) {
      .info("D0 dry run", if (nzchar(trimws(ans))) "that wasn't the app name; nothing deployed" else "all checks passed; nothing deployed")
    } else {
      .say("  deploying ", length(files), " files ...")
      ok <- rsconnect::deployApp(appDir = ".", appFiles = files, appName = app_name, appTitle = app_title,
                                 account = account, server = server_name,
                                 envVars = if (length(send_env)) send_env else NULL,
                                 forceUpdate = TRUE, launch.browser = TRUE, lint = FALSE)
      .check("D1 rsconnect::deployApp() succeeded", isTRUE(ok))
      d2 <- tryCatch(rsconnect::deployments("."), error = function(e) NULL)
      url <- if (is.data.frame(d2)) d2$url[d2$name == app_name][1] else NA
      .info("D2 app URL", if (is.na(url)) "see the Connect dashboard" else url)
      if (isTRUE(ok)) {
        .say("")
        .say("Next, in the RStudio terminal (tag the commit you just deployed):")
        .say("    git tag -a v0.1.0 -m \"First Connect deploy\"   # pick the next version")
        .say("    git push origin v0.1.0")
        if (!length(send_env)) .say("Then set FEMS_USER and FEMS_KEY in the Connect UI (Vars tab).")
      }
    }
  }, error = function(e) {
    .T$abort <- paste("stopped:", conditionMessage(e))
    .say(""); .say("STOPPED: ", conditionMessage(e))
  })
  .test_finish()
})
