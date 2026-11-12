# ==============================================================================
# scripts/publish_static.R -- upload the offline (Shinylive) copy to S3
#
# Syncs static_site/ (built by scripts/build_static.R) to
#     s3://cales-climate-reports/climate/rxwx/
# Run from the project root, in the console, with PrescribedFirePlanner.Rproj open:
#     source("scripts/build_static.R")      # after any change to the app
#     source("scripts/publish_static.R")
#
# Checks first (any FAIL stops before uploading):
#   1  static_site/ exists and is newer than app.R, R/, data/ and www/
#      (so you never publish an out-of-date copy)
#   2  the AWS CLI is installed and your credentials work (aws sts get-caller-identity)
#   3  a dry run of the sync, summarised (uploads, deletes)
# Then it asks you to type "rxwx" to confirm (Enter alone = stop, nothing uploaded).
#
# The upload: aws s3 sync with --delete, limited to the rxwx/ folder (nothing
# else in the bucket is touched). Content types are set explicitly for .js,
# .mjs, .wasm and .json, because Windows can report wrong types for them and
# the browser then refuses to run webR. HTML and app.json get a short cache
# time (5 min) so updates show up quickly.
#
# Needs HTTPS in front of the bucket (e.g. CloudFront): the app uses a service
# worker, which browsers only allow over https. If CloudFront caches the site,
# set cf_distribution below and the script will invalidate /climate/rxwx/*.
# ==============================================================================

local({
  # ---- settings (edit once) -------------------------------------------------------
  bucket_path     <- "s3://cales-climate-reports/climate/rxwx"
  site_dir        <- "static_site"
  aws_profile     <- ""      # AWS CLI profile name, if not the default
  cf_distribution <- ""      # CloudFront distribution id, if one serves the bucket (else leave "")
  aws_exe         <- ""      # path to aws.exe if R can't find it

  stopifnot(file.exists("PrescribedFirePlanner.Rproj"))
  say <- function(...) cat(..., "\n", sep = "")
  fail <- function(...) stop(paste0(...), call. = FALSE)

  # ---- 1. the build is there and up to date ----------------------------------------------
  if (!dir.exists(site_dir) || !file.exists(file.path(site_dir, "index.html")))
    fail("no ", site_dir, "/ yet: run source(\"scripts/build_static.R\") first")
  built <- max(file.mtime(list.files(site_dir, recursive = TRUE, full.names = TRUE)))
  src <- c("app.R", list.files(c("R", "data", "www"), recursive = TRUE, full.names = TRUE))
  newer <- src[file.mtime(src) > built]
  if (length(newer))
    fail("the app changed after the last build (", paste(utils::head(newer, 5), collapse = ", "),
         "): run source(\"scripts/build_static.R\") again")
  say("[PASS] 1 ", site_dir, "/ built ", format(built, "%Y-%m-%d %H:%M"), ", newer than the app's files")

  # ---- 2. AWS CLI and credentials ------------------------------------------------------
  if (!nzchar(aws_exe)) {
    aws_exe <- Sys.which("aws")
    if (!nzchar(aws_exe)) {
      cand <- c("C:/Program Files/Amazon/AWSCLIV2/aws.exe", file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Amazon/AWSCLIV2/aws.exe"))
      cand <- cand[file.exists(cand)]
      if (length(cand)) aws_exe <- cand[1]
    }
  }
  if (!nzchar(aws_exe)) fail("AWS CLI not found: install it (https://aws.amazon.com/cli/) or set aws_exe")
  prof <- if (nzchar(aws_profile)) c("--profile", aws_profile) else character(0)
  aws <- function(args, echo = FALSE) {
    q <- shQuote(c(args, prof), type = if (.Platform$OS.type == "windows") "cmd" else "sh")   # keep * patterns literal
    out <- suppressWarnings(system2(aws_exe, q, stdout = TRUE, stderr = TRUE))
    st <- attr(out, "status"); if (is.null(st)) st <- 0
    if (echo) cat(out, sep = "\n")
    list(ok = st == 0, out = out)
  }
  id <- aws(c("sts", "get-caller-identity", "--query", "Arn", "--output", "text"))
  if (!id$ok) fail("AWS credentials don't work (aws sts get-caller-identity):\n", paste(id$out, collapse = "\n"))
  say("[PASS] 2 AWS CLI ", aws_exe, " as ", id$out[length(id$out)])

  # ---- the sync passes: everything else, then each type with an explicit Content-Type -----
  typed <- list(
    list(pat = "*.js",   type = "text/javascript"),
    list(pat = "*.mjs",  type = "text/javascript"),
    list(pat = "*.wasm", type = "application/wasm"),
    list(pat = "*.json", type = "application/json"))
  short_cache <- "max-age=300"
  passes <- c(
    list(c("--exclude", "*.js", "--exclude", "*.mjs", "--exclude", "*.wasm", "--exclude", "*.json", "--exclude", "*.html")),
    list(c("--exclude", "*", "--include", "*.html", "--content-type", "text/html", "--cache-control", short_cache)),
    lapply(typed, function(t) c("--exclude", "*", "--include", t$pat, "--content-type", t$type,
                                if (t$pat == "*.json") c("--cache-control", short_cache))))
  sync <- function(extra, dry) aws(c("s3", "sync", site_dir, bucket_path, "--delete", "--only-show-errors"[!dry],
                                     if (dry) "--dryrun", extra))

  # ---- 3. dry run ----------------------------------------------------------------------------
  lines <- unlist(lapply(passes, function(p) { r <- sync(p, TRUE); if (!r$ok) fail("dry run failed:\n", paste(r$out, collapse = "\n")); r$out }))
  n_up <- sum(grepl("^\\(dryrun\\) upload", lines)); n_del <- sum(grepl("^\\(dryrun\\) delete", lines))
  say("[PASS] 3 dry run: ", n_up, " file(s) to upload, ", n_del, " to delete, in ", bucket_path, "/")
  if (n_del) say("       deletes (first 5): ", paste(utils::head(sub("^\\(dryrun\\) delete: ", "", lines[grepl("delete", lines)]), 5), collapse = "; "))
  if (!n_up && !n_del) { say("Nothing to do: the bucket already matches ", site_dir, "/."); return(invisible()) }

  ans <- readline(paste0("Type rxwx to upload to ", bucket_path, "/ (Enter to stop): "))
  if (!identical(trimws(ans), "rxwx")) { say("Stopped; nothing uploaded."); return(invisible()) }

  for (p in passes) { r <- sync(p, FALSE); if (!r$ok) fail("upload failed:\n", paste(r$out, collapse = "\n")) }
  say("Uploaded to ", bucket_path, "/")
  if (nzchar(cf_distribution)) {
    r <- aws(c("cloudfront", "create-invalidation", "--distribution-id", cf_distribution, "--paths", "/climate/rxwx/*"))
    say(if (r$ok) "CloudFront invalidation started for /climate/rxwx/*" else paste("CloudFront invalidation failed:", paste(r$out, collapse = " ")))
  }
  say("Open the site's https address for climate/rxwx/ and check it loads (first load can take 20-60 s).")
  invisible()
})
