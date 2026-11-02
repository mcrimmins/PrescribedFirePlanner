# ==============================================================================
# tests/test_fems_retry.R -- retry behaviour of fems_gql(), no network
#
# fems_gql() retries transient FEMS failures (HTTP 429/502/503/504 and
# connection errors) after waiting FEMS_RETRY_WAITS seconds, and raises a
# classed `fems_unavailable` error once every attempt has failed. This test
# swaps in a fake POST (.post) and a fake clock (.sleep), so it needs no
# network, no FEMS credentials and no real waiting.
#
# Run from the project root with PrescribedFirePlanner.Rproj open:
#     source("tests/test_fems_retry.R")
# Takes a second or two. Ends with VERDICT: PASS or VERDICT: FAIL.
# ==============================================================================

local({
  source("tests/helpers.R", local = TRUE)
  .T <- .test_open("test_fems_retry")

  # fems_credentials() needs FEMS_USER/FEMS_KEY to exist. Use dummies only if
  # they are unset, and put everything back afterwards.
  old_env <- Sys.getenv(c("FEMS_USER", "FEMS_KEY"), unset = NA)
  if (is.na(old_env[["FEMS_USER"]])) Sys.setenv(FEMS_USER = "test-user")
  if (is.na(old_env[["FEMS_KEY"]]))  Sys.setenv(FEMS_KEY  = "test-key-0000")

  # A minimal httr response: status code plus a body.
  .resp <- function(status, body) {
    structure(list(url = "https://example.invalid/graphql", status_code = as.integer(status),
                   headers = list(`content-type` = "application/json"),
                   content = charToRaw(body)),
              class = "response")
  }
  ok_body  <- '{"data":{"ping":{"_metadata":{"page":0,"per_page":1,"total_count":1,"page_count":1},"data":[{"x":1}]}}}'
  gql_err  <- '{"errors":[{"message":"Cannot query field \\"nope\\""}],"data":null}'
  html_502 <- "<html><head><title>502 Bad Gateway</title></head></html>"

  # Fake POST that plays a script: each element is a status code, or "boom"
  # for a connection error. Counts calls; records sleeps.
  .scripted <- function(script) {
    st <- new.env(); st$calls <- 0L; st$sleeps <- numeric(0)
    st$post <- function(...) {
      st$calls <- st$calls + 1L
      step <- script[[min(st$calls, length(script))]]
      if (identical(step, "boom")) stop("Timeout was reached: Operation timed out after 600000 milliseconds")
      if (step == 200) return(.resp(200, ok_body))
      if (step == 299) return(.resp(200, gql_err))      # 200 with GraphQL errors
      .resp(step, if (step >= 500) html_502 else '{"message":"denied"}')
    }
    st$sleep <- function(s) st$sleeps <- c(st$sleeps, s)
    st
  }
  # Run fems_gql() against a script; return the value or the condition.
  .try_gql <- function(st) {
    r <- .run("fems_gql", tryCatch(
      fems_gql("query { ping }", .post = st$post, .sleep = st$sleep),
      error = function(e) e))
    r$value
  }

  tryCatch({
    .env_header("fems_gql() retry behaviour (no network)")
    .section("A. Setup")
    at_root <- file.exists("R/fems_api.R") && file.exists("PrescribedFirePlanner.Rproj")
    .check("A1 working directory is the project root", at_root)
    if (!at_root) stop("not at project root")
    src <- .run("source R/fems_api.R", source("R/fems_api.R"))
    .check("A2 R/fems_api.R loads", src$ok)
    has <- all(vapply(c("fems_gql", "fems_gql_paged", "FEMS_RETRY_WAITS"), exists, logical(1), envir = globalenv()))
    .check("A3 fems_gql, fems_gql_paged and FEMS_RETRY_WAITS exist", has)
    if (!has) stop("retry code not found; is R/fems_api.R the new version?")
    .check("A4 retry waits are 2, 5, 15 s", identical(FEMS_RETRY_WAITS, c(2, 5, 15)),
           paste(FEMS_RETRY_WAITS, collapse = ", "))
    .check("A5 fems_gql() has .post and .sleep arguments",
           all(c(".post", ".sleep", "retry_waits") %in% names(formals(fems_gql))))

    .section("B. Transient failures are retried")
    st <- .scripted(list(502, 502, 200)); v <- .try_gql(st)
    .check("B1 502, 502, 200 -> data returned", !inherits(v, "error") && identical(v$ping$data$x, 1L))
    .check("B1b ... after 3 calls and waits of 2 then 5 s", st$calls == 3 && identical(st$sleeps, c(2, 5)),
           paste0(st$calls, " calls, waits ", paste(st$sleeps, collapse = ",")))

    st <- .scripted(list(503, 504, 429, 200)); v <- .try_gql(st)
    .check("B2 503, 504, 429, 200 -> data on the 4th (last) attempt",
           !inherits(v, "error") && st$calls == 4 && identical(st$sleeps, c(2, 5, 15)),
           paste0(st$calls, " calls, waits ", paste(st$sleeps, collapse = ",")))

    st <- .scripted(list("boom", 200)); v <- .try_gql(st)
    .check("B3 connection timeout, then 200 -> data after one 2 s wait",
           !inherits(v, "error") && st$calls == 2 && identical(st$sleeps, 2),
           paste0(st$calls, " calls, waits ", paste(st$sleeps, collapse = ",")))

    .section("C. Giving up cleanly")
    st <- .scripted(list(502)); v <- .try_gql(st)
    .check("C1 always 502 -> fems_unavailable error", inherits(v, "fems_unavailable"),
           paste("class:", paste(class(v), collapse = "/")))
    .check("C1b ... after 4 calls and waits of 2, 5, 15 s", st$calls == 4 && identical(st$sleeps, c(2, 5, 15)),
           paste0(st$calls, " calls, waits ", paste(st$sleeps, collapse = ",")))
    msg <- if (inherits(v, "condition")) conditionMessage(v) else ""
    .check("C1c message is written for users", grepl("FEMS isn't responding right now", msg, fixed = TRUE) &&
             !grepl("<html", msg, fixed = TRUE), msg)
    .check("C1d fems_unavailable is also an ordinary error (old tryCatch(error=) callers still work)",
           inherits(v, "error"))

    st <- .scripted(list("boom")); v <- .try_gql(st)
    .check("C2 repeated timeouts -> fems_unavailable", inherits(v, "fems_unavailable") && st$calls == 4)

    .section("D. Non-transient failures are not retried")
    st <- .scripted(list(401)); v <- .try_gql(st)
    .check("D1 401 -> credentials error, one call, no wait",
           inherits(v, "error") && !inherits(v, "fems_unavailable") && st$calls == 1 && !length(st$sleeps),
           substr(if (inherits(v, "condition")) conditionMessage(v) else "", 1, 70))
    st <- .scripted(list(403)); v <- .try_gql(st)
    .check("D2 403 -> no retry", inherits(v, "error") && st$calls == 1 && !length(st$sleeps))
    st <- .scripted(list(500)); v <- .try_gql(st)
    .check("D3 500 -> plain HTTP error, no retry", inherits(v, "error") && !inherits(v, "fems_unavailable") &&
             st$calls == 1)
    st <- .scripted(list(299)); v <- .try_gql(st)
    .check("D4 200 with GraphQL errors -> GraphQL error, no retry",
           inherits(v, "error") && grepl("GraphQL error", conditionMessage(v), fixed = TRUE) && st$calls == 1)

    .section("E. Paged fetch passes the fakes through")
    st <- .scripted(list(502, 200))
    r <- .run("fems_gql_paged", tryCatch(
      fems_gql_paged("query { ping }", variables = list(), root = "ping", per_page = 1,
                     verbose = FALSE, .post = st$post, .sleep = st$sleep),
      error = function(e) e))
    .check("E1 fems_gql_paged retries through a 502", is.data.frame(r$value) && nrow(r$value) == 1 &&
             st$calls == 2 && identical(st$sleeps, 2))
    st <- .scripted(list(502))
    r <- .run("fems_gql_paged", tryCatch(
      fems_gql_paged("query { ping }", variables = list(), root = "ping", per_page = 1,
                     verbose = FALSE, .post = st$post, .sleep = st$sleep),
      error = function(e) e))
    .check("E2 fems_gql_paged passes fems_unavailable up unchanged", inherits(r$value, "fems_unavailable"))

  }, error = function(e) {
    .T$abort <- paste("test aborted:", conditionMessage(e))
    .say(""); .say("ABORTED: ", conditionMessage(e))
  })

  # Restore the caller's environment variables.
  for (nm in c("FEMS_USER", "FEMS_KEY")) {
    if (is.na(old_env[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, stats::setNames(list(old_env[[nm]]), nm))
  }
  .test_finish()
})
