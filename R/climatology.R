# ==============================================================================
# R/climatology.R -- the one aggregation
#
# Every heatmap, table, pass rate and tooltip number comes from
# clim_summarise(). Never copy an aggregation chain into a plot, module or
# export (that was NFDRSChartBuilder's debt).
#
#   clim_summarise(flagged, value = "in_rx", by = c("month", "hour"), smooth_days = 0)
#
# value        one or more columns. Logical columns (in_rx, pass_*) give the
#              share TRUE; numeric columns give the mean.
# by           any grouping columns: month, week, doy, year, hour, hour_clock...
# smooth_days  > 0 only with "doy" in `by`: a centred window of
#              2 * smooth_days + 1 days that wraps from Dec into Jan.
#
# Returns one row per group per value (long format):
#   <by columns>, value, kind ("share"|"mean"), n_valid, n_missing, n_years, n_true, stat
# n_valid    hours with a value (the denominator)
# n_missing  hours in the group with no value (excluded: missing, screened,
#            or -- for in_rx -- any constrained variable missing)
# n_years    distinct years contributing at least one valid hour
# n_true     for a share: the hours TRUE (the count behind the share; for hour
#            totals). NA for a mean.
#
# Smoothing works on sums and counts, then divides, so it is exact (not an
# average of averages) and fast at ~190k rows.
# ==============================================================================

clim_summarise <- function(flagged, value = "in_rx", by = c("month", "hour"), smooth_days = 0) {
  miss <- setdiff(c(value, by, "year"), names(flagged))
  if (length(miss)) stop("clim_summarise(): no column(s) ", paste(miss, collapse = ", "), call. = FALSE)
  if (smooth_days > 0 && !"doy" %in% by)
    stop("clim_summarise(): smooth_days > 0 needs \"doy\" in `by` (it smooths across days of the year)", call. = FALSE)
  # the grouping key is the slow part (paste over ~190k rows): build it once for all values
  gkey <- if (smooth_days > 0) NULL else do.call(paste, c(unname(as.list(flagged[by])), sep = "\r"))
  out <- lapply(value, function(v) .clim_one(flagged, v, by, smooth_days, gkey))
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# Circular rolling sum down the rows of a matrix (rows = days of year).
.clim_window <- function(M, k) {
  if (k <= 0) return(M)
  n <- nrow(M)
  if (2 * k + 1 >= n) return(matrix(colSums(M), n, ncol(M), byrow = TRUE))
  P <- rbind(M[(n - k + 1):n, , drop = FALSE], M, M[1:k, , drop = FALSE])
  C <- apply(P, 2, cumsum)
  if (is.null(dim(C))) C <- matrix(C, nrow = nrow(P))
  C <- rbind(0, C)
  C[(2 * k + 2):(n + 2 * k + 1), , drop = FALSE] - C[1:n, , drop = FALSE]
}

.clim_one <- function(d, v, by, smooth_days, gkey = NULL) {
  x <- d[[v]]
  kind <- if (is.logical(x)) "share" else "mean"
  xv <- as.numeric(x); valid <- !is.na(xv); xv[!valid] <- 0
  n <- nrow(d)
  yrs <- sort(unique(d$year)); yi <- match(d$year, yrs); Y <- length(yrs)

  if (smooth_days > 0) {
    other <- setdiff(by, "doy")
    gkey <- if (length(other)) do.call(paste, c(unname(as.list(d[other])), sep = "\r")) else rep("all", n)
    G <- sort(unique(gkey)); gi <- match(gkey, G); NG <- length(G)
    N <- CLIM_DOY_N
    cell <- d$doy + N * (gi - 1L)                       # column-major: rows = doy, cols = group
    tab <- function(idx, w = NULL, nb) {
      if (is.null(w)) return(tabulate(idx, nbins = nb))
      s <- rowsum(w, idx); out <- numeric(nb); out[as.integer(rownames(s))] <- s[, 1]; out
    }
    S  <- matrix(tab(cell, xv, N * NG), N)
    NV <- matrix(tab(cell[valid], nb = N * NG), N)
    NA_ <- matrix(tab(cell, nb = N * NG), N)
    k <- smooth_days
    S <- .clim_window(S, k); NV <- .clim_window(NV, k); NA_ <- .clim_window(NA_, k)
    cell3 <- d$doy + N * ((gi - 1L) + NG * (yi - 1L))
    P <- matrix(tab(cell3[valid], nb = N * NG * Y), N) > 0
    P <- .clim_window(P * 1, k) > 0
    NY <- apply(array(P, c(N, NG, Y)), c(1, 2), sum)

    first <- match(G, gkey)
    grp <- d[rep(first, each = N), other, drop = FALSE]
    res <- data.frame(grp, doy = rep(seq_len(N), NG), check.names = FALSE)
    res <- res[, by, drop = FALSE]
    res$value <- v; res$kind <- kind
    res$n_valid <- as.vector(NV); res$n_missing <- as.vector(NA_ - NV)
    res$n_years <- as.vector(NY)
    res$n_true <- if (kind == "share") as.vector(S) else NA_real_
    res$stat <- ifelse(res$n_valid > 0, as.vector(S) / res$n_valid, NA_real_)
    res <- res[res$n_valid + res$n_missing > 0, , drop = FALSE]
  } else {
    if (is.null(gkey)) gkey <- do.call(paste, c(unname(as.list(d[by])), sep = "\r"))
    G <- unique(gkey); gi <- match(gkey, G); NG <- length(G)
    sums <- as.vector(rowsum(xv, gi, reorder = TRUE)[, 1])
    nv <- tabulate(gi[valid], nbins = NG); na <- tabulate(gi, nbins = NG)
    yk <- unique((yi[valid] - 1L) * NG + gi[valid])   # integer key per (year, group); unique() on a data frame is slow
    ny <- tabulate((yk - 1L) %% NG + 1L, nbins = NG)
    res <- d[match(seq_len(NG), gi), by, drop = FALSE]
    res$value <- v; res$kind <- kind
    res$n_valid <- nv; res$n_missing <- na - nv; res$n_years <- ny
    res$n_true <- if (kind == "share") sums else NA_real_
    res$stat <- ifelse(nv > 0, sums / nv, NA_real_)
  }
  ord <- do.call(order, unname(as.list(res[by])))
  res[ord, , drop = FALSE]
}

# ---- runs: the longest stretch of consecutive TRUE hours per group ----------------
#   clim_runs(flagged, value = "in_rx", by = "date", hour_col = "hour")
# For each group (e.g. each day), the longest run of consecutive hours where
# `value` is TRUE, and the hour it starts. A FALSE or NA hour ends a run, and
# runs don't cross group boundaries (a day's run stops at midnight).
# Returns: <by columns>, longest (hours, 0 if none), start_hour (NA if none).
# Used for the Past event daily table now, and the window-length climatology later.
clim_runs <- function(flagged, value = "in_rx", by = "date", hour_col = "hour") {
  miss <- setdiff(c(value, by, hour_col), names(flagged))
  if (length(miss)) stop("clim_runs(): no column(s) ", paste(miss, collapse = ", "), call. = FALSE)
  d <- flagged[do.call(order, unname(as.list(flagged[c(by, hour_col)]))), c(by, hour_col, value)]
  gkey <- do.call(paste, c(unname(as.list(d[by])), sep = "\r"))
  x <- !is.na(d[[value]]) & d[[value]]
  newg <- c(TRUE, gkey[-1] != gkey[-length(gkey)])
  rid <- cumsum(!x | newg)                   # a new run id at every break or group start
  G <- unique(gkey); gi <- match(gkey, G)
  res <- d[match(seq_along(G), gi), by, drop = FALSE]
  res$longest <- 0L; res$start_hour <- NA_integer_
  if (any(x)) {
    len <- tabulate(rid[x], nbins = max(rid))
    first <- match(seq_len(max(rid)), ifelse(x, rid, NA_integer_))   # first TRUE row of each run
    rr <- data.frame(g = gi[first[len > 0]], len = len[len > 0], start = d[[hour_col]][first[len > 0]])
    rr <- rr[order(rr$g, -rr$len, rr$start), ]
    rr <- rr[!duplicated(rr$g), ]
    res$longest[rr$g] <- as.integer(rr$len); res$start_hour[rr$g] <- as.integer(rr$start)
  }
  rownames(res) <- NULL
  res
}
