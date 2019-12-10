Zscs <- function(...) {
    pvals <- stats::na.omit(as.numeric(c(...)))
    rho_zscore <- NA
    pred_perf_pval <- NA
    if (length(pvals) > 2) {
        stouffer <- metap::sumz(pvals)
        rho_zscore <- stouffer$z
        pred_perf_pval <- stouffer$p
    }

    c(zscore = rho_zscore,
      pvalue = pred_perf_pval)
}

MapZscs <- function(...) {
    t(mapply(Zscs, ...))
}

#' Compute row-wise Z-scores and p-valuse using Stouffer's method.
#'
#' @param l A numeric data frame of p-values (or a list with equally long
#'   elements)
#' @return A data frame with two columns: rho_zscore and pred_perf_pval
#' @export
rowZscs <- function(l) {
    as.data.frame(do.call("MapZscs", l))
}
