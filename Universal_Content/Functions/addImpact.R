################################################################################
####                                                                        ####
####                  Functions to Simulate COVID impact                    ####
####                                                                        ####
################################################################################

roundInt <- function(x, to=50) to*(x%/%to + as.logical(x%%to))

Z <- function(data_table, var.to.standardize, reference.year = NULL, rm.na = TRUE) {
  YEAR <- NULL
  x <- data_table[, get(var.to.standardize)]
  if (!is.null(reference.year)){
    y <- data_table[YEAR==reference.year, get(var.to.standardize)]
  } else y <- x
  (x - mean(y, na.rm = rm.na)) / sd(y, na.rm = rm.na)
}

simulatePriorScore <- function(
  data_table,
  score.var = "SCALE_SCORE",
  prior.score.var = "SCALE_SCORE_PRIOR_2YEAR",
  year = "2021",
  digits = 0L) {
  YEAR <- NULL
  if (data_table[1, YEAR] != year) return(data_table[YEAR == year, get(prior.score.var)])

  y <- data_table[YEAR == year, get(score.var)]
  x <- data_table[YEAR == year, get(prior.score.var)]
  if (all(is.na(x))) {
    yz <- (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE)
    hetero.stdv <-0.75*sqrt(abs(yz)); m <- round(median(hetero.stdv),2)
    hetero.stdv[which(hetero.stdv < m)] <- runif(length(which(hetero.stdv < m)), max(c(m-0.25, 0.35)), m-0.01)
    x <- round((rnorm(length(y), mean = yz, sd = hetero.stdv)*sd(y, na.rm = TRUE) + mean(y, na.rm = TRUE)), digits)
    x[x < min(y)] <- min(y)
    x[x > max(y)] <- max(y) # cor(x, y);plot(x, y)
  }
  x
}


addImpact <- function(
  data_table,
  score.var = "SCALE_SCORE",
  prior.score.var = "PRIOR_SCORE_for_IMPACT",
  quantile.var = "PRIOR_SCORE_DECILE",
  impact.var = "IMPACT_PERCENTILE") {

    YEAR <- CONTENT_AREA <- GRADE <- NULL

    y <- data_table[, get(score.var)]
    x <- data_table[, get(prior.score.var)]
    impact.pctl <- data_table[, get(impact.var)]
    taus <- sort(c(unique(impact.pctl), 50))/100
    tmp.rq <- rq((y-x) ~ bs(x), tau=taus)[["coefficients"]]
    x.values <- sort(unique(x))
    predicted.values <- cbind(1, bs(x.values)) %*% tmp.rq
    # predicted.values <- t(apply((cbind(1, bs(x.values)) %*% tmp.rq), 1, sort)) # Isotonize predicted values - don't need if just running taus
    row.names(predicted.values) <- x.values
    colnames(predicted.values) <- taus*100

    tmp.mtrx <- cbind(predicted.values[as.character(x),], impact.pctl)
    tmp.gain <- t(apply(tmp.mtrx, 1, function(f) f[c("50", f["impact.pctl"])]))

    impact.gain <- rnorm(nrow(tmp.gain), mean=tmp.gain[,1]-tmp.gain[,2], sd=sqrt(abs(tmp.gain[,1]-tmp.gain[,2])))
    return(y - impact.gain)
}
