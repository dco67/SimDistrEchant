# Fonctions pour DistrEchant - TabPanel.R

SimData <- function(par1 = 50,
                    par2 = 8,
                    n = 25,
                    ng = 1,
                    dist = "Normale",
                    ns = 1000) {
  smpl <- matrix(NA, nrow = n, ncol = ns)
  for (i in 1:ns) {
    smpl[, i] <-
      if (dist == "Normale") {
        rnorm(n, par1, par2)
      } else if (dist == "Uniforme") {
        runif(n, par1, par2)
      } else if (dist == "Exponentielle") {
        rexp(n, par1)
      } else {
        rchisq(n, par1)
      }
  }
  out <- as.data.frame(smpl)
  return(out)
}

Q1 <- function(x) {
  q1 <- quantile(x, 0.25)
  return(q1)
}

Q3 <- function(x) {
  q3 <- quantile(x, 0.75)
  return(q3)
}


rbern <- function(n, N = 1, p = 0.5){
  #  runif(n) > (1 - p)
  #  sample(1:0, n, prob = c(p, 1-p), replace = TRUE)
  
  rbinom(n, 1, p)
}

dbern <- function(x, prob, log = FALSE){
  dbinom(x,
         size=1,
         prob=prob,
         log=log)
}


gensim3 <- function(param = "Moyenne",
                    par1 = 0,
                    par2 = 1,
                    n = 25,
                    k=10000){
  if(param != "Proportion"){
    dat <- rnorm(n, par1, par2)}
      else {
        dat <- rbern(n, p = par1)
      }
  
  resamples <- lapply(1:k, function(i) sample(dat, n, replace = TRUE))
  out <- list(resamples, dat, par1, par2)
}

gensim2 <- function(k = 10,
                    param = "Moyenne",
                    par1 = 0,
                    par2 = 1,
                    n = 25){
  out <- matrix(NA, nrow = n, ncol = k)
  for (i in 1:k){
    if(param != "Proportion"){
      out[, i] <- rnorm(n, par1, par2)
    } else {
      out[, i] <- rbern(n, p = par1)
    }
  }
  out <- list(out, par1, par2)
  return(out)
  
}



gensim <- function(param = "Moyenne",
                   k = 10,
                   conf.level = 95,
                   distr = "Normale",
                   par1 = 0,
                   par2 = 1,
                   n = 25,
                   sigmaknown = "connu"){
  out <- matrix(NA, nrow = k, ncol = 3)
  cmnd <- 
    if(param == "Proportion"){"rbern("} else
      if(distr == "Normale" & param != "Proportion"){"rnorm("} else 
        if(distr == "Uniforme" & param != "Proportion"){"runif("} else 
          if(distr == "Asymétrique" & param != "Proportion"){"rchisq("}

  cmnd <- paste(cmnd, paste(n, par1,par2, sep = ", ", collapse = ", "), ")")
  for (i in 1:k){
    x <- eval(parse(text = cmnd))
    
    if(param == "Moyenne"){
      if(sigmaknown == "connu"){
        test <- z.conf.int(x, 
                       mu = par1,
                       sigma = par2,
                       conf.level / 100)
        interval <- test
      } 
      else {
        test <- t.test(x, 
                       conf.level=conf.level / 100)
        interval <- test$conf.int
      }
    } 
    if(param == "Proportion"){
        test <- PropCI(x, conf.level / 100)
    }
    if(param == "Variance"){
        test <- VarCI(x, conf.level / 100)
      }

    out[i, 1:3] <- test
  }
  return(out)
  
}



ci_meanz <- function(x, mu = 0, sigma = 1, conf.lev = 0.95){
  vc <- c(-1, 1) * qnorm(conf.lev + ((1 - conf.lev) / 2), 0, 1)
  cint <- vc * sigma / sqrt(length(x)) + mean(x)
  out <- list(parameter = "population mean", interval = cint, 
              estimate = mean(x), probs = conf.lev)
  class(out) <- "cint"
  out
}

PropCI <- function(x, conf.lev = 0.95 ){
  vc <- c(-1, 1) * qnorm(conf.lev + ((1 - conf.lev) / 2), 0, 1)
  p <- mean(x)
  sep <- sqrt((p * (1 - p)) / length(x))
  ic <- vc * sep + p
  return(c(mean(x), ic))
}

VarCI <- function(x, conf.lev = 0.95){
 # DescTools::VarCI(x, method = "classic", conf.level = 0.95) 
  test <- confintr::ci_var(x, probs = c((1-conf.lev)/2, (1 + conf.lev)/2))
  ic <- test$cint
  return(c(var(x), ic))
}


SkewKurtosis <- function(x) {
  w <- length(x)
  m1 <- mean(x)
  m2 <- sum((x-m1)^2)
  m3 <- sum((x-m1)^3)
  m4 <- sum((x-m1)^4)
  s1 <- sd(x)
  skew <- w*m3/(w-1)/(w-2)/s1^3
  sdskew <- sqrt( 6*w*(w-1) / ((w-2)*(w+1)*(w+3)) )
  kurtosis <- (w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4)
  sdkurtosis <- sqrt( 4*(w^2-1) * sdskew^2 / ((w-3)*(w+5)) )
  mat <- matrix(c(skew,kurtosis, sdskew,sdkurtosis), 2,
        dimnames=list(c("skew","kurtosis"), c("estimate","se")))
  return(mat)
}

check_probs <- function(probs) {
  stopifnot(length(probs) == 2L,
            is.numeric(probs),
            probs >= 0, probs <= 1,
            probs[1] < probs[2],
            probs[1] + 1 - probs[2] > 0)
  TRUE
}

check_bca <- function(boot_type, n, R) {
  if (boot_type == "bca" && n > R) {
    stop("Number of bootstrap replications must be larger than the sample size.")
  }
  TRUE
}

se_CV <- function(z, na.rm = TRUE, ...) {
  if (na.rm) {
    z <- z[!is.na(z)]
  }
  n <- length(z)
  if (n <= 3L) {
    stop("Not enough non-missing observations to calculate standard error. Need at least four.")
  }
  z/sqrt(2 * n) * sqrt(1+2 * (z/100)^2)
}

ci_CV <- function(x, probs = c(0.025, 0.975), type = c("chi-squared", "bootstrap"),
                   boot_type = c("bca", "perc", "stud", "norm", "basic"),
                   R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)
  
  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- sd(x) / abs(mean(x))
  n <- length(x)
  
  # Calculate CI
  if (type == "chi-squared") {
    cint <- estimate * (n - 1) / qchisq(1 - probs, df = n - 1)
  } else if (type == "bootstrap") {
    check_bca(boot_type, n, R)
    set_seed(seed)
    S <- boot(x, statistic = function(x, id) c(sqrt(var(x[id]))/mean(x[id])), se_CV(x[id])^2, R = R, ...)
    cint <- ci_boot(S, boot_type, probs)
  }
  cint <- check_output(cint, probs, c(0, Inf))
  
  # Organize output
  out <- list(parameter = "population variance",
              interval = cint, estimate = estimate,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}

# From https://github.com/mayer79/confintr/blob/master/R/internal_helpers.R

# Helper functions

# Input check
check_probs <- function(probs) {
  stopifnot(length(probs) == 2L,
            is.numeric(probs),
            probs >= 0, probs <= 1,
            probs[1] < probs[2],
            probs[1] + 1 - probs[2] > 0)
  TRUE
}

# Output Check
check_output <- function(ci, probs, parameter_range = c(-Inf, Inf)) {
  stopifnot(length(ci) == 2L,
            length(probs) == 2L,
            length(parameter_range) == 2L)
  ci <- as.numeric(ci)
  w <- which(probs %in% 0:1)
  if (length(w)) {
    ci[w] <- parameter_range[w]
  }
  out <- pmin(pmax(ci, parameter_range[1]), parameter_range[2])
  stopifnot(out[1] <= out[2])
  out
}

# Map boot type to a nice name and also to the output of boot.ci
map_boot_type <- function(ty) {
  out <- c(norm = "normal", basic = "basic", stud = "student",
           perc = "percent", bca = "bca")[ty]
  if (anyNA(out)) {
    stop("Wrong boot_type.")
  }
  as.character(out)
}

# Set seed
set_seed <- function(s) {
  if (!is.null(s)) {
    set.seed(s)
  }
}

# Formats probabilities
format_p <- function(z, digits = max(2L, getOption("digits"))) {
  paste0(format(100 * z, digits = digits), "%")
}

# Pastes together some info on bootstrap
boot_info <- function(type, boot_type, R) {
  if (type == "bootstrap") {
    sprintf("based on %s bootstrap replications and the %s method", R, map_boot_type(boot_type))
  }
}

# Checks if CI is symmetric
is_symmetric <- function(probs) {
  isTRUE(all.equal(probs[1], 1 - probs[2]))
}

# Checks if CI is one-sided
is_onesided <- function(probs) {
  any(probs %in% 0:1)
}

# Calculate prefix from "probs" used in types info
props2text <- function(p) {
  if (is_onesided(p)) {
    out <- "One-sided"
  } else if (!is_symmetric(p)) {
    out <- "Asymmetric two-sided"
  } else {
    out <- "Two-sided"
  }
  out
}

# Turns probs into alternative hypothesis string
probs2alternative <- function(p) {
  if (is_symmetric(p)) {
    return("two.sided")
  } else if (is_onesided(p)) {
    if (p[1] > 0) {
      return("greater")
    } else {
      return("less")
    }
  }
  asymmetric_stop()
}

# Consistent error message
asymmetric_stop <- function() {
  stop("Asymmetric two-sided case not supported in this case.")
  FALSE
}

# Title case
title_case1 <- function(s) {
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}

# Map F test statistic to R-squared
f_to_r2 <- function(f, df1, df2) {
  f / (f + df2 / df1)
}

# Map R-squared to F test statistic
#r2_to_f <- function(r2, df1, df2) {
#  r2 / (1 - r2) * df2 / df1
#}

# Map non-centrality parameter of the F distribution to the R-squared
ncp_to_r2 <- function(ncp, df1, df2) {
  ncp / (ncp + df1 + df2 + 1)
}

# Map F test statistic to non-centrality parameter
f_to_ncp <- function(f, df1, df2) {
  df1 * f * (df1 + df2 + 1) / df2
}

# Map chi-squared statistic to non-centrality parameter
chi2_to_ncp <- function(stat, df) {
  pmax(0, stat - df)
}

# Function to efficiently calculate the mean difference statistic in boot
boot_two_means <- function(X, id, se = FALSE, var.equal = FALSE) {
  X <- X[id, ]
  x <- X[X[["g"]] == 1, "v"]
  y <- X[X[["g"]] == 2, "v"]
  c(mean(x) - mean(y), if (se) se_mean_diff(x, y, var.equal = var.equal)^2)
}

# Function to efficiently calculate the median difference statistic in boot
boot_two_stats <- function(X, id, FUN = mean, ...) {
  X <- X[id, ]
  x <- X[X[["g"]] == 1, "v"]
  y <- X[X[["g"]] == 2, "v"]
  FUN(x, ...) - FUN(y, ...)
}

# Error if R < n for bca bootstrap
check_bca <- function(boot_type, n, R) {
  if (boot_type == "bca" && n > R) {
    stop("Number of bootstrap replications must be larger than the sample size.")
  }
  TRUE
}


#' Plot a Normal Distribution with Shading
#'
#' Draw a normal distribution curve and optional shading of a particular region.
#'
#' @param mean The mean of the normal distribution to plot (mu)
#' @param sd The standard deviation of the normal distribution to plot (sigma)
#' @param shadeValues Either a single number or a vector of two numbers which
#' identify the boundary or boundaries of the shaded region
#' @param direction A character string (in quotes) indicating the direction from
#' shadeValues to shade under the normal curve. Must be one of "\code{less}",
#' "\code{greater}", "\code{beyond}", or "\code{between}". See Details below.
#' @param col.shade The color of the shaded region
#' @param ... Other graphical parameters passed to \link[base]{plot} (e.g.,
#' \code{xlim}, \code{main}, etc.). Generally, these will not control the
#' shading, only the normal distribution curve.
#'
#' @return A plot of a normal distribution curve with optional shading.
#' @export
#'
#' @details The \code{direction} argument is used to control the region under
#' the normal distribution curve which is shaded. If \code{shadeValues} is a
#' single number, \code{direction} must be either "\code{less}" (in which case
#' the shaded region will be to the *left* of \code{shadeValues}) or
#' "\code{greater}" (the shaded region will be to the *right* of
#' \code{shadeValues}).
#'
#' If `shadeValues` is a *vector*, then `direction` must be either "`between`"
#' (the region between the two numbers in `shadeValues` will be shaded) or
#' "`beyond`" (the region below the smaller and above the larger of the
#' `shadeValues` will be shaded).
#'
#' @seealso \link{plot_t}
#'
#' @examples
#' # Probability of a value being between 3 and 3.5 in a N(3.39, 0.55) distribution
#' plot_norm(mean = 3.39, sd = 0.55,
#'           shadeValues = c(3, 3.5),
#'           direction = "between",
#'           col.shade = "forestgreen")
#'
#' # Probability of a value being greater than 1.96 in a N(0, 1) distributio
#' plot_norm(shadeValues = 1.96,
#'           direction = "greater",
#'           col.shade = "peachpuff")

plot_norm <- function(mean = 0, sd = 1, shadeValues = NULL,
                      direction = c("less", "greater", "beyond", "between"),
                      col.shade = "cornflowerblue",
                      ...) {
  
  checks <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(shadeValues, null.ok = TRUE, max.len = 2, add = checks)
  checkmate::assert_number(mean, na.ok = FALSE, finite = TRUE, add = checks)
  checkmate::assert_number(sd, na.ok = FALSE, finite = TRUE, add = checks)
  checkmate::reportAssertions(checks)
  
  direction <- match.arg(direction)
  
  dots <- list(...)
  if (any(names(dots) == "xlim")) {
    xlim <- dots$xlim
    dots$xlim <- NULL
  } else xlim <- c(mean - 3*sd, mean + 3 * sd)
  
  if (any(names(dots) == "xlab")) {
    xlab <- dots$xlab
    dots$xlab <- NULL
  } else xlab <- ""
  
  if (any(names(dots) == "main")) {
    main <- dots$main
    dots$main <- NULL
  } else main <- paste0("N(", mean, ", ", sd, ") Distribution")
  
  xRange <- seq(mean - 3 * sd, mean + 3 * sd, length = 300)
  height <- dnorm(xRange, mean = mean, sd = sd)
  
  do.call(plot, c(list(height ~ xRange, type = "l", axes = F, ylab = "",
                       xlab = xlab, main = main, xlim = xlim, frame.plot = F, ...),
                  dots))
  axis(1, at = seq(mean - 3 * sd, mean + 3 * sd, sd),
       labels = prettyNum(seq(mean - 3 * sd, mean + 3 * sd, sd)),
       pos = 0)
  
  if (length(shadeValues) == 2) {
    shadeValues <- sort(shadeValues)
    if (!any(c("beyond", "between") == direction))
      stop(paste("When you provide two shadeValues for shading the plot,",
                 "you must also specify direction as 'between' or 'beyond'.",
                 "Fix this and try again."))
    else if (direction == "between") {
      xShade <- seq(shadeValues[1], shadeValues[2], length = 100)
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade, xShade[100]),
              c(0, shadeHeight, 0), col = col.shade)
    }
    else if (direction == "beyond") {
      xShade <- c(seq(min(xRange), shadeValues[1], length = 100),
                  seq(shadeValues[2], max(xRange), length = 100))
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade[1:100], shadeValues[1]),
              c(0, shadeHeight[1:100], 0), col = col.shade)
      polygon(c(shadeValues[2], xShade[101:200], xShade[200]),
              c(0, shadeHeight[101:200], 0), col = col.shade)
    }
    
    text(x = shadeValues[1], y = 0, xpd = TRUE,
         labels = prettyNum(formatC(shadeValues[1], digits = 3)),
         pos = 1, font = 2, offset = 0.6, col = col.shade)
    text(x = shadeValues[2], y = 0, xpd = TRUE,
         labels = prettyNum(formatC(shadeValues[2], digits = 3)),
         pos = 1, font = 2, offset = 0.6, col = col.shade)
    
  } else if (length(shadeValues == 1)) {
    if (!any(c("less", "greater") == direction))
      stop(paste("When you provide one shadeValue for shading the plot,",
                 "you must also specify direction as 'less' or 'greater'.",
                 "Fix this and try again."))
    if (direction == "less") {
      xShade <- seq(mean - 3 * sd, shadeValues, length = 100)
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade, xShade[100]), c(0, shadeHeight, 0),
              col = col.shade)
    } else if (direction == "greater") {
      xShade <- seq(shadeValues, mean + 3 * sd, length = 100)
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade, xShade[100]), c(0, shadeHeight, 0),
              col = col.shade)
    }
    text(x = shadeValues, y = 0, xpd = TRUE,
         labels = prettyNum(formatC(shadeValues, digits = 3)), pos = 1,
         font = 2, offset = 0.6, col = col.shade)
  }
  
}



#' Plot a t Distribution with Shading
#'
#' Draw a t distribution curve and optional shading of a particular region.
#'
#' @param df The degrees of freedom of the t distribution to plot
#' @param shadeValues Either a single number or a vector of two numbers which
#' identify the boundary or boundaries of the shaded region
#' @param direction A character string (in quotes) indicating the direction from
#' shadeValues to shade under the normal curve. Must be one of "\code{less}",
#' "\code{greater}", "\code{beyond}", or "\code{between}". See Details below.
#' @param col.shade The color of the shaded region
#' @param ... Other graphical parameters passed to \link[base]{plot} (e.g.,
#' \code{xlim}, \code{main}, etc.). Generally, these will not control the
#' shading, only the normal distribution curve.
#'
#' @return A plot of a normal distribution curve with optional shading.
#' @export
#'
#' @details The \code{direction} argument is used to control the region under
#' the normal distribution curve which is shaded. If \code{shadeValues} is a
#' single number, \code{direction} must be either "\code{less}" (in which case
#' the shaded region will be to the *left* of \code{shadeValues}) or
#' "\code{greater}" (the shaded region will be to the *right* of
#' \code{shadeValues}).
#'
#' If `shadeValues` is a *vector*, then `direction` must be either "`between`"
#' (the region between the two numbers in `shadeValues` will be shaded) or
#' "`beyond`" (the region below the smaller and above the larger of the
#' `shadeValues` will be shaded).
#'
#' @seealso \link{plot_norm}
#' @examples
#' # Shade the region below 1.4 on the t(4) distribution.
#' plot_t(df = 4, shadeValues = 1.4, direction = "less")
#'
#' # Shade the region between -2 and 0.5 on the t(13) distribution.
#' plot_t(df = 13, shadeValues = c(-2, 0.5), direction = "between")

plot_t <- function(df, shadeValues = NULL,
                   direction = c("less", "greater", "beyond", "between"),
                   col.shade = "cornflowerblue",
                   ...) {
  
  checks <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(shadeValues, null.ok = TRUE, max.len = 2, add = checks)
  checkmate::assert_number(df, na.ok = FALSE, finite = TRUE, add = checks)
  checkmate::reportAssertions(checks)
  
  direction <- match.arg(direction)
  
  lowerLimSV <- sign(min(shadeValues)) * ceiling(abs(min(shadeValues)))
  upperLimSV <- sign(max(shadeValues)) * ceiling(abs(max(shadeValues)))
  
  dots <- list(...)
  if (any(names(dots) == "xlim")) {
    xlim <- dots$xlim
    dots$xlim <- NULL
  } else xlim <- c(min(c(-3, lowerLimSV)), max(c(3, upperLimSV)))
  
  if (any(names(dots) == "xlab")) {
    xlab <- dots$xlab
    dots$xlab <- NULL
  } else xlab <- ""
  
  if (any(names(dots) == "main")) {
    main <- dots$main
    dots$main <- NULL
  } else main <- paste0("t(", df, ") Distribution")
  
  xRange <- seq(xlim[1], xlim[2], length = 300)
  height <- dt(xRange, df = df)
  
  do.call(plot, c(list(height ~ xRange, type = "l", axes = F, ylab = "",
                       xlab = xlab, main = main, xlim = xlim, frame.plot = F),
                  dots))
  axis(1, at = seq(xlim[1], xlim[2], 1),
       labels = prettyNum(seq(xlim[1], xlim[2], 1)),
       pos = 0)
  
  if (length(shadeValues) == 2) {
    shadeValues <- sort(shadeValues)
    if (!any(c("beyond", "between") == direction))
      stop(paste("When you provide two shadeValues for shading the plot,",
                 "you must also specify direction as 'between' or 'beyond'.",
                 "Fix this and try again."))
    else if (direction == "between") {
      xShade <- seq(shadeValues[1], shadeValues[2], length = 100)
      shadeHeight <- dt(xShade, df = df)
      polygon(c(xShade[1], xShade, xShade[100]),
              c(0, shadeHeight, 0), col = col.shade)
    }
    else if (direction == "beyond") {
      xShade <- c(seq(min(xRange), shadeValues[1], length = 100),
                  seq(shadeValues[2], max(xRange), length = 100))
      shadeHeight <- dt(xShade, df = df)
      polygon(c(xShade[1], xShade[1:100], shadeValues[1]),
              c(0, shadeHeight[1:100], 0), col = col.shade)
      polygon(c(shadeValues[2], xShade[101:200], xShade[200]),
              c(0, shadeHeight[101:200], 0), col = col.shade)
    }
    
    text(x = shadeValues[1], y = 0, xpd = TRUE,
         labels = prettyNum(formatC(shadeValues[1], digits = 3)),
         pos = 1, font = 2, offset = 0.6, col = col.shade)
    text(x = shadeValues[2], y = 0, xpd = TRUE,
         labels = prettyNum(formatC(shadeValues[2], digits = 3)),
         pos = 1, font = 2, offset = 0.6, col = col.shade)
    
  } else if (length(shadeValues == 1)) {
    if (!any(c("less", "greater") == direction))
      stop(paste("When you provide one shadeValue for shading the plot,",
                 "you must also specify direction as 'less' or 'greater'.",
                 "Fix this and try again."))
    if (direction == "less") {
      xShade <- seq(xlim[1], shadeValues, length = 100)
      shadeHeight <- dt(xShade, df = df)
      polygon(c(xShade[1], xShade, xShade[100]), c(0, shadeHeight, 0),
              col = col.shade)
    } else if (direction == "greater") {
      xShade <- seq(shadeValues, xlim[2], length = 100)
      shadeHeight <- dt(xShade, df = df)
      polygon(c(xShade[1], xShade, xShade[100]), c(0, shadeHeight, 0),
              col = col.shade)
    }
    text(x = shadeValues, y = 0, xpd = TRUE,
         labels = prettyNum(formatC(shadeValues, digits = 3)), pos = 1,
         font = 2, offset = 0.6, col = col.shade)
  }
  
}



plot_chi2 <- function(df = 10, alpha = 0.05, tail = "right", vline = NA, col = col, col.alpha = 0.5){
  curve(dchisq(x, df), from = 0, to = qchisq(0.999, df),
        main = paste("Distribution du \u03C7 \u00B2 (dl = ", df, ")"),
        ylab = 'Densité',
        xlab = "\u03C7 \u00B2",
        lwd = 2,
        xaxs = "i",
        yaxs = "i")
  
  #find upper and lower values for middle 95% of distribution
  if(tail == "two"){
    lower95 <- qchisq(alpha / 2, df)
    upper95 <- qchisq(1 - alpha / 2, df)
    x_lower95 <- seq(0, lower95)
    x_upper95 <- seq(upper95, qchisq(0.999, df))
    p_lower95 <- dchisq(x_lower95, df = df)
    p_upper95 <- dchisq(x_upper95, df = df)
    
    polygon(c(x_lower95, rev(x_lower95)), c(p_lower95, rep(0, length(p_lower95))),
            col = adjustcolor(col, alpha= col.alpha), border = NA)
    polygon(c(x_upper95, rev(x_upper95)), c(p_upper95, rep(0, length(p_upper95))),
            col = adjustcolor(col, alpha= col.alpha), border = NA)} else
              
              if(tail == "right"){
                upper95 <- qchisq(1 - alpha, df)
                x_upper95 <- seq(upper95, qchisq(0.999, df))
                p_upper95 <- dchisq(x_upper95, df = df)
                polygon(c(x_upper95, rev(x_upper95)), c(p_upper95, rep(0, length(p_upper95))),
                        col = adjustcolor(col, alpha= col.alpha), border = NA)} else {
                          
                          lower95 <- qchisq(alpha, df)
                          x_lower95 <- seq(0, lower95)
                          p_lower95 <- dchisq(x_lower95, df = df)
                          polygon(c(x_lower95, rev(x_lower95)), c(p_lower95, rep(0, length(p_lower95))),
                                  col = adjustcolor(col, alpha= col.alpha), border = NA)}
  
  if(!is.na(vline)){
    segments(vline, 0, vline, dchisq(vline, df),
             lwd = 2,
             lty = 2,
             col = "red")
    segments(df, 0, df, dchisq(vline, df),
             lwd = 2,
             lty = 1,
             col = "red")} 
}