#' Auxiliary Function (not accessible to users) to create all bootstrap statistics used to perform the unit root tests.
#' @inheritParams boot_ur
#' @param boot_sqt_test Logical indicator whether or not to perform the Bootstrap Quantile Test (\verb{TRUE}) or not (\verb{FALSE}).
#' @param boot_ur_test Logical indicator whether or not to perform the individual ADF Tests (\verb{TRUE}) or not (\verb{FALSE}).
#' @param level Desired significance level of the unit root test.
#' @param bootstrap String for bootstrap method to be used. Options are
#' \describe{
#' \item{\verb{"MBB"}}{Moving blocks bootstrap;}
#' \item{\verb{"BWB"}}{Block wild bootstrap;}
#' \item{\verb{"DWB"}}{Dependent wild bootstrap;}
#' \item{\verb{"AWB"}}{Autoregressive wild bootstrap;}
#' \item{\verb{"SB"}}{Sieve bootstrap;}
#' \item{\verb{"SWB"}}{Sieve wild bootstrap.}
#' }
#' @param block_length Desired 'block length' in the bootstrap. For the MBB, BWB and DWB bootstrap, this is a genuine block length. For the AWB bootstrap, the block length is transformed into an autoregressive parameter via the formula \eqn{0.01^(1/block_length)}; this can be overwritten by setting \verb{ar_AWB} directly. If NULL, sets the block length as a function of the time series length T, via the rule \eqn{block_length = 1.75 T^(1/3)}.
#' @param ar_AWB Autoregressive parameter used in the AWB bootstrap method (\verb{bootstrap = "AWB"}). Can be used to set the parameter directly rather than via the default link to the block length.
#' @param union Logical indicator whether or not to use bootstrap union tests (\verb{TRUE}) or not (\verb{FALSE}).
#' @param min_lag Minimum lag length in the augmented Dickey-Fuller regression.
#' @param max_lag Maximum lag length in the augmented Dickey-Fuller regression.
#' @param criterion String for information criterion used to select the lag length in the augmented Dickey-Fuller regression. Options are: \verb{"AIC"}, \verb{"BIC"}, \verb{"MAIC"}, \verb{"MBIC}.
#' @param deterministics Numeric vector indicating the deterministic specification. Only relevant if \code{union = FALSE}. Options are (combinations of): \code{0} - no deterministics; \code{1} - intercept only; \code{2} - intercept and trend. If \code{union = FALSE} and \code{deterministics = NULL}, an intercept is added.
#' @param detrend String vector indicating the type of detrending to be performed. Only relevant if \verb{union = FALSE}. Options are: \verb{"OLS"} and/or \verb{"QD"} (typically also called GLS). If NULL, set to \verb{"OLS"}.
#' @param criterion_scale Logical indicator whether or not to use the rescaled information criteria (\verb{TRUE}) or not (\verb{FALSE}).
#' @param steps Numeric vector of quantiles to be tested. Default is to test each unit sequentially.
#' @param h_rs Bandwidth used in rescaled information criteria.
#' @seealso \code{\link{boot_ur}}, \code{\link{boot_sqt}}, \code{\link{boot_fdr}}
#' @keywords internal
do_tests_and_bootstrap <- function(data, boot_sqt_test, boot_ur_test, level, bootstrap,
                                   B, block_length, ar_AWB, union, min_lag, max_lag, criterion,
                                   deterministics, detrend, criterion_scale, steps, h_rs,
                                   show_progress, do_parallel, cores, data_name){


  data <- as.matrix(data)

  # Check correctness arguments and perform initial calculations and transformations
  inputs <- check_inputs(data = data, boot_sqt_test = boot_sqt_test, boot_ur_test = boot_ur_test,
                         level = level, bootstrap = bootstrap, B = B, block_length = block_length,
                         ar_AWB = ar_AWB, union = union, min_lag = min_lag, max_lag = max_lag,
                         criterion = criterion, deterministics = deterministics,
                         detrend = detrend, steps = steps, do_parallel = do_parallel,
                         cores = cores, data_name = data_name)

  boot <- inputs$boot
  l <- inputs$l
  s_DWB <- inputs$s_DWB
  ar_AWB <- inputs$ar_AWB
  dc <- inputs$dc
  dc_boot <- inputs$dc_boot
  detr <- inputs$detr
  detr_int <- inputs$detr_int
  ic <- inputs$ic
  max_lag <- inputs$p_max
  p_vec <- inputs$p_vec
  range_nonmiss <- inputs$range_nonmiss
  joint <- inputs$joint

  list_inputs <- inputs
  if (!(bootstrap %in% c("MBB", "BWB", "DWB"))) {
    list_inputs$l <- NULL
  }
  if (bootstrap != "AWB") {
    list_inputs$ar_AWB <- NULL
  }
  if (!union) {
    list_inputs$union_quantile <- NULL
    dc_vector <- c("no deterministics", "intercept", "intercept and trend")
    list_inputs$deterministics <- dc_vector[dc + 1]
    detr_name_vector <- c("ADF", "ADF-QD")
    detr_vector <- c("OLS", "QD")
    list_inputs$name <- detr_name_vector[detr_int]
    list_inputs$detrend <- detr_vector[detr_int]
  } else {
    list_inputs$union_quantile <- level
    list_inputs$deterministics <- NULL
    list_inputs$name <- "Union"
    list_inputs$detrend <- NULL
  }
  if (min_lag == max_lag) {
    list_inputs$criterion <- NULL
    list_inputs$criterion_scale <- NULL
  } else {
    list_inputs$criterion <- criterion
    list_inputs$criterion_scale <- criterion_scale
  }

  # Dimensions
  n <- nrow(data)
  N <- ncol(data)

  # ADF tests
  panel_est <- adf_panel_bootstrap_dgp_cpp(y = data, pmin = min_lag, pmax = max_lag, ic = ic,
                                           dc = dc_boot, QD = FALSE, trim = FALSE,
                                           ic_scale = criterion_scale, h_rs = h_rs,
                                           range = range_nonmiss)
  u_boot <- panel_est$b_res
  u_boot[is.nan(u_boot)] <- NA
  res <- panel_est$res
  ar_est <- panel_est$par[-1, , drop = FALSE]
  t_star_mat <- bootstrap_cpp(B = B, boot = boot, u = u_boot, e = res, l = l, s = s_DWB,
                          ar = ar_AWB, ar_est = ar_est, y0 = matrix(0, ncol = N),
                          pmin = min_lag, pmax = max_lag, ic = ic, dc = dc, detr = detr_int,
                          ic_scale = criterion_scale, h_rs = h_rs, range = range_nonmiss,
                          joint = joint, show_progress = show_progress,
                          do_parallel = do_parallel)

  D <- length(dc) * length(detr_int)
  t_star <- array(t_star_mat, dim = c(B, D * N))

  tests <- adf_tests_panel_cpp(data, pmin = min_lag, pmax = max_lag, ic = ic,
                               dc = dc, detr = detr_int, ic_scale = criterion_scale,
                               h_rs = h_rs, range = range_nonmiss)
  tests_ind <- tests$tests # Individual test statistics
  par_est_ind <- tests$par # Parameter estimates
  lags_ind <- tests$lags   # Selected lag lengths

  pvals <- matrix(iADF_cpp(matrix(tests_ind, nrow = 1),
                           matrix(t_star, nrow = B)), nrow = N, ncol = D, byrow = TRUE)
  if (union) {
    scaling <- scaling_factors_cpp(t_star, D, level)
    if (N > 1) {
      test_stats_star <- union_tests_cpp(t_star, D, scaling)
      test_stats <- union_tests_cpp(array(tests_ind, dim = c(1, D * N)), D,
                                    scaling)
    } else {
      test_stats_star <- union_test_cpp(t_star, scaling)
      test_stats <- union_test_cpp(array(tests_ind, dim = c(1, D)), scaling)
    }
  } else {
    pvals <- matrix(iADF_cpp(matrix(tests_ind, nrow = 1),
                             matrix(t_star, nrow = B)), nrow = N, ncol = 1, byrow = TRUE)
    test_stats_star <- NULL
    test_stats <- NULL
  }
  out <- list("y" = data, "p_vec" = p_vec, "t_star" = t_star, "test_stats_star" = test_stats_star,
              "indiv_test_stats" = tests_ind, "indiv_par_est" = par_est_ind,
              "indiv_pval" = pvals, "indiv_lags_stats" = lags_ind, "test_stats" = test_stats,
              "level" = level, "dc" = dc, "detr" = detr, "inputs" = list_inputs)

  return(out)
}

#' Auxiliary Function (not accessible to users) to check if all arguments put in by the user are correct, and to perform some preliminary calculations.
#' @inheritParams boot_ur
#' @param boot_sqt_test Logical indicator whether or not to perform the Bootstrap Quantile Test (\verb{TRUE}) or not (\verb{FALSE}).
#' @param boot_ur_test Logical indicator whether or not to perform the individual ADF Tests (\verb{TRUE}) or not (\verb{FALSE}).
#' @param level Desired significance level of the unit root test.
#' @param bootstrap String for bootstrap method to be used. Options are
#' \describe{
#' \item{\verb{"MBB"}}{Moving blocks bootstrap;}
#' \item{\verb{"BWB"}}{Block wild bootstrap;}
#' \item{\verb{"DWB"}}{Dependent wild bootstrap;}
#' \item{\verb{"AWB"}}{Autoregressive wild bootstrap;}
#' \item{\verb{"SB"}}{Sieve bootstrap;}
#' \item{\verb{"SWB"}}{Sieve wild bootstrap.}
#' }
#' @param B Number of bootstrap replications. Default is 1999.
#' @param block_length Desired 'block length' in the bootstrap. For the MBB, BWB and DWB bootstrap, this is a genuine block length. For the AWB bootstrap, the block length is transformed into an autoregressive parameter via the formula \eqn{0.01^(1/block_length)}; this can be overwritten by setting \verb{ar_AWB} directly. If NULL, sets the block length as a function of the time series length T, via the rule \eqn{block_length = 1.75 T^(1/3)}.
#' @param ar_AWB Autoregressive parameter used in the AWB bootstrap method (\verb{bootstrap = "AWB"}). Can be used to set the parameter directly rather than via the default link to the block length.
#' @param union Logical indicator whether or not to use bootstrap union tests (\verb{TRUE}) or not (\verb{FALSE}).
#' @param min_lag Minimum lag length in the augmented Dickey-Fuller regression.
#' @param max_lag Maximum lag length in the augmented Dickey-Fuller regression.
#' @param criterion String for information criterion used to select the lag length in the augmented Dickey-Fuller regression. Options are: \verb{"AIC"}, \verb{"BIC"}, \verb{"MAIC"}, \verb{"MBIC}.
#' @param deterministics Numeric vector indicating the deterministic specification. Only relevant if \code{union = FALSE}. Options are (combinations of): \code{0} - no deterministics; \code{1} - intercept only; \code{2} - intercept and trend. If \code{union = FALSE} and \code{deterministics = NULL}, an intercept is added.
#' @param detrend String vector indicating the type of detrending to be performed. Only relevant if \verb{union = FALSE}. Options are: \verb{"OLS"} and/or \verb{"QD"} (typically also called GLS). If NULL, set to \verb{"OLS"}.
#' @param steps Numeric vector of quantiles to be tested. Default is to test each unit sequentially.
#' @seealso \code{\link{boot_ur}}, \code{\link{boot_sqt}}, \code{\link{boot_fdr}}
#' @keywords internal
check_inputs <- function(data, boot_sqt_test, boot_ur_test, level, bootstrap, B, block_length,
                         ar_AWB, union, min_lag, max_lag, criterion, deterministics,
                         detrend, steps, do_parallel, cores, data_name){

  # Dimensions
  n <- nrow(data)
  N <- ncol(data)

  # Check if valid data_name is given
  if (!is.character(data_name) & !is.null(data_name)) {
    stop("Invalid argument data_name: not of type character.")
  }

  # Check if sufficient bootstrap replications are done
  if (level * (B + 1) < 1) {
    stop("Bootstrap iterations B too low to perform test at desired significance level.")
  }
  # Set up parallel computing
  if (do_parallel) {
    if (is.null(cores)) {
      cores <- parallelly::availableCores(omit = 2)
    } else if ((cores != round(cores)) | (cores < 1)) {
      stop("Invalid value for argument cores")
    }
    RcppParallel::setThreadOptions(numThreads = cores)
  }

  # Check for missing values or unbalanced panels (MBB, SB)
  check_missing <- check_missing_insample_values(data)
  if (any(check_missing)) {
    stop("Missing values detected inside sample.")
  } else {
    joint <- TRUE
    check_nonmiss <- find_nonmissing_subsample(data)
    range_nonmiss <- check_nonmiss$range - 1
    all_range_equal <- check_nonmiss$all_equal
    if (!all_range_equal) {
      if (bootstrap %in% c("MBB", "SB")) {
        if (!boot_ur_test) {
          stop("Resampling-based bootstraps MBB and SB cannot handle unbalanced series.")
        } else if (N > 1) {
          joint <- FALSE
          warning("Missing values cause resampling bootstrap to be executed for each time series individually.")
        }
      }
    }
  }

  # Checks on inputs
  if (!(bootstrap %in% c("MBB", "BWB", "DWB", "AWB", "SB", "SWB")) | length(bootstrap) > 1) {
    stop("The argument bootstrap should be equal to either MBB, BWB, DWB, AWB, SB or SWB")
  } else if (bootstrap %in% c("SB", "SWB") & !boot_ur_test) {
    warning("SB and SWB bootstrap only recommended for boot_ur; see help for details.")
  }
  boot <- 1 * (bootstrap == "MBB") + 2*(bootstrap == "BWB") + 3 * (bootstrap == "DWB") +
    4 * (bootstrap == "AWB") + 5 * (bootstrap == "SB") + 6 * (bootstrap == "SWB")

  if (any(!is.element(criterion, c("AIC", "BIC", "MAIC", "MBIC"))) | length(criterion) > 1) {
    stop("The argument ic should be equal to either AIC, BIC, MAIC, MBIC)")
  }
  ic <- 1*(criterion=="AIC") + 2*(criterion=="BIC") + 3*(criterion=="MAIC") + 4*(criterion=="MBIC")

  # Bootstrap Union Tests: Settings
  if (union) {
    if (!is.null(deterministics)) {
      warning("Deterministic specification in argument deterministics is ignored, as union test is applied.")
    }
    if (!is.null(detrend)) {
      warning("Detrending method in argument detrend is ignored, as union test is applied.")
    }
    dc <- c(1,2)
    dc_boot <- 2
    detr_int <- 1:2
  } else {
    if (is.null(deterministics)) {
      # stop("No deterministic specification set.
      #      Set deterministics to 0, 1 and/or 2, or set union to TRUE for union test.")
      stop("No deterministic specification set.
           Set deterministics to the strings none, intercept or trend, or set union to TRUE for union test.")
    } else if (any(!is.element(deterministics, c("none", "intercept", "trend"))) | length(deterministics) > 1){
      stop("The argument deterministics should be equal to either none, intercept, trend:
           (none: no deterministics, intercept: intercept only, trend: intercept and trend)")
    }
    dc <- 0*(deterministics=="none") + 1*(deterministics=="intercept") +
      2*(deterministics=="trend")
    dc_boot <- dc
    if (is.null(detrend)) {
      warning("No detrending specification set. Using OLS detrending.")
      detrend <- "OLS"
    } else if(any(!is.element(detrend, c("OLS", "QD")))| length(detrend) > 1) {
      stop("The argument detrend should be equal to either OLS, QD")
    }
    detr_int <- 1*(detrend=="OLS") + 2*(detrend=="QD")
  }

  if (is.null(block_length)) {
    block_length <- round(1.75 * nrow(data)^(1/3))
  }

  if (is.null(ar_AWB)) {
    ar_AWB <- 0.01^(1/block_length)
  } else if (boot != 4){
    warning("Argument ar_AWB set, but AWB method not used. ar_AWB is ignored.")
  }

  # Defaults
  p_vec <- NULL
  if (boot_sqt_test) {
    if (is.numeric(steps) & all(steps == floor(steps) & steps >= 0) & !anyNA(steps)) {
      p_vec <- sort(unique(steps[steps <= N]))
      if (max(p_vec) < N) {
        p_vec <- c(p_vec, N)
      }
      if (min(p_vec) > 0) {
        p_vec <- c(0, p_vec)
      }
      if (!identical(p_vec, steps)) {
        warning(paste0(paste0("Input to argument steps transformed to fit sequential test: steps = c("),
                       paste0(p_vec, collapse = ", "), ")."))
      }
      if (!identical(unique(p_vec), p_vec)) {
        p_vec <- unique(p_vec)
        warning(paste0(paste0("Input to argument steps transformed to remove duplicate groups: steps = c("),
                       paste0(p_vec, collapse = ", "), ")."))
      }
    } else if (is.numeric(steps) & all(steps >= 0 & steps <= 1) & !anyNA(steps)) {
      q_vec <- sort(unique(steps))
      if (max(q_vec) < 1) {
        q_vec <- c(q_vec, 1)
      }
      if (min(q_vec) > 0) {
        q_vec <- c(0, q_vec)
      }
      if (!identical(q_vec, steps)) {
        warning(paste0(paste0("Input to argument steps transformed to fit sequential test: steps = c("),
                       paste0(q_vec, collapse = ", "), ")"))
      }

      p_vec <- round(q_vec * N)
      if (!identical(unique(p_vec), p_vec)) {
        p_vec <- unique(p_vec)
        warning(paste0(paste0("Input to argument steps transformed to remove duplicate groups after transformation to integers: steps = c("),
                       paste0(p_vec, collapse = ", "), ")."))
      }
    } else {
      stop("Invalid input values for steps: must be quantiles or positive integers.")
    }
  }
  if (boot == 1){
    # Obtain the probability that we draw identical blocks for the whole time series:
    # This will cause multicollinearity if max_lag is larger than block_length.
    prob_identical_bl <- 1 - (1 - (1 / (n - block_length))^(ceiling(n / block_length) - 1))^B
  }
  if(is.null(max_lag)){
    # Correction for small samples as formula doesn't work well for micropanels
    max_lag = round(12*(n/100)^(1/4)) - 7*max(1 - n/50, 0)*(n/100)^(1/4)
    if (boot == 1) {
      if (prob_identical_bl > 0.01) {
        # If the probability of obtaining a multicollinear bootstrap sample is too large,
        # force max_lag to be smaller than block_length.
        max_lag <- min(max_lag, block_length)
      }
    }
  }
  s_DWB <- matrix(0, n, n)
  if (boot == 3) {
    # Self-convolution
    self.conv <- function(t, c) {
      y <- apply(as.matrix(t), c(1,2), FUN = function(x){
        stats::integrate(f.w, -1, 1, t = x, c = c)$value})
      return(y)
    }
    # Integrand
    f.w <- function(x, t, c) {
      y <- w.trap(x, c) * w.trap(x + abs(t), c)
      return(y)
    }
    # Trapezoid
    w.trap <- function(x, c) {
      y <- (x/c) * (x >= 0) * (x<c) + (x >= c) * (x <= 1-c) + ((1-x) / c) * (x > 1-c) * (x <= 1)
      return(y)
    }
    m <- self.conv(outer(1:n, 1:n, "-") / block_length, 0.43)/ drop(self.conv(0, 0.43))
    ev <- eigen(m)
    e_va <- ev$values
    e_ve <- ev$vectors
    e_va <- diag((e_va > 1e-10) * e_va + (e_va <= 1e-10) * 1e-10)
    e_ve <- e_ve * (matrix(sign(e_ve[1,]), nrow = n, ncol = n, byrow = TRUE))
    s_DWB <- e_ve %*% sqrt(e_va)
  }
  out <- list(boot = boot, l = block_length, s_DWB = s_DWB, ar_AWB = ar_AWB, dc = dc,
              dc_boot = dc_boot, detr = detrend, detr_int = detr_int, ic = ic,
              p_max = max_lag, p_vec = p_vec, range_nonmiss = range_nonmiss, joint = joint)

  return(out)
}

#' Printing Summary Output for Objects of class mult_htest
#' @description This function prints summary output for objects of class mult_htest (for multiple hypothesis testing)
#' @export
#' @keywords internal
print.mult_htest <- function(x, digits = max(3, getOption("digits") - 3), ...){
  cat("\n")
  cat(strwrap(x$method, prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data: ", x$data.name, "\n", sep = "")

  if (!is.null(x$alternative)) {
    cat("alternative hypothesis: ")
    if (!is.null(x$null.value)) {
      alt.char <- switch(x$alternative,
                         two.sided = "not equal to",
                         less = "less than",
                         greater = "greater than")
      cat("true ", names(x$null.value), " is ", alt.char, " ", x$null.value, "\n", sep = "")
    } else {
      cat(x$alternative, "\n", sep = "")
    }
  }
  cat("\n")
  if (x$specifications$mult_test_ctrl == "none") {
    N <- NROW(x$statistic)
    out_matrix <- matrix(nrow = N, ncol = 3)
    colnames(out_matrix) <- c("estimate", "statistic", "p-value")
    rownames(out_matrix) <- x$series.names
    if (!is.null(x$estimate)) {
      out_matrix[, 1] <- x$estimate
    } else{
      out_matrix[, 1] <- NA
    }
    if (!is.null(x$statistic)) {
      out_matrix[, 2] <- x$statistic
    } else{
      out_matrix[, 2] <- NA
    }
    if (!is.null(x$p.value)) {
      out_matrix[, 3] <- x$p.value
    } else{
      out_matrix[, 3] <- NA
    }
    cat("Tests performed on each series:", "\n")
    print(out_matrix, digits = digits, ...)
  } else {
    cat("Sequence of tests:", "\n")
    print(x$details[[x$specifications$mult_test_ctrl]], digits = digits, ...)
  }
  cat("\n")
  invisible(x)
}

#' Printing Summary Output for Objects of class bootUR
#' @description This function prints summary output for objects of class bootUR (for unit root testing)
#' @export
#' @keywords internal
print.bootUR <- function(x, digits = max(3, getOption("digits") - 3), ...){
  cat("\n")
  cat(strwrap(x$method, prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data: ", x$data.name, "\n", sep = "")
  N <- NROW(x$statistic)

  if (!is.null(x$details$txt_null)) {
    cat("null hypothesis: ")
    cat(x$details$txt_null, "\n", sep = "")
  }
  if (!is.null(x$details$txt_alternative)) {
    cat("alternative hypothesis: ")
    cat(x$details$txt_alternative, "\n", sep = "")
  } else if (!is.null(x$alternative)) {
    cat("alternative hypothesis: ")
    if (!is.null(x$null.value)) {
      alt.char <- switch(x$alternative,
                         two.sided = "not equal to",
                         less = "less than",
                         greater = "greater than")
      cat("true ", names(x$null.value), " is ", alt.char, " ", x$null.value, "\n", sep = "")
    } else {
      cat(x$alternative, "\n", sep = "")
    }
  }
  cat("\n")
  if ("mult_test_ctrl" %in% names(x$specifications)) {
    if (x$specifications$mult_test_ctrl == "none") {
      out_matrix <- matrix(nrow = N, ncol = 3)
      colnames(out_matrix) <- c("estimate largest root", "statistic", "p-value")
      rownames(out_matrix) <- x$series.names
      if (!is.null(x$estimate)) {
        out_matrix[, 1] <- x$estimate + 1
      } else{
        out_matrix[, 1] <- NA
      }
      if (!is.null(x$statistic)) {
        out_matrix[, 2] <- x$statistic
      } else{
        out_matrix[, 2] <- NA
      }
      if (!is.null(x$p.value)) {
        out_matrix[, 3] <- x$p.value
      } else{
        out_matrix[, 3] <- NA
      }
      if (N > 1) {
        cat("Tests performed on each series:", "\n")
      }
      print(out_matrix, digits = digits, ...)
    } else {
      cat("Sequence of tests:", "\n")
      print(x$details[[x$specifications$mult_test_ctrl]], digits = digits, ...)
    }
  } else {
    out_matrix <- matrix(nrow = N, ncol = 3)
    colnames(out_matrix) <- c("estimate largest root", "statistic", "p-value")
    rownames(out_matrix) <- x$series.names
    if (!is.null(x$estimate)) {
      out_matrix[, 1] <- x$estimate + 1
    } else{
      out_matrix[, 1] <- NA
    }
    if (!is.null(x$statistic)) {
      out_matrix[, 2] <- x$statistic
    } else{
      out_matrix[, 2] <- NA
    }
    if (!is.null(x$p.value)) {
      out_matrix[, 3] <- x$p.value
    } else{
      out_matrix[, 3] <- NA
    }
    print(out_matrix, digits = digits, ...)
  }
  cat("\n")
  invisible(x)
}
