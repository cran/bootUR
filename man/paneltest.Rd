% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootURtests.R
\name{paneltest}
\alias{paneltest}
\title{Panel Unit Root Test}
\usage{
paneltest(y, level = 0.05, boot = "AWB", B = 1999, l = NULL,
  ar_AWB = NULL, union = TRUE, p_min = 0, p_max = NULL, ic = "MAIC",
  dc = NULL, detr = NULL, ic_scale = TRUE, verbose = FALSE,
  show_progress = FALSE, do_parallel = FALSE, nc = NULL)
}
\arguments{
\item{y}{A \eqn{T}-dimensional vector or a (\eqn{T} x \eqn{N})-matrix of \eqn{N} time series with \eqn{T} observations to be tested for unit roots. Data may also be in a time series format (e.g. \code{ts}, \code{zoo} or \code{xts}), or a data frame, as long as each column represents a single time series.}

\item{level}{Desired significance level of the unit root test. Default is 0.05.}

\item{boot}{String for bootstrap method to be used. Options are
\describe{
\item{\code{"MBB"}}{Moving blocks bootstrap (Paparoditis and Politis, 2003; Palm, Smeekes and Urbain, 2011);}
\item{\code{"BWB"}}{Block wild bootstrap (Shao, 2011; Smeekes and Urbain, 2014a);}
\item{\code{"DWB"}}{Dependent wild bootstrap (Shao, 2010; Smeekes and Urbain, 2014a; Rho and Shao, 2019);}
\item{\code{"AWB"}}{Autoregressive wild bootstrap (Smeekes and Urbain, 2014a; Friedrich, Smeekes and Urbain, 2020), this is the default;}
\item{\code{"SB"}}{Sieve bootstrap (Chang and Park, 2003; Palm, Smeekes and Urbain, 2008; Smeekes, 2013);}
\item{\code{"SWB"}}{Sieve wild boostrap (Cavaliere and Taylor, 2009; Smeekes and Taylor, 2012).}
}}

\item{B}{Number of bootstrap replications. Default is 1999.}

\item{l}{Desired 'block length' in the bootstrap. For the MBB, BWB and DWB boostrap, this is a genuine block length. For the AWB boostrap, the block length is transformed into an autoregressive parameter via the formula \eqn{0.01^(1/l)} as in Smeekes and Urbain (2014a); this can be overwritten by setting \code{ar_AWB} directly. Default sets the block length as a function of the time series length T, via the rule \eqn{l = 1.75 T^(1/3)} of Palm, Smeekes and Urbain (2011).}

\item{ar_AWB}{Autoregressive parameter used in the AWB bootstrap method (\code{boot = "AWB"}). Can be used to set the parameter directly rather than via the default link to the block length l.}

\item{union}{Logical indicator whether or not to use bootstrap union tests (\code{TRUE}) or not (\code{FALSE}), see Smeekes and Taylor (2012). Default is \code{TRUE}.}

\item{p_min}{Minimum lag length in the augmented Dickey-Fuller regression. Default is 0.}

\item{p_max}{Maximum lag length in the augmented Dickey-Fuller regression. Default uses the sample size-based rule \eqn{12(T/100)^{1/4}}.}

\item{ic}{String for information criterion used to select the lag length in the augmented Dickey-Fuller regression. Options are: \code{"AIC"}, \code{"BIC"}, \code{"MAIC"}, \code{"MBIC"}. Default is \code{"MAIC"} (Ng and Perron, 2001).}

\item{dc}{Numeric vector indicating the deterministic specification. Only relevant if \code{union = FALSE}. Options are (combinations of)

\verb{0 } no deterministics;

\verb{1 } intercept only;

\verb{2 } intercept and trend.

If \code{union = FALSE}, the default is adding an intercept (a warning is given).}

\item{detr}{String vector indicating the type of detrending to be performed. Only relevant if \code{union = FALSE}. Options are: \code{"OLS"} and/or \code{"QD"} (typically also called GLS, see Elliott, Rothenberg and Stock, 1996). The default is \code{"OLS"}.}

\item{ic_scale}{Logical indicator whether or not to use the rescaled information criteria of Cavaliere et al. (2015) (\code{TRUE}) or not (\code{FALSE}). Default is \code{TRUE}.}

\item{verbose}{Logical indicator whether or not information on the outcome of the unit root test needs to be printed to the console. Default is \code{FALSE}.}

\item{show_progress}{Logical indicator whether a bootstrap progress update should be printed to the console. Default is FALSE.}

\item{do_parallel}{Logical indicator whether bootstrap loop should be executed in parallel. Parallel computing is only available if OpenMP can be used, if not this option is ignored. Default is FALSE.}

\item{nc}{The number of cores to be used in the parallel loops. Default is to use all but one.}
}
\value{
For the union test (\code{union = TRUE}), the test statistic and p-value are returned. If \code{union = FALSE}, the test statistics and p-values are reported per type of deterministic component (\code{dc}) and detrending method (\code{detr}).
}
\description{
Performs a test on a multivariate (panel) time series by testing the null hypothesis that all series have a unit root. The test is based on averaging the individual test statistics, also called the Group-Mean (GM) test in Palm, Smeekes and Urbain (2011).
}
\details{
See \code{\link{iADFtest}} for details on the bootstrap algorithm and lag selection.
}
\section{Errors and warnings}{

\describe{
\item{\code{Error: Resampling-based bootstraps MBB and SB cannot handle missing values.}}{If the time series in \code{y} have different starting and end points (and thus some series contain \code{NA} values at the beginning and/or end of the sample, the resampling-based moving block bootstrap (MBB) and sieve bootstrap (SB) cannot be used, as they create holes (internal missings) in the bootstrap samples. Switch to another bootstrap method or truncate your sample to eliminate \code{NA} values.}
\item{\code{Warning: SB and SWB bootstrap only recommended for iADFtest; see help for details.}}{Although the sieve bootstrap methods \code{"SB"} and \code{"SWB"} can be used, Smeekes and Urbain (2014b) show that these are not suited to capture general forms of dependence across units, and using them for joint or multiple testing is not valid. This warning thereofre serves to recommend the user to consider a different bootstrap method.}
\item{\code{Warning: Deterministic specification in argument dc is ignored, as union test is applied.}}{The union test calculates the union of all four combinations of deterministic components (intercept or intercept and trend) and detrending methods (OLS or QD). Setting deterministic components manually therefore has no effect.}
\item{\code{Warning: Detrending method in argument detr is ignored, as union test is applied.}}{The union test calculates the union of all four combinations of deterministic components (intercept or intercept and trend) and detrending methods (OLS or QD). Setting detrending methods manually therefore has no effect.}
}
}

\examples{
# paneltest on GDP_BE and GDP_DE
two_series_paneltest <- paneltest(MacroTS[, 1:2], boot = "AWB", B = 399,  verbose = TRUE)
}
\references{
Chang, Y. and Park, J. (2003). A sieve bootstrap for the test of a unit root. \emph{Journal of Time Series Analysis}, 24(4), 379-400.

Cavaliere, G. and Taylor, A.M.R (2009). Heteroskedastic time series with a unit root. \emph{Econometric Theory}, 25, 1228–1276.

Cavaliere, G., Phillips, P.C.B., Smeekes, S., and Taylor, A.M.R.
(2015). Lag length selection for unit root tests in the presence of nonstationary volatility. \emph{Econometric Reviews}, 34(4), 512-536.

Elliott, G., Rothenberg, T.J., and Stock, J.H. (1996). Efficient tests for an autoregressive unit root. \emph{Econometrica}, 64(4), 813-836.

Friedrich, M., Smeekes, S. and Urbain, J.-P. (2020). Autoregressive wild bootstrap inference for nonparametric trends. \emph{Journal of Econometrics}, 214(1), 81-109.

Ng, S. and Perron, P. (2001). Lag Length Selection and the Construction of Unit Root Tests with Good Size and Power. \emph{Econometrica}, 69(6), 1519-1554,

Palm, F.C., Smeekes, S. and Urbain, J.-P. (2008). Bootstrap unit root tests: Comparison and extensions. \emph{Journal of Time Series Analysis}, 29(1), 371-401.

Palm, F. C., Smeekes, S., and Urbain, J.-.P. (2011). Cross-sectional dependence robust block bootstrap panel unit root tests. \emph{Journal of Econometrics}, 163(1), 85-104.

Paparoditis, E. and Politis, D.N. (2003). Residual-based block bootstrap for unit root testing. \emph{Econometrica}, 71(3), 813-855.

Perron, P. and Qu, Z. (2008). A simple modification to improve the finite sample properties of Ng and Perron's unit root tests. \emph{Economic Letters}, 94(1), 12-19.

Rho, Y. and Shao, X. (2019). Bootstrap-assisted unit root testing with piecewise locally stationary errors. \emph{Econometric Theory}, 35(1), 142-166.

Shao, X. (2010). The dependent wild bootstrap. \emph{Journal of the American Statistical Association}, 105(489), 218-235.

Shao, X. (2011). A bootstrap-assisted spectral test of white noise under unknown dependence. \emph{Journal of Econometrics}, 162, 213-224.

Smeekes, S. (2013). Detrending bootstrap unit root tests. \emph{Econometric Reviews}, 32(8), 869-891.

Smeekes, S. and Taylor, A.M.R. (2012). Bootstrap union tests for unit roots in the presence of nonstationary volatility. \emph{Econometric Theory}, 28(2), 422-456.

Smeekes, S. and Urbain, J.-P. (2014a). A multivariate invariance principle for modified wild bootstrap methods with an application to unit root testing. GSBE Research Memorandum No. RM/14/008, Maastricht University

Smeekes, S. and Urbain, J.-P. (2014b). On the applicability of the sieve bootstrap in time series panels. \emph{Oxford Bulletin of Economics and Statistics}, 76(1), 139-151.
}
\seealso{
\code{\link{iADFtest}}
}
