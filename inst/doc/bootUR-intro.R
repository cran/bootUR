## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install-cran, eval = FALSE-----------------------------------------------
#  install.packages("bootUR")

## ----install-github, eval = FALSE---------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("smeekes/bootUR")

## ----install-vign, eval = FALSE-----------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("smeekes/bootUR", build_vignettes = TRUE, dependencies = TRUE)

## ----load---------------------------------------------------------------------
library(bootUR)

## ----missing------------------------------------------------------------------
data("MacroTS")
check_missing_insample_values(MacroTS)

## ----start_end----------------------------------------------------------------
sample_check <- find_nonmissing_subsample(MacroTS)
# Provides the number of the first and last non-missing observation for each series:
sample_check$range 
# Gives TRUE if the time series all start and end at the same observation:
sample_check$all_equal

## ----plot_na, fig.height = 4, fig.width = 7, eval = FALSE---------------------
#  plot_missing_values(MacroTS, show_names = TRUE, axis_text_size = 5, legend_size = 6)

## ----adf----------------------------------------------------------------------
GDP_NL <- MacroTS[, 4]
adf(GDP_NL, deterministics = "trend")

## ----boot_adf-----------------------------------------------------------------
set.seed(155776)
boot_adf(GDP_NL, B = 399, bootstrap = "SB", deterministics = "trend", 
                    detrend = "OLS", do_parallel = FALSE)

## ----union--------------------------------------------------------------------
boot_union(GDP_NL, B = 399, bootstrap = "SWB", do_parallel = FALSE)

## ----panel--------------------------------------------------------------------
boot_panel(MacroTS, bootstrap = "DWB", B = 399, do_parallel = FALSE)

## ----iADF---------------------------------------------------------------------
ADFtests_out <- boot_ur(MacroTS[, 1:5], bootstrap = "MBB", B = 399, union = FALSE, 
                        deterministics = "trend", detrend = "OLS", do_parallel = FALSE)
print(ADFtests_out)

## ----BSQT---------------------------------------------------------------------
N <- ncol(MacroTS)
# Test each unit sequentially
boot_sqt(MacroTS, steps = 0:N, bootstrap = "AWB", B = 399, do_parallel = FALSE)
# Split in four equally sized groups (motivated by the 4 series per country)
boot_sqt(MacroTS, steps = 0:4 / 4, bootstrap = "AWB", B = 399, do_parallel = FALSE)

## ----bFDR---------------------------------------------------------------------
boot_fdr(MacroTS[, 1:10], FDR_level = 0.1, bootstrap = "BWB", B = 399, do_parallel = FALSE)

## ----orders-------------------------------------------------------------------
out_orders <- order_integration(MacroTS[, 11:15], method = "boot_fdr", B = 399, 
                                do_parallel = FALSE)
# Orders
out_orders$order_int
# Differenced data
stationary_data <- out_orders$diff_data

## ----plot_orders, fig.width = 7, fig.height = 4, eval = FALSE-----------------
#  plot_order_integration(out_orders)

