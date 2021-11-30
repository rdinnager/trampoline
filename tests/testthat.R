Sys.setenv("R_TESTS" = "")

library(testthat)
library(trampoline)

test_check("trampoline")
