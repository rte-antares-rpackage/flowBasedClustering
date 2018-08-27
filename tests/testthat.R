#Copyright © 2016 RTE Réseau de transport d’électricité

library(testthat)
library(flowBasedClustering)

# This follow line allow to perform test on parallel functions
Sys.unsetenv("R_TESTS")
Sys.setenv("RGL_USE_NULL"=TRUE)
options(rgl.useNULL = TRUE)


if (Sys.getenv("DO_R_TESTS") %in% c(1, "TRUE", "true", TRUE))
{
  test_check("flowBasedClustering")
}