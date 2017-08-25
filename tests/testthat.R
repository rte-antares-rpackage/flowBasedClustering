#Copyright © 2016 RTE Réseau de transport d’électricité

library(testthat)
library(flowBasedClustering)

# This follow line allow to perform test on parallel functions
Sys.unsetenv("R_TESTS")
Sys.setenv("RGL_USE_NULL"=TRUE)

test_check("flowBasedClustering")
