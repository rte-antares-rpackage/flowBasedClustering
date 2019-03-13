context("Function .getDistExt")

test_that(".getDistExt", {
  library(data.table)
  library(quadprog)
  object <- readRDS(system.file("testdata/object.rds", package = "flowBasedClustering"))

  PL <- object[[4]]
  x0 <- object[[1]]
  ram <- object[[6]]
  Dmat <- diag(1, nrow = nrow(x0))
  Ax0 <- object[[3]]

  res <- .getDistExt(V1 = x0, PL = -t(PL) , ram = ram, Dmat = Dmat, col_ptdf = NULL)
  expect_true(res < 496 & res > 494)
})
