context("Function .getDistInt")

test_that(".getDistInt", {
  library(data.table)
  object_int <- readRDS(system.file("testdata/object_int.rds", package = "flowBasedClustering"))
  PL <- object_int[[3]]
  x0 <- object_int[[1]]
  ram <- object_int[[4]]
  Ax0 <- object_int[[2]]
  
  res <- .getDistInt(V1 = x0, PL = PL , ram = ram, Ax0 = Ax0)
  expect_true(res < 291 & res > 289)
})