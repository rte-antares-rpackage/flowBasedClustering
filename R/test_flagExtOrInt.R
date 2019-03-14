context("Function .flagExtOrInt")

test_that(".flagExtOrInt", {
  library(data.table)
  object_int <- readRDS(system.file("testdata/object_int.rds", package = "flowBasedClustering"))
  object_ext <- readRDS(system.file("testdata/object.rds", package = "flowBasedClustering"))

  ram_int <- object_int[[4]]
  Ax0_int <- object_int[[2]]
  ram_ext <- object_ext[[6]]
  Ax0_ext <- object_ext[[3]]
  
  res_int <- .flagExtOrInt(Ax0 = Ax0_int, b = ram_int)
  res_ext <- .flagExtOrInt(Ax0 = Ax0_ext, b = ram_ext)
  expect_true(res_ext)
  expect_false(res_int)
})