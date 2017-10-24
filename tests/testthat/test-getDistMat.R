context("Function getDistMatrix")
library(data.table)
options(rgl.useNULL = TRUE)


test_that("Function getDistMatrix works and return appropriate object", {

  data <- fread(system.file("testdata/verticesForTestClustering.txt",package = "flowBasedClustering"))

  data <- data[,list(out = list(cbind(BE, DE, FR))), by = c("Date", "Period")]
  data[,mesh := list(flowBasedClustering:::.getMesh(out[[1]])),by = c("Date", "Period") ]
  
  ditMat <- flowBasedClustering:::.getDistMatrix(data, rep(1,24))
  # expect object dist
  expect_equal(class(ditMat), "dist")
  
  # expect no NA in the distances
  expect_false(any(is.na(as.matrix(ditMat))))

})
