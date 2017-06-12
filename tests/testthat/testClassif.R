context("Classif")
library(data.table)
data <- fread(system.file("testdata/VerticesTest.txt",package = "flowBasedClustering"))

data <- data[,list(out = list(cbind(BE, DE, FR))), by = c("Date", "Period")]
data[,mesh := list(flowBasedClustering:::.getMesh(out[[1]])),by = c("Date", "Period") ]
ditMat <- flowBasedClustering:::.getDistMatrix(data)

expect_equal(class(ditMat), "dist")
