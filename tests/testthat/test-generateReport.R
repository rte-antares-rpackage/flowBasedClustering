context("Function generateClusteringReport")

test_that("Function generateClusteringReport works", {
  clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
  temp_directory <- tempdir()
  generateClusteringReport(dayType = 2, data = clusterTD, outputFile = temp_directory)
})
