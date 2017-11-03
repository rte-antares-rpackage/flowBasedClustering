context("Function generateClusteringReport")

test_that("Function generateClusteringReport works", {
  clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
  generateClusteringReport(dayType = 2, data = clusterTD)
})
