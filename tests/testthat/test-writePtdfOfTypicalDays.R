 context("writePtdfOfTypicalDays")
 
 test_that("writePtdfOfTypicalDays", {
   
   
   clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
   PTDF = system.file("dataset/ptdf_example.csv",
         package = "flowBasedClustering")
   tempDir <- tempdir()
   writePtdfOfTypicalDays(clusterTD, PTDF, tempDir)
   
   expect_true(file.exists(paste0(tempDir, "/PTDF.csv")))

   unlink(tempDir, recursive = TRUE)
   
 })