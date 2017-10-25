context("Function getProbability")
library(data.table)
library(pipeR)
library(dplyr)

test_that("clustering getProbability works", {
  
  # load climate file (with NA)
  climate <- fread(system.file("testdata/climate2016.txt",package = "flowBasedClustering"))
  
  # load previsouly made typical day selection on a few days (cf. test-clustering.R)
  clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
  
  # run getProbability function
  probMatrix <- getProbability(climate, cluster = clusterTD, levelsProba = c(0.333, 0.666))
  
  # expect list of size 2
  expect_type(probMatrix, "list")
  expect_length(probMatrix, 2)
  
  # check that : for a combinaison of class AND levels of each variable,
  # the sum of the probabilities for each typical day is one
  
  shoubBeOne <- probMatrix[[1]] %>% group_by(Class, FR_load, DE_wind, DE_solar)  %>% 
                    summarise(sumOfProbabilities = sum(Proba))

  expect_true(all(abs(shoubBeOne$sumOfProbabilities - 1) < 0.00001))

  # check that there is no NA in output :
  expect_false(any(is.na(probMatrix[[1]]$Proba)))
  
  # getProbability without NA extrapolation
  probMatrixWithNA <- getProbability(climate, cluster = clusterTD, levelsProba = c(0.333, 0.666), extrapolationNA = FALSE)
  
  # check that there is a NA when there is no historical record
  expect_true(all(is.na(probMatrixWithNA[[1]][effectifClass == 0]$Proba)))
  
  # check that non-NA values are similar
  pNA <- probMatrixWithNA[[1]]$Proba
  p <- probMatrix[[1]]$Proba
  expect_true(length(pNA) == length(p))
  expect_true(all(pNA[!is.na(pNA)] == p[!is.na(pNA)]))
  
  
  # no automatic test yet exists on how the NA values have been replaced with extrapolationNA = TRUE
  
  # getProbability with different levels
  levelsProba <- list(FR_load = c(0.2,0.25,0.5,0.75,0.90, 0.95), DE_wind = c(1/3, 2/3),DE_solar = c(0.5) )
  probMatrix2 <- getProbability(climate, clusterTD, levelsProba = levelsProba)
  
  # check if size of outputs are OK 
  expect_equal(nrow(probMatrix[[1]]), 9*3*3*3) # cluster * load levels * wind levels * solar levels
  expect_equal(nrow(probMatrix2[[1]]), 9*7*3*2) # cluster * load levels * wind levels * solar levels
  
})
