context("Function pdtfToVertices")
library(data.table)

test_that(".ctrlvlPb works", {
  
  library(data.table)
  
  climate <- fread(system.file("dataset/climate_example.txt",package = "flowBasedClustering"))
  
  # load clustering results (or build them with clusteringTypicalDays function())
  clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
  
  
  class <- unique(clusterTD$Class)
  clVar <- names(climate)[!names(climate)%in%"Date"]
  
  
  
  #Test if levelsProba not between 0 and 1
  expect_error(.ctrlvlPb(2, clVar, class))
  
  levelsProba <- list(
    winterWd = list(FR_load = c(1/3, 2/3),
                    DE_wind = c(1/3, 2/3)),
    interSeasonWd2 = list(FR_load = c(1/3, 2/3),
                          DE_wind = c(1/3, 2/3), 
                          DE_solar = c(1/2))
  )
  expect_error(.ctrlvlPb(levelsProba, clVar, class))
  
  
  levelsProba <- list(
    winterWd = list(FR_load = c(1/3, 2/3),
                    DE_wind = c(1/3, 2/3)),
    interSeasonWd = list(FR_load = c(1/3, 2/3),
                          DE_wind2 = c(1/3, 2/3), 
                          DE_solar = c(1/2))
  )
  expect_error(.ctrlvlPb(levelsProba, clVar, class))
  
  
  
  
  levelsProba <- list(
    FR_load = c(1/3, 2/3), DE_wind = c(1/3, 2/3)
  )
  
  expect_error(.ctrlvlPb(levelsProba, clVar, class))
  
  
  levelsProba <- list(
    summerWd = c(1/3, 2/3)
  )
  expect_error(.ctrlvlPb(levelsProba, clVar, class))
  
  levelsProba <- list(
    winterWd = list(FR_LAOD = c(1/3, 2/3), DE_WIND = c(1/3, 2/3))
  )
  expect_error(.ctrlvlPb(levelsProba, clVar, class))
  
  
  
  
})