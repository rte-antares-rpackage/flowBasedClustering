library(data.table)

test_that("compare one and all", {
  
  vertices <- fread(system.file("dataset/vertices_example.txt",package = "flowBasedClustering"))
  dates <- seq(as.Date("2016-08-09"), as.Date("2018-09-01"), by = "day")
  hourWeight = rep(1, 24)
  
  out <- clusterTypicalDaysForOneClass(vertices = vertices,
                                       dates = dates, nbCluster = 2,
                                       className = "myName", id_start = 5, report = FALSE)
  
  
  calendar <- list()
  calendar$interSeasonWe <- c("2016-09-17", "2016-09-18")
  calendar$interSeasonWd <- c("2016-09-19", "2016-09-20", "2016-09-21", "2016-09-22", "2016-09-23")
  calendar$winterWe <- c("2016-12-10", "2016-12-11")
  calendar$winterWd <- seq(as.Date("2016-08-09"), as.Date("2018-09-01"), by = "day")
  calendar$summerWe <- c("2016-12-12", "2016-12-13", "2016-12-14", "2016-12-15", "2016-12-16") 
  calendar$summerWd <- c("2016-08-08", "2016-08-09", "2016-08-10", "2016-08-11", "2016-08-12")
  
  # run clustering algorithm
  clusterTD <- clusteringTypicalDays(calendar, vertices, nbClustWeek = 2, nbClustWeekend = 1, report = FALSE)
  
  expect_true(identical(clusterTD[Class == "winterWd"]$dayIn, out$dayIn))
  expect_true(identical(clusterTD[Class == "winterWd"]$TypicalDay, out$TypicalDay))
  expect_true(identical(clusterTD[Class == "winterWd"]$distance, out$distance))
  
})


test_that("compare one and all", {
  
  
  #create small calendar
  calendar <- list()
  calendar$interSeasonWe <- c("2016-09-17", "2016-09-18")
  calendar$interSeasonWd <- c("2016-09-19", "2016-09-20", "2016-09-21", "2016-09-22", "2016-09-23")
  calendar$winterWe <- c("2016-12-10", "2016-12-11")
  calendar$winterWd <- c("2016-12-12", "2016-12-13", "2016-12-14", "2016-12-15", "2016-12-16")
  calendar$summerWe <- c("2016-08-06", "2016-08-07")
  calendar$summerWd <- c("2016-08-08", "2016-08-09", "2016-08-10", "2016-08-11", "2016-08-12")
  
  # open vertices
  vertices <- fread(system.file("dataset/vertices_example.txt", package = "flowBasedClustering"))
  
  # weights
  weights <- rep(0,24)
  weights[19] <- 1
  
  
  
  # clustering1Class
  out <- clusterTypicalDaysForOneClass(vertices = vertices, dates = calendar$winterWd, nbCluster = 2,
                                       className = "winterWd", id_start = 4, report = FALSE, hourWeight = weights)
  
  
  # cluster all 
  clusterTD <- clusteringTypicalDays(calendar, vertices, nbClustWeek = 2,nbClustWeekend = 1,
                                     report = FALSE, hourWeight  = weights)
  
  # les jours types sont inversés
  expect_true(identical(out[idDayType == 4] ,  clusterTD[idDayType == 4]))

  expect_true(identical(out[idDayType == 5] ,  clusterTD[idDayType == 5]))
  
})