context("Function clustering")
library(data.table)

test_that("clustering typical days works", {
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
  
  clusterTD <- clusteringTypicalDays(calendar, vertices, nbClustWeek = 2, 
                                     nbClustWeekend = 1, report = FALSE)
  
  #↔ a few checks on the results
  expect_type(clusterTD, "list")
  expect_equal(nrow(clusterTD), 9)
  
  expect_true(all(clusterTD[Class == "interSeasonWe"]$TypicalDay %in% calendar$interSeasonWe))
  expect_true(all(clusterTD[Class == "interSeasonWd"]$TypicalDay %in% calendar$interSeasonWd))
  expect_true(all(clusterTD[Class == "winterWe"]$TypicalDay %in% calendar$winterWe))
  expect_true(all(clusterTD[Class == "winterWd"]$TypicalDay %in% calendar$winterWd))
  expect_true(all(clusterTD[Class == "summerWe"]$TypicalDay %in% calendar$summerWe))
  expect_true(all(clusterTD[Class == "summerWd"]$TypicalDay %in% calendar$summerWd))
  
  apply(clusterTD, 1, function(X){
    expect_true(X$TypicalDay == X$distance$Date[which.min(X$distance$Distance)])
  })
  
})


