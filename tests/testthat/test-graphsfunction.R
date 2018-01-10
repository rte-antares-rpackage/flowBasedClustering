context("graphs funciton")

test_that("Prepare data", {
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
  
  out <- clusterPlot(clusterTD, "BE", "FR", 1, 1, ggplot = TRUE)
  expect_true("ggplot"%in%class(out))
  
  out <- clusterPlot(clusterTD, "BE", "FR", 1, 1, ggplot = FALSE)
  expect_true("htmlwidget"%in%class(out))
  
  
  
})