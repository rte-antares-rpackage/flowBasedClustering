context("Function clusterTypicalDaysForOneClass")

test_that("clusterTypicalDaysForOneClass", {
  library(data.table)
   vertices <- fread(system.file("dataset/vertices_example.txt",package = "flowBasedClustering"))
   dates <- seq(as.Date("2016-08-09"), as.Date("2016-08-12"), by = "day")
   
   out <- clusterTypicalDaysForOneClass(vertices = vertices,
                                 dates = dates, nbCluster = 2,
                                 className = "myName", id_start = 5, report = FALSE)
   
   expect_equal(nrow(out), 2)
   expect_equal(out$idDayType, 5:6)
   
   apply(out, 1, function(X){
     expect_true(X$TypicalDay == X$distance$Date[which.min(X$distance$Distance)])
   })
})