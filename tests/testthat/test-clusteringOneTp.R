context("Function clusterTypicalDaysForOneClass")

test_that("clusterTypicalDaysForOneClass", {
  library(data.table)
  vertices <- fread(system.file("dataset/vertices_example.txt",package = "flowBasedClustering"))
  dates <- seq(as.Date("2016-08-06"), as.Date("2016-08-12"), by = "day")
  
  nbCluster = 2
  id_start = 5
  out <- clusterTypicalDaysForOneClass(vertices = vertices,
                                       dates = dates, nbCluster = nbCluster,
                                       className = "myName", id_start = id_start, report = FALSE)
  
  expect_equal(nrow(out), nbCluster)
  expect_equal(out$idDayType, id_start:(id_start+nbCluster-1))
  
  apply(out, 1, function(X){
    expect_true(X$TypicalDay == X$distance$Date[which.min(X$distance$Distance)])
  })
})