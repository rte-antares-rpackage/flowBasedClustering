context("Function getVertices")
library(data.table)


test_that("Test that object returned by getVertices is convex and that it fits expected results on a small dataset", {
  cat("start testgetVertices")
  PTDF <- fread(system.file("testdata/PTDF.csv",package = "flowBasedClustering"))
  SOMMETS <- fread(system.file("testdata/sommets/sommets_which_solver.csv",package = "flowBasedClustering"), sep = " ", header = TRUE)

  tt <- sapply(unique(SOMMETS$Id_day), function(X){
    sapply(unique(SOMMETS$Period),  function(Y){
      #print(Y)
      PTDFsel <- PTDF[Id_day == X & Period == Y]
      SOMMETSSel <- SOMMETS[Id_day == X & Period == Y,c("V1","V2","V3")]
      SOMMETSSel <- as.matrix(SOMMETSSel)
      SOMMETSSel <- round(SOMMETSSel, 0)
      SOMMETSSel <- SOMMETSSel[order(SOMMETSSel[,1], SOMMETSSel[,2],SOMMETSSel[,3]),]
      res <- flowBasedClustering::getVertices(as.matrix(PTDFsel[,.SD, .SDcols = c("BE","DE","FR","NL")]), PTDFsel$RAM)

      #Test convexity
      expect_equal(all(apply(res, 1, function(VV){
        c(VV, -sum(VV))%*%t(as.matrix(PTDFsel[,.SD, .SDcols = c("BE","DE","FR", "NL")]))<PTDFsel$RAM+2
      })), TRUE)

      res <- round(res, 0)
      res <- res[order(res[,1], res[,2],res[,3]),]
      sum(res- SOMMETSSel)
    })
  })

  expect_equal(all(tt>=-1) & all(tt<=1), TRUE)
  cat("end testgetVertices")
  
})

