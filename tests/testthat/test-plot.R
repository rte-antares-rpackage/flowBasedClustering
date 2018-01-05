context("graphs funciton")

test_that("Prepare data", {
  
  PTDF <- fread(system.file("dataset/ptdf_example.csv",package = "flowBasedClustering"))
  #Plot unique PTDF
  PTDF1 <- PTDF[ Date == "06/08/2016" & Period == 1]
  plo <- plotFlowbased(PTDF1, country1 = "DE", country2 = "BE", domainsNames = "06/08/2016")
  expect_true("AmChart"%in% class(plo))
  
  plo <- plotFlowbased(PTDF1, country1 = "DE", country2 = "BE")
  expect_true("AmChart"%in% class(plo))
  
  expect_error(plotFlowbased(PTDF2, country1 = "DE",
                             country2 = "BE",
                             domainsNames = c("1", "2"), main = "Myplot"))
  
  PTDF2 <- list(PTDF[ Date == "06/08/2016" & Period == 1],
  PTDF[ Date == "06/08/2016" & Period == 2],
  PTDF[ Date == "06/08/2016" & Period == 3])
  plo <- plotFlowbased(PTDF2, country1 = "DE",
                       country2 = "BE", domainsNames = c("01/11/2015 - H1",
                        "01/11/2015 - H2", "01/11/2015 - H3"), main = "Myplot")
  expect_true("AmChart"%in% class(plo))
  
  
  
  expect_error(plotFlowbased(PTDF2, country1 = "DE",
                             country2 = "BE",
                             domainsNames = c("01/11/2015 - H1"), main = "Myplot"))
  
  plo <- plotFlowbased(PTDF2, country1 = "DE",
                       country2 = "BE")
  expect_true("AmChart"%in% class(plo))
  
})

