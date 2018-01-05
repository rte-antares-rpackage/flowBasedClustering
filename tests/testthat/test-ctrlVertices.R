context("Function ctrlVertices")

test_that(".ctrlVertices", {
  PTDF <- fread(system.file("testdata/PTDF6h.csv",package = "flowBasedClustering"))
  PTDF <- .ctrlVertices(PTDF)
  expect_true(class(PTDF$Date) == "Date")
  expect_true(PTDF$Date[1] == as.Date("2015-11-01"))
  
  expect_error(.ctrlVertices(data.frame()))
  
})