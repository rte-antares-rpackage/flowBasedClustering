## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
require(flowBasedClustering)

## ---- eval=FALSE---------------------------------------------------------
#  
#  # Generate calendar
#  dates <- getSequence("2015-11-01", "2017-01-20")
#  interSeasonBegin <- c("2016-03-01", "2016-10-01")
#  interSeasonEnd <- c("2016-05-15", "2016-10-31")
#  # Define a calendar with season
#  calendar <- getCalendar(dates, interSeasonBegin, interSeasonEnd)
#  
#  # Transform PTDF data to vertices
#  vertices <- ptdfToVertices(PTDF = "faceAllYear.csv", nbCore = 4)
#  
#  # Cluster the typical days
#  clusterTD <- clusteringTypicalDays(calendar, vertices, nbClustWeek = 3,
#                        nbClustWeekend = 1, report = TRUE)
#  
#  # Get probabilities and quantiles from climate file in order to classify the typical days
#  climate <- fread(system.file("dev/Climat.txt",package = "flowBasedClustering"))
#  getProbability <- function(climate, cluster = clusterTD, levelsProba = c(0.333, 0.666)

## ---- eval = TRUE--------------------------------------------------------
# Specify a repository
getSequence("2015-11-01", "2015-11-05")

## ---- eval = TRUE--------------------------------------------------------
dates <- getSequence("2017-01-01", "2017-12-31")
interSeasonBegin <- c("2017-03-01", "2017-09-01")
interSeasonEnd <- c("2017-05-01", "2017-11-01")
cal <- getCalendar(dates, interSeasonBegin, interSeasonEnd)
str(cal)

## ---- eval = FALSE-------------------------------------------------------
#  ptdf_data <- fread(system.file("dev/data/faceAllYear.csv",package = "flowBasedClustering"),
#                     data.table = F)
#  head(ptdf_data[, 1:7])

## ---- eval = TRUE, echo = FALSE------------------------------------------
vertices <- data.table::fread(system.file("dev/verticesAllDay.txt",package = "flowBasedClustering"))
head(vertices)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  clusterTD <- clusteringTypicalDays(calendar, vertices, nbClustWeek = 3, nbClustWeekend = 1,
#                                report = TRUE)
#  

## ---- echo = FALSE-------------------------------------------------------
clusterTD <- readRDS(system.file("dev/ClassifOut.RDS",package = "flowBasedClustering"))

## ---- eval = FALSE-------------------------------------------------------
#  # build report for one typical day
#  generateClusteringReport(dayType = 7, data = clusterTD)

## ---- echo = TRUE, fig.width= 7, fig.height= 4.5, warning=FALSE----------
# static graphic
clusterPlot(clusterTD, country1 = "FR", country2 = "DE", 
            hour = 8, dayType = 9, typicalDayOnly = FALSE, ggplot = TRUE)

## ---- echo = TRUE--------------------------------------------------------
# dynamic graphic
clusterPlot(clusterTD, country1 = "FR", country2 = "DE", 
            hour = 8, dayType = 9, typicalDayOnly = TRUE, ggplot = FALSE)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  # same quantiles for each variables
#  MatProb <- getProbability(climate, clusterTD, levelsProba = c(0.333, 0.666))
#  
#  # diffents quantiles for each variables
#  levelsProba <- list(`Conso (J-1)` = c(0.2, 0.8), DE_wind = c(0.3, 0.7),DE_solar = c(0.3,0.4,0.8))
#  MatProb <- getProbability(climate, clusterTD, levelsProba = levelsProba)
