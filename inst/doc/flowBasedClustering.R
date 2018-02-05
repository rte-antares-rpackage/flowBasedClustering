## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
require(flowBasedClustering)

## ---- eval=FALSE---------------------------------------------------------
#  
#  # Define a calendar (period on which the clustering will be made + distinction of each season)
#  dates <- getSequence("2015-11-01", "2017-01-20")
#  interSeasonBegin <- c("2016-03-01", "2016-10-01")
#  interSeasonEnd <- c("2016-05-15", "2016-10-31")
#  calendar <- getCalendar(dates, interSeasonBegin, interSeasonEnd)
#  
#  # Identify PTDF in a csv file
#  ptdf_file_path <- system.file("dataset/ptdf_example.csv", package = "flowBasedClustering")
#  # (note : this small dataset does not span on all the calendar defined above)
#  
#  # Transform PTDF data to vertices
#  vertices <- ptdfToVertices(PTDF = ptdf_file_path, nbCore = 4)
#  
#  # Cluster the typical days
#  clusterTD <- clusteringTypicalDays(calendar, vertices, nbClustWeek = 3,
#                        nbClustWeekend = 1, report = TRUE)
#  
#  # Get probabilities and quantiles from climate file in order to classify the typical days
#  climate <- fread(system.file("dataset/climate_example.txt",package = "flowBasedClustering"))
#  probMatrix <- getProbability(climate, cluster = clusterTD, levelsProba = c(1/3, 2*3))

## ---- eval = TRUE--------------------------------------------------------
getSequence("2015-11-01", "2015-11-05")

## ---- eval = TRUE--------------------------------------------------------
dates <- getSequence("2017-01-01", "2017-12-31")
interSeasonBegin <- c("2017-03-01", "2017-09-01")
interSeasonEnd <- c("2017-05-01", "2017-11-01")
cal <- getCalendar(dates, interSeasonBegin, interSeasonEnd)
str(cal)

## ---- eval = TRUE, echo = FALSE------------------------------------------
ptdf_data <- data.table::fread(system.file("dataset/ptdf_example.csv",package = "flowBasedClustering"),
                   data.table = F)
head(ptdf_data[, 1:7])

## ---- eval = TRUE, echo = FALSE------------------------------------------
vertices <- data.table::fread(system.file("dataset/vertices_example.txt",package = "flowBasedClustering"))
head(vertices)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  clusterTD <- clusteringTypicalDays(calendar, vertices, nbClustWeek = 3, nbClustWeekend = 1,
#                                report = TRUE)
#  

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  clusterTD <- clusterTypicalDaysForOneClass(vertices = vertices, dates = getSequence("2018-01-01", "2018-01-31"), nbCluster = 2,className = "january2018")
#  
#  

## ---- echo = FALSE-------------------------------------------------------
clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))

## ---- eval = FALSE-------------------------------------------------------
#  # build report for one typical day
#  generateClusteringReport(dayType = 7, data = clusterTD)

## ---- echo = TRUE, fig.width= 7, fig.height= 4.5, warning=FALSE----------
# static graphic
clusterPlot(clusterTD, country1 = "FR", country2 = "DE", 
            hour = 8, dayType = 9, typicalDayOnly = FALSE, ggplot = TRUE)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  # dynamic graphic
#  clusterPlot(clusterTD, country1 = "FR", country2 = "DE",
#              hour = 8, dayType = 9, typicalDayOnly = TRUE, ggplot = FALSE)

## ---- eval = TRUE, echo = FALSE------------------------------------------
climate <- data.table::fread(system.file("dataset/climate_example.txt",package = "flowBasedClustering"))
head(climate)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  # same quantiles for each variables
#  MatProb <- getProbability(climate, clusterTD, levelsProba = c(1/3, 2/3))
#  
#  # diffents quantiles for each class and variables
#  levelsProba <- list(summerWd = list(FR_load = c(0.5), DE_wind = c(1/3, 2/3), DE_solar = .5),
#                      winterWD = list(FR_load = c(0.5, 0.7), DE_wind = c(.5)))
#  MatProb <- getProbability(climate, clusterTD, levelsProba = levelsProba)

