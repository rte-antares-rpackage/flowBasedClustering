#' Generate a plot for a typical day cluster
#'
#' @param data \code{data.table}, output of \link{classifTipycalDay}
#' @param country1 \code{character}, name of first country
#' @param country2 \code{character}, name of second country
#' @param hour \code{numeric}, hour
#' @param dayType  \code{numeric}, dayType
#' @param holiday \code{character}, holiday day default :
#'
#' @import rAmCharts
#'
#' @export
clusterPlot <- function(data, country1, country2, hour, dayType){
  datapreparing <- .prepareDataPlutClusturing(data[idDayType==dayType],  country1, country2, hour)
  .makeGraph(datapreparing, data[idDayType==dayType]$TypicalDay)
}


# clusterPlot(Myclassif, "FR", "DE", 8, 9)


#' Prepare data for plot
#'
.PrepareChull <- function(data, country1, country2){
  data <- data.frame(data)
  if(country1 == "NL"){
    ptctry <- -rowSums(data)
  }else{
    ptctry <- data[[country1]]
  }
  if(country2 == "NL"){
    ptctry2 <- -rowSums(data)
  }else{
    ptctry2 <- data[[country2]]
  }
  res <- cbind(ptctry, ptctry2)
  res <- res[chull(res),]
  res <- rbind(res, res[1,])
  res
}


#' Prepare data for plot
#'
.prepareDataPlutClusturing <- function(allTypDay, country1, country2, hour)
{
  tt <- apply(allTypDay$dayIn[[1]][[1]][Period == hour], 1, function(data){
    ctry1 <- country1
    ctry2 <- country2
    dataChull <- .PrepareChull(data$out, ctry1, ctry2)
    dataChull <- data.frame(dataChull)
    names(dataChull) <- c(paste0(data$Date, ctry1), paste0(data$Date, ctry2))
    round(data.table(dataChull), 0)
  })


  maxRow <- max(unlist(lapply(tt, nrow)))
  tt <- lapply(tt, function(X){
    rbind(X, data.table(rep(NA, maxRow-nrow(X)), rep(NA, maxRow-nrow(X))), use.names=FALSE)
  })
  tt <- cbind.data.frame(tt)
  tt
}

#' Graph function
#'
.makeGraph <- function(data, dateRef){
  ctry <- unique(substr(names(data), 11, 12))
  Dates <- unique(substr(names(data), 1, 10))
  graphS <- sapply(Dates, function(X){
    columns <- names(data)[grep(X,names(data))]
    if(X == dateRef){
      graph <- amGraph(title = X, balloonText =
                         paste0('<b>',X,'<br>', ctry[1], '</b> :[[x]] <br><b>',ctry[2], '</b> :[[y]]'),
                       bullet = 'circle', xField = columns[1],yField = columns[2],
                       lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineColor = "#FF0000",
                       lineThickness = 3)
    }else{
      graph <-  amGraph(title = X, balloonText =
                          paste0('<b>',X,'<br>', ctry[1], '</b> :[[x]] <br><b>',ctry[2], '</b> :[[y]]'),
                        bullet = 'circle', xField = columns[1],yField = columns[2],
                        lineAlpha = 1, bullet = "bubble", bulletSize = 4, lineColor = "#D3D3D3",
                        lineThickness = 1)
    }
    graph
  }, USE.NAMES = FALSE, simplify = FALSE)
  pipeR::pipeline(
    amXYChart(dataProvider = data),
    addTitle(text = paste0("Flow-based  clustering", ctry[1], "/", ctry[2])),
    setGraphs(graphS),
    setChartCursor(),
    addValueAxes(title = paste(ctry[1], "(MW)"), position = "bottom", minimum = -8000, maximum = 8000),
    addValueAxes(title =  paste(ctry[2], "(MW)"), minimum = -8000, maximum = 8000),
    setExport(enabled = TRUE)
  )

}
