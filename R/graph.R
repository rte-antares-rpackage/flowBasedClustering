#' Generate html report
#'
#' @param dayType \code{numeric} Typical day
#' @param output_file \code{character} path of output
#' @param data \code{data.table} data from \link{classifTypicalDay}
#'
#'
#' @examples
#'
#' \dontrun{
#' sapply(1:12, generateClusteringReport)
#' generateClusteringReport(dayType = 7)
#' }
#' @export
generateClusteringReport <- function(dayType, output_file = NULL,
                             data = NULL){

  if(is.null(data))
  {
    data <- readRDS(system.file("dev/ClassifOut.RDS",package = "flowBasedClustering"))
  }

  if(is.null(output_file)){
    output_file <- getwd()
  }
  output_Dir <- output_file
  output_file <- paste0(output_file, "/", "FlowBased_clustering",dayType, "_", Sys.Date(), ".html")
  e <- environment()
  e$dayType <- dayType
  e$data <- data

  rmarkdown::render(system.file("/report/resumeclustflex.Rmd", package = "flowBasedClustering"),
                    output_file = output_file,
                    params = list(set_title = paste0("Typical Day ", dayType, " (generated on ", Sys.Date(), ")")),
                    intermediates_dir = output_Dir, envir = e,
                    quiet = TRUE)
}



#' Generate a plot for a typical day cluster
#'
#' @param data \code{data.table}, output of \link{classifTypicalDay}
#' @param country1 \code{character}, name of first country
#' @param country2 \code{character}, name of second country
#' @param hour \code{numeric}, hour
#' @param dayType  \code{numeric}, dayType
#' @param typicalDayOnly : plot only typical day ?
#' @param ggplot : ggplot or amCharts ?
#'
#' @import rAmCharts
#'
#' @export
clusterPlot <- function(data, country1, country2, hour, dayType,
                        typicalDayOnly = FALSE, ggplot = FALSE){
  dataPlot <- .getDataPlotClustering(data[idDayType==dayType],  country1, country2, hour)
  .makeGraph(dataPlot, data[idDayType==dayType]$TypicalDay,
             typicalDayOnly = typicalDayOnly, ggplot = ggplot)
}


# clusterPlot(Myclassif, "FR", "DE", 8, 9)


#' Prepare data for plot
#'
.getChull <- function(data, country1, country2){
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
.getDataPlotClustering <- function(allTypeDay, country1, country2, hour)
{
  tt <- apply(allTypeDay$dayIn[[1]][[1]][Period == hour], 1, function(data){
    ctry1 <- country1
    ctry2 <- country2
    dataChull <- .getChull(data$out, ctry1, ctry2)
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
#'@param data : data.frame
#'@param typicalDayDate : date of typical day
#'@param typicalDayOnly : plot only typical day ?
#'@param ggplot : ggplot or amCharts ?

.makeGraph <- function(data, typicalDayDate, typicalDayOnly = FALSE, ggplot = FALSE){
  ctry <- unique(substr(names(data), 11, 12))
  if(typicalDayOnly){
    dates <- typicalDayDate
  } else {
    dates <- unique(substr(names(data), 1, 10))
  }

  if(!ggplot){
    graphs <- sapply(dates, function(X){
      columns <- names(data)[grep(X,names(data))]
      if(X == typicalDayDate){
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
      addTitle(text = paste0("Flow-based  clustering ", ctry[1], "/", ctry[2])),
      setGraphs(graphs),
      setChartCursor(),
      addValueAxes(title = paste(ctry[1], "(MW)"), position = "bottom", minimum = -8000, maximum = 8000),
      addValueAxes(title =  paste(ctry[2], "(MW)"), minimum = -8000, maximum = 8000),
      setExport(enabled = TRUE)
    )
  } else {

    gg_data <- do.call("rbind.data.frame", lapply(dates, function(X){
      columns <- names(data)[grep(X,names(data))]
      tmp_data <- data[, columns]
      colnames(tmp_data) <- gsub(X, "", colnames(tmp_data))
      tmp_data <- tmp_data[!is.na(tmp_data[, 1]), ]
      tmp_data$date  <- X
      if(X == typicalDayDate){
        tmp_data$col <- "0"
        tmp_data$size <- 1
      }else {
        tmp_data$col <- "1"
        tmp_data$size <- 0.5
      }
      tmp_data
    }))

    ggplot(data=gg_data, aes(x = eval(parse(text=ctry[1])), y = eval(parse(text=ctry[2])), group = date, colour = col, size = size, linetype = as.character(col))) + geom_path() +
      geom_point()  + scale_size(range=c(0.1, 2), guide=FALSE) + theme(legend.position="none") +
      xlim(-8000, 8000) + ylim(-8000, 8000) +
      ggtitle(paste0("Flow-based  clustering ", ctry[1], "/", ctry[2])) +
      theme(plot.title = element_text(hjust = 0.5)) + ylab(paste(ctry[2], "(MW)")) +
      xlab(paste(ctry[1], "(MW)"))

  }
}
