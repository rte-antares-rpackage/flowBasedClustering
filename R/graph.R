#' Generate html report
#'
#' Write and save a report with the results of the clustering of the flow-based domains. For one typical day,
#' the report contains the representation of all the 24 hourly flow-based domains and their comparison
#' with the domains of the other historical days it represents.
#'
#' @param dayType \code{numeric} Typical day identifier
#' @param outputFile \code{character} a folder where the html report is saved
#' @param data \code{data.table} results from the clustering, output data from \link{clusteringTypicalDays}
#'
#'
#' @examples
#'
#' \dontrun{
#' 
#' clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
#' generateClusteringReport(dayType = 7, data = clusterTD)
#' 
#' }
#' @export
#' 
#' @import rmarkdown flexdashboard manipulateWidget gridExtra
#' @importFrom shiny tags
generateClusteringReport <- function(dayType, outputFile = NULL, data){
  
  if(is.null(outputFile)){
    outputFile <- getwd()
  }
  
  output_Dir <- outputFile
  outputFile <- paste0(outputFile, "/", gsub(":", "", gsub( " ", "_",as.character(Sys.time()))), "_flowBased_",dayType, "_",data[idDayType == dayType]$Class, ".html")
  e <- environment()
  e$dayType <- dayType
  e$data <- data
  
  matchingNameTable <- data.table(Class = c("interSaisonWe", "interSaisonSe", "winterWe", "winterSe",
                                            "summerWe", "summerSe"),
                                  title = c("inter-season weekend day", "inter-season working day",
                                            "winter weekend day", "winter working day",
                                            "summer weekend day", "summer working day"))
  
  
  CompTitle <- matchingNameTable$title[which(data[dayType]$Class == matchingNameTable$Class)]
  e$CompTitle <- CompTitle
  
  rmarkdown::render(system.file("/report/resumeclustflex.Rmd", package = "flowBasedClustering"),
                    output_file = outputFile,
                    params = list(set_title = paste0("Typical Day ", dayType,
                                                     " : ", CompTitle,  " (generated on ", Sys.Date(), ")")),
                    intermediates_dir = output_Dir, envir = e,
                    quiet = TRUE)
  outputFile
}



#' #' Generate a plot for a typical day cluster
#' #'
#' #' @param data \code{data.table}, output of \link{classifTypicalDay}
#' #' @param country1 \code{character}, name of first country
#' #' @param country2 \code{character}, name of second country
#' #' @param hour \code{numeric}, hour
#' #' @param dayType  \code{numeric}, dayType
#' #' @param typicalDayOnly : plot only typical day ?
#' #' @param ggplot : ggplot or amCharts ?
#' #'
#' #' @import rAmCharts
#' #'
#' #' @export
#' clusterPlot <- function(data, country1, country2, hour, dayType, 
#'                         typicalDayOnly = FALSE, ggplot = FALSE){
#'   dataPlot <- .getDataPlotClustering(data[idDayType==dayType],  country1, country2, hour)
#'   .makeGraph(dataPlot, data[idDayType==dayType]$TypicalDay, 
#'              typicalDayOnly = typicalDayOnly, ggplot = ggplot)
#' }
# clusterPlot(Myclassif, "FR", "DE", 8, 9)



#' Prepare data for plot
#'
#' @importFrom grDevices chull
#' @noRd
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
#' @noRd
.getDataPlotClustering <- function(allTypeDay, country1, country2, hour)
{
  data_plot <- apply(allTypeDay$dayIn[[1]][[1]][Period == hour], 1, function(data){
    ctry1 <- country1
    ctry2 <- country2
    dataChull <- .getChull(data$out, ctry1, ctry2)
    dataChull <- data.frame(dataChull)
    names(dataChull) <- c(paste0(data$Date, ctry1), paste0(data$Date, ctry2))
    round(data.table(dataChull), 0)
  })
  
  
  maxRow <- max(unlist(lapply(data_plot, nrow)))
  data_plot <- lapply(data_plot, function(X){
    rbind(X, data.table(rep(NA, maxRow-nrow(X)), rep(NA, maxRow-nrow(X))), use.names=FALSE)
  })
  data_plot <- cbind.data.frame(data_plot)
  data_plot
}

#' Graph function
#'
#'@param data : data.frame
#'@param typicalDayDate : date of typical day
#'@param typicalDayOnly : plot only typical day ?
#'@param ggplot : ggplot or amCharts ?
#'@param width \code{character}, for rAmCharts only. Default to "420px" (set to "100%" for dynamic resize)
#'@param height \code{character}, for rAmCharts only. Default to "410px" (set to "100%" for dynamic resize)
#' 
#'@noRd
.makeGraph <- function(data, typicalDayDate, typicalDayOnly = FALSE, 
                       ggplot = FALSE, width = "420px", height = "410px"){
  ctry <- unique(substr(names(data), 11, 12))
  if(typicalDayOnly){
    dates <- typicalDayDate
  } else {
    dates <- unique(substr(names(data), 1, 10))
  }
  
  xlim = c(-10000, 10000)
  ylim = c(-10000, 10000)
  
  if(max(data, na.rm = TRUE) <= 8000 & min(data, na.rm = TRUE) >= -8000){
    xlim = c(-8000, 8000)
    ylim = c(-8000, 8000)
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
      addValueAxes(title = paste(ctry[1], "(MW)"), position = "bottom", minimum = xlim[1], 
                   maximum = xlim[2], minHorizontalGap = 35, minVerticalGap = 35),
      addValueAxes(title =  paste(ctry[2], "(MW)"), minimum = ylim[1], 
                   maximum = ylim[2], minHorizontalGap = 35, minVerticalGap = 35),
      setExport(enabled = TRUE),
      plot(width = width, height = height)
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
    
    ggplot(data=gg_data, aes(x = eval(parse(text=ctry[1])), y = eval(parse(text=ctry[2])), 
                             group = date, colour = col, size = size, linetype = as.character(col))) + geom_path() +
      geom_point()  + scale_size(range=c(0.1, 2), guide=FALSE) + theme(legend.position="none") +
      # xlim(xlim[1], xlim[2]) + ylim(ylim[1], ylim[2]) + 
      ggtitle(paste0("Flow-based  clustering ", ctry[1], "/", ctry[2])) +
      theme(plot.title = element_text(hjust = 0.5)) + ylab(paste(ctry[2], "(MW)")) +
      xlab(paste(ctry[1], "(MW)")) + theme(panel.background = element_rect(fill = 'white', colour = 'black'),
                                           panel.grid.major = element_line(size = 0.5, linetype = 'dashed',
                                                                           colour = "grey")) +
      scale_y_continuous(breaks = seq(ylim[1], ylim[2], 2000), limits = ylim, expand = c(0, 0)) +
      scale_x_continuous(breaks = seq(xlim[1], xlim[2], 2000), limits = xlim, expand = c(0, 0))
    
    
  }
}



