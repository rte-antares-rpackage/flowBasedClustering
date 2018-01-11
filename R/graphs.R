#' @title Plot a flow-based domain of a typical day
#' 
#' @description For a given hour, plot the flow-based domain of one typical day along with the other
#' flow-based domains it represents (i.e. other domains of the same cluster).
#'
#' @param data \code{data.table} results from the clustering, output data from \link{clusteringTypicalDays}
#' @param country1 \code{character}, name of the country whose net position is in the x axis (BE, FR, DE or NL)
#' @param country2 \code{character}, name of the country whose net position is in the y axis (BE, FR, DE or NL)
#' @param hour \code{numeric}, hour of the plotted domain
#' @param dayType  \code{numeric}, typical flow-based day identifier
#' @param typicalDayOnly \code{logical} if TRUE, plot only the domain of the typical day and not the other domains of the cluster
#' @param ggplot \code{logical} should ggplot package be used (static graph) instead of rAmCharts (dynamic graph)
#' @param width \code{character}, for rAmCharts only. Default to "420px" (set to "100/100" for dynamic resize)
#' @param height \code{character}, for rAmCharts only. Default to "410px" (set to "100/100" for dynamic resize)
#' 
#' @import rAmCharts DT backports
#'
#' @examples
#'
#' \dontrun{
#'
#' # classification result
#' clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
#'
#' clusterPlot(clusterTD, "FR", "DE", 8, 9, FALSE, FALSE)
#' clusterPlot(clusterTD, "FR", "DE", 8, 9, FALSE, TRUE)
#' clusterPlot(clusterTD, "FR", "DE", 8, 9, TRUE, TRUE)
#' clusterPlot(clusterTD, "FR", "DE", 8, 9, TRUE, FALSE)
#'
#' }
#' @import ggplot2
#' 
#' @export
clusterPlot <- function(data, country1, country2, hour, dayType,
                        typicalDayOnly = FALSE, ggplot = FALSE, width = "420px", height = "410px"){
  dataPlot <- .getDataPlotClustering(data[idDayType==dayType],  country1, country2, hour)
  .makeGraph(dataPlot, data[idDayType==dayType]$TypicalDay,
             typicalDayOnly = typicalDayOnly, ggplot = ggplot, width = width, height = height)
}









#' Plot flow-based domain(s)
#'
#' @param PTDF \code{data.frame or list} (list of) data.frame(s) with the PTDF of one time step. 
#' The data.frame must have the same columns as the input file of \link{ptdfToVertices}
#' @param country1 \code{character}, name of the country whose net position is in the x axis (BE, FR, DE or NL)
#' @param country2 \code{character}, name of the country whose net position is in the y axis (BE, FR, DE or NL)
#' @param domainsNames \code{character} names of the domain(s), used as legend of the graph
#' @param main \code{character} title of the graph
#' @param xlim \code{numeric} x axis limits, vector with two values (min and max)
#' @param ylim \code{numeric} y axis limits, vector with two values (min and max)
#' @param width \code{character}, for rAmCharts only. Default to "420px" (set to "100/100" for dynamic resize)
#' @param height \code{character}, for rAmCharts only. Default to "410px" (set to "100/100" for dynamic resize)
#' 
#' @examples
#'
#' \dontrun{
#' PTDF <- fread(system.file("dataset/ptdf_example.csv",package = "flowBasedClustering"))
#' #Plot unique PTDF
#' PTDF1 <- PTDF[ Date == "06/08/2016" & Period == 1]
#' plotFlowbased(PTDF1, country1 = "DE", country2 = "BE", domainsNames = "06/08/2016")
#'
#' PTDF2 <- list(PTDF[ Date == "06/08/2016" & Period == 1],
#' PTDF[ Date == "06/08/2016" & Period == 2],
#' PTDF[ Date == "06/08/2016" & Period == 3])
#' plotFlowbased(PTDF2, country1 = "DE", country2 = "BE", domainsNames = c("01/11/2015 - H1",
#' "01/11/2015 - H2", "01/11/2015 - H3"), main = "Myplot")
#'
#' }
#'
#' @export
plotFlowbased <- function(PTDF,
                          country1,
                          country2,
                          domainsNames = NULL,
                          main = "",
                          xlim = c(-10000, 10000),
                          ylim = c(-10000, 10000), 
                          width = "420px", height = "410px"){
  #Generate data for plot
  givePlotData <- function(PTDF, country1, country2){
    PTDF <- data.table(PTDF)
    if("RAM_0" %in% names(PTDF))setnames(PTDF, "RAM_0", "RAM")
    
    vertices <- getVertices(as.matrix(PTDF[, .SD, .SDcols = c("BE", "DE", "FR", "NL")]),
                            PTDF$RAM)
    vertices <- data.frame(vertices)
    names(vertices) <- c("BE", "DE", "FR")
    res <- .getChull(vertices, country1, country2)
    res <- data.frame(res)
    res <- round(res, 1)
    names(res) <- c(country1, country2)
    res
  }

  #Control arguments
  multiPDTF <- all(class(PTDF ) == "list")
  if(!is.null(domainsNames)){
    if(!multiPDTF){
      if(length(domainsNames) != 1){
        stop("Only one PTDF specify for 2 or more domainsNames")
      }
    }else{
      if(length(domainsNames) != length(PTDF)){
        stop(paste0("You must have one domainsNames specify by element of PDTF list currently you have ",
                    length(domainsNames), " domainsNames specify for ", length(PTDF), " PTDF"))
      }
    }
  }
  if(is.null(domainsNames)){
    if(!multiPDTF){
      domainsNames <- "D1"
    }else{
      domainsNames <- paste0("D", 1:length(PTDF))
    }
  }

  #Apply function to generate data
  if(!multiPDTF){
    dataToGraph <- givePlotData(PTDF, country1, country2)
    names(dataToGraph) <- paste0(domainsNames, names(dataToGraph))
  }else{
    dataToGraph <- sapply(1:length(PTDF), function(increm){
      dta <- PTDF[[increm]]
      dttp <- givePlotData(dta, country1, country2)
      names(dttp) <- paste0(domainsNames[increm], names(dttp))
      dttp
    }, USE.NAMES = FALSE, simplify = FALSE)
    rowMax <- max(unlist(lapply(dataToGraph, nrow)))
    dataToGraph <- lapply(dataToGraph, function(dta){
      if(nrow(dta)<rowMax){
        Na <-  data.frame(rep(NA,rowMax - nrow(dta)),
                          rep(NA,rowMax - nrow(dta)))
        names(Na) <- names(dta)
        rbind(dta,Na)
      }else{
        dta
      }
    })
    dataToGraph <- do.call(cbind, dataToGraph)}

  #Graph creation for more exmples see rAmCharts::runExamples()
  graphs <- sapply(1:length(domainsNames), function(X){
    amGraph(title = domainsNames[X], balloonText =
              paste0('<b>',domainsNames[X],'<br>', paste0(domainsNames[X], country1), '</b> :[[x]] <br><b>',paste0(domainsNames[X], country2), '</b> :[[y]]'),
            bullet = 'circle', xField = paste0(domainsNames[X], country1),yField = paste0(domainsNames[X], country2),
            lineAlpha = 1, bullet = "bubble", bulletSize = 4,
            lineThickness = 3)

  }, USE.NAMES = FALSE)
  pipeR::pipeline(
    amXYChart(dataProvider = dataToGraph),
    addTitle(text = main),
    setGraphs(graphs),
    setChartCursor(),
    addValueAxes(title = paste(country1, "(MW)"), position = "bottom", minimum = xlim[1], 
                 maximum = xlim[2], minHorizontalGap = 35, minVerticalGap = 35),
    addValueAxes(title =  paste(country2, "(MW)"), minimum = ylim[1], 
                 maximum = ylim[2], minHorizontalGap = 35, minVerticalGap = 35),
    setExport(enabled = TRUE),
    setLegend(enabled = TRUE),
    plot(width = width, height = height)
  )

}
