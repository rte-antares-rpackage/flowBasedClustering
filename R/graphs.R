#' Generate html report
#'
#' @param dayType \code{numeric} Typical day
#' @param outputPath \code{character} path of output
#' @param data \code{data.table} output of \link{clusteringTypicalDays}
#'
#'
#' @examples
#'
#' \dontrun{
#' # classification result
#' clusterTD <- readRDS(system.file("dev/ClassifOut.RDS",package = "flowBasedClustering"))
#'
#' generateClusteringReport(dayType = 7, data = clusterTD)
#'
#' }
#'
#' @export
#'
#' @import rAmCharts
#' @import pipeR
#' @import ggplot2
#'
#'
generateClusteringReport <- function(dayType, outputPath = NULL,
                             data = NULL){

  # If data are not specify by user, load them from package default file
  if(is.null(data))
  {
    data <- readRDS(system.file("dev/ClassifOut.RDS",package = "flowBasedClustering"))
  }

  if(is.null(outputPath)){
    outputPath <- getwd()
  }
  #Create an environment for the execution of markdown
  output_Dir <- outputPath
  outputPath <- paste0(outputPath, "/", "FlowBased_clustering", dayType, "_", Sys.Date(), ".html")
  e <- environment()
  e$dayType <- dayType
  e$data <- data

  #report creation
  rmarkdown::render(system.file("/report/resumeclustflex.Rmd", package = "flowBasedClustering"),
                    output_file = outputPath,
                    params = list(set_title = paste0("Typical Day ", dayType, " (generated on ", Sys.Date(), ")")),
                    intermediates_dir = output_Dir, envir = e,
                    quiet = TRUE)
}



#' Generate a plot for a typical day cluster
#'
#' @param data \code{data.table}, output of \link{clusteringTypicalDays}
#' @param country1 \code{character}, name of first country
#' @param country2 \code{character}, name of second country
#' @param hour \code{numeric}, hour
#' @param dayType  \code{numeric}, dayType
#' @param typicalDayOnly : plot only typical day ?
#' @param ggplot : ggplot or amCharts ?
#'
#' @import rAmCharts
#'
#' @examples
#'
#' \dontrun{
#'
#' # classification result
#' clusterTD <- readRDS(system.file("dev/ClassifOut.RDS",package = "flowBasedClustering"))
#'
#' clusterPlot(clusterTD, "FR", "DE", 8, 9, FALSE, FALSE)
#' clusterPlot(clusterTD, "FR", "DE", 8, 9, FALSE, TRUE)
#' clusterPlot(clusterTD, "FR", "DE", 8, 9, TRUE, TRUE)
#' clusterPlot(clusterTD, "FR", "DE", 8, 9, TRUE, FALSE)
#'
#' }
#'
#' @export
clusterPlot <- function(data, country1, country2, hour, dayType,
                        typicalDayOnly = FALSE, ggplot = FALSE){
  dataPlot <- .getDataPlotClustering(data[idDayType==dayType],  country1, country2, hour)
  .makeGraph(dataPlot, data[idDayType==dayType]$TypicalDay,
             typicalDayOnly = typicalDayOnly, ggplot = ggplot)
}

#' Prepare data for plot
#'
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
#' @param data : data.frame
#' @param typicalDayDate : date of typical day
#' @param typicalDayOnly : plot only typical day ?
#' @param ggplot : ggplot or amCharts ?
#'
#' @noRd
#'
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



#' Plot flow-based domain(s)
#'
#' @param PTDF \code{data.frame or list} PTDF file
#' @param country1 \code{character} country 1 possible values are : BE, DE, FR, NL
#' @param country2 \code{character} country 2 possible values are : BE, DE, FR, NL
#' @param domainsNames \code{character} names for domain(s), use in legend
#' @param main \code{character} title
#'
#' @examples
#'
#' \dontrun{
#' PTDF <- fread(system.file("dev/data/faceAllYear.csv",package = "flowBasedClustering"))
#' #Plot unique PTDF
#' PTDF1 <- PTDF[ Date == "01/11/2015" & Period == 1]
#' plotFlowbased(PTDF1, country1 = "DE", country2 = "BE", domainsNames = "01/11/2015")
#'
#' PTDF2 <- list(PTDF[ Date == "01/11/2015" & Period == 1],
#' PTDF[ Date == "01/11/2015" & Period == 2],
#' PTDF[ Date == "01/11/2015" & Period == 3])
#' plotFlowbased(PTDF2, country1 = "DE", country2 = "BE", domainsNames = c("01/11/2015 - H1",
#' "01/11/2015 - H2", "01/11/2015 - H3"), main = "Myplot")
#'
#' }
#'
#' @export
plotFlowbased <- function(PTDF,  country1, country2, domainsNames = NULL, main = ""){
  #Generate data for plot
  givePlotData <- function(PTDF, country1, country2){
    PTDF <- data.table(PTDF)
    vertices <- getVertices(as.matrix(PTDF[, .SD, .SDcols = c("BE", "DE", "FR", "NL")]),
                            PTDF$RAM_0)
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
    addValueAxes(title = paste(country1, "(MW)"), position = "bottom", minimum = -8000, maximum = 8000),
    addValueAxes(title =  paste(country2, "(MW)"), minimum = -8000, maximum = 8000),
    setExport(enabled = TRUE),
    setLegend(enabled = TRUE)
  )

}
