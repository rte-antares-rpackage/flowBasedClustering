#' @title Generate a set of flow-based typical days for a season
#' 
#' @description To do
#'
#' @param dates \code{vector}, vector of date.
#' @param vertices \code{data.table}, 5 columns :
#' \itemize{
#'  \item Date : date (%Y-%M-%D)
#'  \item Period : Hour (1:24)
#'  \item BE : belgium vertices
#'  \item DE : german vertices
#'  \item FR : french vertices
#' }
#' This parameter can be obtained with the function \code{ptdfToVertices()}.
#' @param nbCluster \code{numeric}, number of clusters.
#' @param className \code{character}, name of class.
#' @param id_start \code{numeric}, first id of typical days.
#' @param reportPath \code{character}, path where the report is written. Defaut to \code{getwd()}
#' @param hourWeight \code{numeric}, vector of 24 weights to ponderate differently each hour of the day
#' @param report \code{boolean}, if TRUE, reports are generated.
#'
#' @examples
#'
#' \dontrun{
#' library(data.table) 
#' vertices <- fread(system.file("dataset/vertices_example.txt",package = "flowBasedClustering"))
#' dates <- seq(as.Date("2016-08-09"), as.Date("2018-09-01"), by = "day")
#' hourWeight = rep(1, 24)
#' 
#' clusterTypicalDaysForOneClass(vertices = vertices,
#'                               dates = dates, nbCluster = 2,
#'                               className = "myName", id_start = 5)
#' }
#'
#'
#' @export
clusterTypicalDaysForOneClass <- function(dates, 
                                          vertices, 
                                          nbCluster,
                                          hourWeight = rep(1, 24),
                                          className = NULL,
                                          reportPath = getwd(),
                                          report = TRUE, 
                                          id_start = 1){
  
  cat("Compute distances\n")
  
  pb <- txtProgressBar(style = 3)
  setTxtProgressBar(pb, 0)
  
  vertices <- .ctrlVertices(vertices)
  
  dates <- as.character(dates)
  .ctrlDates(dates, unique(vertices$Date))
  
  #control if the format of the vertices file is good
  .ctrlVerticesFormat(vertices)
  
  .ctrlWeight(hourWeight)
  
  #Compute mesh from vertices
  vertices <- .computeMesh(vertices)
 
  
  distMat <- .getDistMatrix(veticesSel <- vertices[Date %in% dates], hourWeight)
  
  set.seed(123456)
  vect <- cluster::pam(distMat, nbCluster, diss = TRUE)$clustering
  distMat <- as.matrix(distMat)
  
  if(is.null(className)){
    className <- as.character("Class")
  }
  
  allTypDay <- rbindlist(sapply(1:nbCluster, function(X){
    # Found a representative day for each class
    .getDataAndMakeOutput(X, vect, distMat, className)

  }, simplify = FALSE))
  
  allTypDay <- .addVerticesToTp(allTypDay, vertices)
  
  nb <- id_start:(id_start+nrow(allTypDay)-1)
  
  
  allTypDay[,idDayType :=nb]
  setTxtProgressBar(pb, 1)

  
  # report generation
  if(report){
    cat("\n")
    cat("Write report(s)\n")
    pb <- txtProgressBar(style = 3)
    setTxtProgressBar(pb, 0)
    outL <- .crtOutFile(allTypDay, reportPath)
    
    sapply(allTypDay$idDayType, function(X){
      setTxtProgressBar(pb, getTxtProgressBar(pb) + 1/(outL$step + 1))
      generateClusteringReport(X, data = allTypDay, outputFile = outL$outputFile)
    })
    
    saveRDS(allTypDay, paste0(outL$outputFile, "/resultClust.RDS"))
    setTxtProgressBar(pb, 1)
  }

  allTypDay
}

.crtOutFile <- function(allTypDay, reportPath){
  outputFile <- reportPath
  if(is.null(outputFile)){
    outputFile <- getwd()
  }
  direName <-  as.character(Sys.time())
  direName <- gsub(" ", "", gsub( ":", "",direName))
  reportDir <- paste0(outputFile, "/fb-clustering-", direName)
  dir.create(reportDir)
  outputFile <- reportDir
  step <- length(allTypDay$idDayType)
  list(outputFile = outputFile, step = step)
}

.ctrlDates <- function(dates, dayInVertices){
  if(!any(dates%in%dayInVertices)){
    stop("Some(s) season(s) are not in vertices data.")
  }
  
  if(!all(dates%in%dayInVertices)){
    warning("Somes dates in calendar are not in vertices data.")
  }
  
  if(length(dates) < 2){
    stop("Clustering cannot be performed when class(season/type of day) contains less than 2 days")
  }
  
}