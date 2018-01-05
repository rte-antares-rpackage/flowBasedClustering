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
  pb <- txtProgressBar(style = 3)
  
  setTxtProgressBar(pb, 0)
  

  
  vertices <- .ctrlVertices(vertices)
  
  dayInVertices <- unique(vertices$Date)
  
  if(!any(dates%in%dayInVertices)){
    stop("Some(s) season(s) are not in vertices data.")
  }
  
  if(!all(dates%in%dayInVertices)){
    warning("Somes dates in calendar are not in vertices data.")
  }
  
  #control if the format of the vertices file is good
  .ctrlVerticesFormat(vertices)
  
  .ctrlWeight(hourWeight)
  
  #Compute mesh from vertices
  vertices <- .computeMesh(vertices)
  
  if(length(dates) < 2){
    stop("A distance cant be compute when season contains less than 2 days")
  }
  
  veticesSel <- vertices[Date %in% dates]
  veticesSel$Date <- as.character(veticesSel$Date)
  
  distMat <- .getDistMatrix(veticesSel, hourWeight)
  vect <- cluster::pam(distMat, nbCluster, diss = TRUE)$clustering
  distMat <- as.matrix(distMat)
  
  if(is.null(className)){
    className <- as.character("Class")
  }
  
  allTypDay <- rbindlist(sapply(1:nbCluster, function(X){
    # Found a representative day for each class
    dateIn <- names(vect[which(vect == X)])
    colSel <- row.names(distMat)%in%dateIn
    #detect day closed to middle of cluster
    if(length(dateIn) > 1)
    {
      minDay <- which.min(rowSums(distMat[, colSel]))
      distINfo <- distMat[minDay,colSel]
      data.table(TypicalDay = names(minDay),
                 Class = className,
                 dayIn = list(data.table(Date = rep(dateIn, each = 24), Period = rep(1:24, length(dateIn)))),
                 distance = list(data.table(Date = dateIn, Distance = distINfo)))
    }
    # case where cluster is of size one :
    else
    {
      minDay <- dateIn
      distINfo <- 0
      data.table(TypicalDay = minDay,
                 Class = className,
                 dayIn = list(data.table(Date = rep(dateIn, each = 24), Period = rep(1:24, length(dateIn)))),
                 distance = list(data.table(Date = dateIn, Distance = distINfo)))
    }
  }, simplify = FALSE))
  
  vertices$Date <- as.character(vertices$Date)
  
  for(i in 1:nrow(allTypDay)){
    allTypDay$dayIn[[i]] <- list(merge(allTypDay$dayIn[[i]], vertices[,.SD, .SDcols = 1:3], by = c("Date", "Period")))
  }
  
  nb <- id_start:(id_start+nrow(allTypDay)-1)
  allTypDay[,idDayType :=nb]
  setTxtProgressBar(pb, 0.5)
  
  # report generation
  if(report){
    outL <- .crtOutFile(allTypDay, reportPath)
    
    sapply(allTypDay$idDayType, function(X){
      setTxtProgressBar(pb, getTxtProgressBar(pb) + 1/(outL$step + 1))
      generateClusteringReport(X, data = allTypDay, outputFile = outL$outputFile)
    })
    
    
    saveRDS(allTypDay, paste0(outL$outputFile, "/resultClust.RDS"))
  }
  setTxtProgressBar(pb, 1)
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
  step <- length(allTypDay$idDayType)*2
  list(outputFile = outputFile, step = step)
}
