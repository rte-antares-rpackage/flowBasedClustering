#' Convert a PTDF file into a table of vertices
#'
#' PTDF are the equations of the hyperplanes defining the limits of flow-based domains, 
#' while the vertices are the coordinnates of the extreme points of the domains.
#'
#' @param PTDF \code{character}, path of a PTDF file. 
#' The PTDF file must contains at least these seven columns : Date, Period, BE, DE, FR, NL and RAM - in this order.
#' It must be a .csv file with ";" as column separator and "." as decimal separator.
#' The names of the seven columns (Date, Period, ...) must be included in the header (first line)
#' of the .csv file.
#' The Period lies between 1 and 24 for the 24 hourly market periods. Therefore, period i corresponds to the time-step [i-1 ; i].
#' 
#' @param nbCore \code{numeric}, number of cores for parallel computation. Default to one.
#' @param maxDomainSize \code{numeric} limit of domain size in each axis. The function will return a warning if one domain
#' or more exceed these limits.
#' 
#' @return A data table with the vertices of the flow-based domains.
#'
#' @examples
#'
#' \dontrun{
#' 
#' ptdf_data <- data.table::fread(system.file("dataset/ptdf_example.csv", 
#'    package = "flowBasedClustering"), data.table = F)
#' 
#' head(ptdf_data)
#' 
#' vertices <- ptdfToVertices(system.file("dataset/ptdf_example.csv",
#'     package = "flowBasedClustering"))
#' 
#' head(vertices)
#' 
#' }
#'
#' @import data.table
#' @import pipeR
#' @import parallel
#'
#' @export
ptdfToVertices <- function(PTDF, nbCore = 1, maxDomainSize = 10000)
{
  isSupLim <- NULL
  # Load PTDF
  if(!file.exists(PTDF)){
    stop(paste0(PTDF, " file not found"))
  }
  
  PTDF <- fread(PTDF)
  
  
  if("RAM_0" %in% names(PTDF)){
    setnames(PTDF, "RAM_0", "RAM")
  }
  
  # Control PTFD format
  if(any(names(PTDF)[1:7] != c("Date", "Period", "BE", "DE", "FR", "NL", "RAM"))){
    stop(paste0("Names of ptdf file must be : Date, Period, BE, DE, FR, NL, RAM currently : ",
                paste0(names(PTDF)[1:7] , collapse = ", ")))
  }
  
  PTDF <- .ctrlVertices(PTDF)
  

  #calcul vertices from PTDF function
  calcPoly <- function(X, PTDF){
    data.table::rbindlist(lapply(1:24, function(Y, PTDF){
      resSel <- PTDF[Date == X & Period == Y]
      if(nrow(resSel)>0)
      {
        out <- getVertices(as.matrix(resSel[, .SD, .SDcols = c("BE", "DE", "FR", "NL")]),
                           resSel$RAM)
        out <- data.table(out)
        names(out) <- c("BE", "DE", "FR")
        data.table(Date = X, Period = Y, out)
      }else{
        NULL
      }
    }, PTDF = PTDF))
  }
  
  #Not parallel
  if(nbCore == 1)
  {
    vertices <- data.table::rbindlist(lapply(unique(PTDF$Date), calcPoly, PTDF = PTDF))
  }else{
    # parallel
    cl <- parallel::makeCluster(nbCore)
    e = new.env()
    e$PTDF <- PTDF
    e$calcPoly = calcPoly
    clusterExport(cl, c("PTDF", "calcPoly"), envir = e)
    rm(e)
    
    clusterEvalQ(cl, {
      library(data.table)
      #library(geometry)
      #library(rgl)
      #library(Rvcg)
      library(pipeR)
      library(flowBasedClustering)
    })
    
    vertices <- data.table::rbindlist(parLapplyLB(cl, unique(PTDF$Date), function(X){
      calcPoly(X,
               PTDF = PTDF
      )}))
    stopCluster(cl)
    
  }
  
  
  Max <- vertices[,max(unlist(.SD)), by = c("Date", "Period"), .SDcols = 3:ncol(vertices)]
  Max[, isSupLim := V1 > maxDomainSize]
  Max <- Max[Max$isSupLim]
  if(nrow(Max) > 0){
    Max <- Max[,list(list(Period)), by = "Date"]
   
    warning( paste("The following flow-based domains exceed the expected maximum size :", 
                   paste(Max$Date, "(hour : ",lapply(Max$V1, function(X){paste(X, collapse = ", ")}),")", collapse = ", ")))
  }
  
  vertices
}
