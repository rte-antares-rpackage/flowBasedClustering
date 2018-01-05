#' Transform a PTDF file to vertices file
#'
#' @param PTDF \code{character}, path for PTDF file. This file must have 7 obligatory columns : Id_day, Period, BE, DE, FR, NL, RAM
#' @param nbCore \code{numeric}, use parallel computing or not. Indicate number of cores use. If 1 (default), no parallel.
#'
#' @examples
#'
#' \dontrun{
#' ptdfToVertices(nbCore = 4)
#' }
#'
#' @import data.table
# @import geometry
# @import rgl 
# @import Rvcg
#' @import pipeR
#' @import parallel
#'
#' @export
ptdfToVertices <- function(PTDF = system.file("dataset/ptdf_example.csv",package = "flowBasedClustering"),
                           nbCore = 1)
{
  
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
  
  vertices
}
