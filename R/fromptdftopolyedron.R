#' Transforme a PTDF file to vertives file
#'
#' @param PTDF \code{character}, path for PTDF file
#' @param nbCors \code{numeric}, use parallelisation or not.Indicate number of cores use. If 1 (default) a no parallel.
#'
#' @examples
#'
#' \dontrun{
#' fromPtdfToVertices(nbCors = 4)
#' }
#'
#' @import data.table
#' @import geometry
#' @import rgl
#' @import Rvcg
#' @import pipeR
#' @import parallel
#'
#' @export
fromPtdfToVertices <- function(PTDF = system.file("data/faceAllYear.csv",package = "flowBasedClustering"),
                               nbCors = 1)
{
  PTDF <- fread(PTDF)


  calcPoly <- function(X, PTDF){
    data.table::rbindlist(lapply(1:24, function(Y, PTDF){
      resSel <- PTDF[Date == X & Period == Y]
      if(nrow(resSel)>0)
      {
        out <- getVertices(as.matrix(resSel[, .SD, .SDcols = c("BE", "DE", "FR", "NL")]),
                           resSel$RAM_0)

        # # triangulation
        # tc <- geometry::delaunayn(out, full = F)
        # # tetramesh(tc,out,alpha=0.7)
        # # tri
        # tc_tri <- geometry::surf.tri(out, tc)
        # # rajout d'une colonne 1
        # out2 <- cbind(out, rep(1, nrow(out)))
        # mesh <- rgl::tmesh3d(as.vector(t(out2[as.vector(t(tc_tri)),])),1:(nrow(tc_tri)*3))
        # # wire3d(mesh)
        #
        # data.table(Date = X, Period = Y,  out = list(out), mesh = list(mesh))
        out <- data.table(out)
        names(out) <- c("BE", "DE", "FR")
        data.table(Date = X, Period = Y, out)
      }else{
        NULL
      }
    }, PTDF = PTDF))
  }

  if(nbCors == 1)
  {
    vertices <- data.table::rbindlist(lapply(unique(PTDF$Date)[1:3], function(X){
      calcPoly(X,
               PTDF = PTDF
      )}, PTDF = PTDF))
  }else{

    cl <- parallel::makeCluster(nbCors)
    e = new.env()
    e$PTDF <- PTDF
    e$calcPoly = calcPoly
    clusterExport(cl, c("PTDF", "calcPoly"), envir = e)
    rm(e)

    clusterEvalQ(cl, {
      library(data.table)
      library(geometry)
      library(rgl)
      library(Rvcg)
      library(pipeR)
      library(flowBasedClustering)
    })

    vertices <- data.table::rbindlist(parLapplyLB(cl, unique(PTDF$Date)[1:3], function(X){
      calcPoly(X,
               PTDF = PTDF
      )}))
  }
  stopCluster(cl)
  vertices
}
