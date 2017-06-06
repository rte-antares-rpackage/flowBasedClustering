#' Generate clusturing of typical day
#'
#' @param calendar \code{list}, vector of date in eatch period can be obtain which \link{cutYear}
#' @param vertices \code{data.table}, 5 column :
#' \itemize{
#'  \item Date : date (%Y-%M-%D)
#'  \item Period : Hour, (1:24)
#'  \item BE : Belgium vertices
#'  \item DE : deutschland vertices
#'  \item FR : french vertices
#' }
#' @param nbClustWeek \code{numeric} number of cluster for week period
#' @param nbClustWeekend \code{numeric} number of cluster for weekend period
#'
#' @examples
#'
#' \dontrun{
#' library(data.table)
#' vertices <- fread(system.file("dev/verticesAllDay.txt",package = "flowBasedClustering"))
#' dates <- generateAllDate("2015-11-01", "2017-01-20")
#' interSeasonBegin <- c("2016-03-01", "2016-10-01")
#' interSeasonEnd <- c("2016-05-15", "2016-10-31")
#' dayInWeekend = c(6, 7)
#' calendar <- cutYear(dates, interSeasonBegin, interSeasonEnd)
#' myClassif <- classifTypicalDay(calendar, vertices)
#' }
#'
#'
#' @export
classifTypicalDay <- function(calendar, vertices, nbClustWeek = 3, nbClustWeekend = 1,
                              report = TRUE){

  getMesh <- function(out){
    tc <- geometry::delaunayn(out, full = F)
    # tetramesh(tc,out,alpha=0.7)
    # tri
    tc_tri <- geometry::surf.tri(out, tc)
    # rajout d'une colonne 1
    out2 <- cbind(out, rep(1, nrow(out)))
    mesh <- rgl::tmesh3d(as.vector(t(out2[as.vector(t(tc_tri)),])),1:(nrow(tc_tri)*3))
    list(mesh)
  }

  vertices <- vertices[,
                       list(out = list(cbind(BE, DE, FR))), by = c("Date", "Period")]
  vertices[,mesh := list(getMesh(out[[1]])),by = c("Date", "Period") ]

  We <- rep(FALSE, length(calendar))
  We[grep("We", names(calendar))] <- TRUE
  #Apply classification for eatch period in calendar
  allTypDay <- rbindlist(apply(  data.table(calendar, We), 1, function(season){
    nbClust = ifelse(season$We, nbClustWeekend, nbClustWeek)
    veticesSel <- vertices[Date %in% as.character(season$calendar)]
    #get distance for eatch  day pairs
    ditMat <- .getDistMatrix(veticesSel)

    #Methode avec hclust
    #vect <- cutree(hclust(ditMat), nbClust)

    #Methode avec PAM
    vect <- pam(ditMat, nbClust, diss = TRUE)$clustering


    ditMat <- as.matrix(ditMat)
    typicalDay <- rbindlist(sapply(unique(vect), function(X){
      #Found a representativ day for eatch class
      dateIn <- names(vect[which(vect == X)])
      colSel <- row.names(ditMat)%in%dateIn
      data.table(TypicalDay = names(which.min(rowSums(ditMat[colSel, colSel]))),
                 dayIn = list(data.table(Date = rep(dateIn, each = 24), Period = rep(1:24, length(dateIn)))))


    }, simplify = FALSE))


    typicalDay
  }))
  for(i in 1:nrow(allTypDay))
  {
    allTypDay$dayIn[[i]] <- list(merge(allTypDay$dayIn[[i]], vertices[,.SD, .SDcols = 1:3], by = c("Date", "Period")))
  }
  allTypDay[,idDayType :=1:.N ]

  if(report){
    sapply(allTypDay$idDayType, function(X){
      generateRaportClustering(X, data = allTypDay)})
  }
  allTypDay

}



#' Compute distance matrix
#'
#' @param vertices \code{data.table}, 7 column :
#' \itemize{
#'  \item Date : date (%Y-%M-%D)
#'  \item Period : Hour, (1:24)
#'  \item BE : Belgium vertices
#'  \item DE : deutschland vertices
#'  \item FR : french vertices
#' }
#'
#'
.getDistMatrix <- function(vertices)
{
  res_hour <- data.table(t(combn(unique(vertices$Date), 2)))
  ditMat <- data.table::rbindlist(lapply(1:nrow(res_hour), function(comb){
    date_1 <- res_hour[comb, V1]
    date_2 <- res_hour[comb, V2]
    v_hours <- intersect(vertices[Date%in% date_1, Period], vertices[Date%in% date_2, Period])
    hourDist <- data.table::rbindlist(lapply(v_hours, function(h){
      # a voir si on somme tout / positifs / negatifs, ...


      x_on_y_All <- vcgClost(vertices[Date%in% date_1 & Period %in% h, ]$out[[1]],
                             vertices[Date%in% date_2 & Period %in% h, ]$mesh[[1]],
                             borderchk = TRUE,
                             sign = TRUE, facenormals = TRUE, barycentric = TRUE)

      x_on_y <- x_on_y_All$quality

      #Si on veux filtrer les points dans les domaines
      # PTDFTp <- PTDF[Date == date_2 & Period == h]
      # x_on_y[which(apply(cbind(vertices[Date%in% date_1 & Period %in% h, ]$out[[1]], -rowSums(vertices[Date%in% date_1 & Period %in% h, ]$out[[1]])), 1, function(X){
      #   all(t(X)%*%
      #          t(as.matrix(PTDFTp[,.SD, .SDcols = c("BE", "DE", "FR", "NL")]))<= PTDFTp$RAM_0)}))] <- 0

      y_on_x_All <- vcgClost(vertices[Date%in% date_2 & Period %in% h, ]$out[[1]],
                             vertices[Date%in% date_1 & Period %in% h, ]$mesh[[1]], borderchk = TRUE, sign = TRUE)


      y_on_x <- y_on_x_All$quality
      #Si on veux filtrer les points dans les domaines
      # PTDFTp <- PTDF[Date == date_1 & Period == h]
      # y_on_x[which(apply(cbind(vertices[Date%in% date_2 & Period %in% h, ]$out[[1]], -rowSums(vertices[Date%in% date_2 & Period %in% h, ]$out[[1]])), 1, function(X){
      #   all(t(X)%*%
      #         t(as.matrix(PTDFTp[,.SD, .SDcols = c("BE", "DE", "FR", "NL")]))<= PTDFTp$RAM_0)}))] <- 0

      d <- mean(x_on_y^2) + mean(y_on_x^2)
      d
      data.table(Date1 = c(date_1, date_2), Date2 = c(date_2, date_1), Period = h, dist = d)
    }))
    hourDist <- hourDist[, sqrt(sum(dist)), by = c("Date1", "Date2")]
    setnames(hourDist, "V1", "dayDist")
    hourDist
  }))

  ditMat <- dcast(ditMat, Date1~Date2,  value.var= "dayDist")
  ditMat <- ditMat[,.SD, .SDcols = 2:ncol(ditMat)]
  diag(ditMat) <- 0
  as.dist(ditMat)
}



