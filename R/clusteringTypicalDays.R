#' 
#' @title Generate a set of flow-based typical days
#' @description 
#'
#' @param calendar \code{list}, vector of date for each period. Can be obtain with \link{getCalendar}
#' @param vertices \code{data.table}, 5 columns :
#' \itemize{
#'  \item Date : date (%Y-%M-%D)
#'  \item Period : Hour (1:24)
#'  \item BE : belgium vertices
#'  \item DE : german vertices
#'  \item FR : french vertices
#' }
#' This parameter can be obtained with the function \code{ptdfToVertices()}.
#' @param nbClustWeek \code{numeric}, number of clusters for week (working days) period. Defaut to 3
#' @param nbClustWeekend \code{numeric}, number of clusters for weekend period. Defaut to 1
#' @param report \code{boolean}, generate report. Defaut to TRUE
#' @param reportPath \code{character}, path where the report is written. Defaut to \code{getwd()}
#' @param hourWeigth \code{numeric}, vector of 24 weights to ponderate differently each hour of the day
#'
#' @examples
#'
#' \dontrun{
#' library(data.table)
#' vertices <- fread(system.file("dev/verticesAllDay.txt",package = "flowBasedClustering"))
#'
#' dates <- getSequence("2015-11-01", "2017-01-20")
#' interSeasonBegin <- c("2016-03-01", "2016-10-01")
#' interSeasonEnd <- c("2016-05-15", "2016-10-31")
#' calendar <- getCalendar(dates, interSeasonBegin, interSeasonEnd)
#'
#' clusterTD <- clusteringTypicalDays(calendar, vertices)
#' }
#'
#'
#' @export
#'
#' @importFrom cluster pam
#' @importFrom utils txtProgressBar getTxtProgressBar setTxtProgressBar
#'
clusteringTypicalDays <- function(calendar, vertices, nbClustWeek = 3, nbClustWeekend = 1,
                                  report = TRUE, reportPath = getwd(),
                                  hourWeigth = rep(1, 24)){
  
  pb <- txtProgressBar(style = 3)
  
  setTxtProgressBar(pb, 0)
  
  #control if the format of the vertices file is good
  if(any(names(vertices) != c("Date", "Period", "BE", "DE", "FR"))){
    stop(paste0("Names of vertices must be 'Date', 'Period', 'BE', 'DE', 'FR', currently : ",
                paste0(names(vertices), collapse = ", ")))
  }
  if(!is.character(vertices$Date)){
    vertices$Date <- as.character(vertices$Date)
  }
  unVerticeDate <- unique(vertices$Date)
  testIsDate <- try(as.Date(unVerticeDate), silent = TRUE)
  if( class( testIsDate ) == "try-error" || is.na( testIsDate ) ){
    stop( "Date column must have this format : %Y-%m-%d ?as.Date for help")
  } 
  
  
  #control Weigth
  if(length(hourWeigth)!=24){
    stop("Length of hourWeigth must be 24")
  }
  
  #control names of calendar
  if(any(!names(calendar)%in% c("interSeasonWe",
                                "interSeasonWd",
                                "winterWe",
                                "winterWd",
                                "summerWe","summerWd")) | is.null(names(calendar))){
    stop("Names of calendar must be 'interSeasonWe', 'interSeasonWd', 'winterWe', 'winterWd', 'summerWe', 'summerWd'")
  }
  
  sapply(names(calendar), function(X){
    if(sum(unVerticeDate%in%as.character(calendar[[X]])) == 0){
      stop(paste0("Intersection between season ", X, "(calendar) and vertices$Date is empty. This job cant be run"))
    }
  })
  
  
  
  # generate mesh for each polyhedron, mesh is an object use to calculate distance between polyhedron
  vertices <- vertices[, list(out = list(cbind(BE, DE, FR))), by = c("Date", "Period")]
  vertices[, mesh := list(.getMesh(out[[1]])),by = c("Date", "Period") ]
  
  # Detect weekend
  We <- rep(FALSE, length(calendar))
  We[grep("We", names(calendar))] <- TRUE
  
  # Apply classification for each period in calendar
  allTypDay <- rbindlist(apply(data.table(calendar, We, nn = names(calendar)), 1, function(season){
    setTxtProgressBar(pb, getTxtProgressBar(pb) + 1/7)
 
    nbClust <- ifelse(season$We, nbClustWeekend, nbClustWeek)
    veticesSel <- vertices[Date %in% as.character(season$calendar)]
    # get distance for each day pairs
    distMat <- .getDistMatrix(veticesSel, hourWeigth)
    
    # with hclust
    # vect <- cutree(hclust(distMat), nbClust)
    
    # clustering using PAM
    vect <- cluster::pam(distMat, nbClust, diss = TRUE)$clustering
    
    distMat <- as.matrix(distMat)
    typicalDay <- rbindlist(sapply(unique(vect), function(X){
      # Found a representative day for each class
      dateIn <- names(vect[which(vect == X)])
      colSel <- row.names(distMat)%in%dateIn
      #detect day closed to middle of cluster
      minDay <- which.min(rowSums(distMat[colSel, colSel]))
      distINfo <- distMat[minDay,colSel]
      data.table(TypicalDay = names(minDay),
                 Class = season$nn,
                 dayIn = list(data.table(Date = rep(dateIn, each = 24), Period = rep(1:24, length(dateIn)))),
                 distance = list(data.table(Date = dateIn, Distance = distINfo)))
    }, simplify = FALSE))
    typicalDay
  }))
  setTxtProgressBar(pb, getTxtProgressBar(pb) + 1/7)
  #Generate out data.table
  for(i in 1:nrow(allTypDay)){
    allTypDay$dayIn[[i]] <- list(merge(allTypDay$dayIn[[i]], vertices[,.SD, .SDcols = 1:3], by = c("Date", "Period")))
  }
  
  
  ##Ordered result
  
  orderVect <- c(which(allTypDay$Class == "summerWd"),
                 which(allTypDay$Class == "summerWe"),
                 which(allTypDay$Class == "winterWd"),
                 which(allTypDay$Class == "winterWe"),
                 which(allTypDay$Class == "interSeasonWd"),
                 which(allTypDay$Class == "interSeasonWe"))
  if(length(orderVect) == nrow(allTypDay)){
    allTypDay <- allTypDay[orderVect]
  }
  
  
  allTypDay[,idDayType :=1:.N ]
  
  # report generation
  if(report){
    outputFile <- reportPath
    if(is.null(outputFile)){
      outputFile <- getwd()
    }
    direName <-  as.character(Sys.time())
    direName <- gsub(" ", "", gsub( ":", "",direName))
    reportDir <- paste0(outputFile, "/", direName)
    dir.create(reportDir)
    outputFile <- reportDir
    
    sapply(allTypDay$idDayType, function(X){
      generateClusteringReport(X, data = allTypDay, outputFile = outputFile)})
    
    saveRDS(allTypDay, paste0(outputFile, "/resultClust.RDS"))
  }
  
  allTypDay
}

#' function to get mesh3d data from vertices
#'
#' @noRd
#'
.getMesh <- function(out){
  tc <- geometry::delaunayn(out, full = F)
  # visualize : tetramesh(tc,out,alpha=0.7)
  
  # sort
  tc_tri <- geometry::surf.tri(out, tc)
  # add column equal to 1
  out2 <- cbind(out, rep(1, nrow(out)))
  mesh <- rgl::tmesh3d(as.vector(t(out2[as.vector(t(tc_tri)),])),1:(nrow(tc_tri)*3))
  list(mesh)
}


#' Compute distance matrix
#'
#' @param vertices \code{data.table}
#' @param hourWeigth \code{numeric}, weigth vector of weighting for hours

#' @importFrom stats as.dist
#' @noRd
.getDistMatrix <- function(vertices, hourWeigth)
{
  res_hour <- data.table(t(combn(unique(vertices$Date), 2)))
  
  distMat <- data.table::rbindlist(lapply(1:nrow(res_hour), function(comb){
    date_1 <- res_hour[comb, V1]
    date_2 <- res_hour[comb, V2]
    v_hours <- intersect(vertices[Date%in% date_1, Period], vertices[Date%in% date_2, Period])
    hourDist <- data.table::rbindlist(lapply(v_hours, function(h){
      
      #Dist from X to Y
      x_on_y_All <- vcgClost(vertices[Date%in% date_1 & Period %in% h, ]$out[[1]],
                             vertices[Date%in% date_2 & Period %in% h, ]$mesh[[1]],
                             borderchk = TRUE,
                             sign = TRUE, facenormals = TRUE, barycentric = TRUE)
      
      x_on_y <- x_on_y_All$quality
      
      # If we want to filter points in domains
      # PTDFTp <- PTDF[Date == date_2 & Period == h]
      # x_on_y[which(apply(cbind(vertices[Date%in% date_1 & Period %in% h, ]$out[[1]], -rowSums(vertices[Date%in% date_1 & Period %in% h, ]$out[[1]])), 1, function(X){
      #   all(t(X)%*%
      #          t(as.matrix(PTDFTp[,.SD, .SDcols = c("BE", "DE", "FR", "NL")]))<= PTDFTp$RAM)}))] <- 0
      #Dist from Y to X
      y_on_x_All <- vcgClost(vertices[Date%in% date_2 & Period %in% h, ]$out[[1]],
                             vertices[Date%in% date_1 & Period %in% h, ]$mesh[[1]], borderchk = TRUE, sign = TRUE)
      
      
      y_on_x <- y_on_x_All$quality
      
      # If we want to filter points in domains
      # PTDFTp <- PTDF[Date == date_1 & Period == h]
      # y_on_x[which(apply(cbind(vertices[Date%in% date_2 & Period %in% h, ]$out[[1]], -rowSums(vertices[Date%in% date_2 & Period %in% h, ]$out[[1]])), 1, function(X){
      #   all(t(X)%*%
      #         t(as.matrix(PTDFTp[,.SD, .SDcols = c("BE", "DE", "FR", "NL")]))<= PTDFTp$RAM)}))] <- 0
      d <- mean(x_on_y^2) + mean(y_on_x^2)
      weigthPond <- hourWeigth[h]
      d <- weigthPond * d
      data.table(Date1 = c(date_1, date_2), Date2 = c(date_2, date_1), Period = h, dist = d)
    }))
    
    # Hour aggregation
    hourDist <- hourDist[, sqrt(sum(dist)), by = c("Date1", "Date2")]
    setnames(hourDist, "V1", "dayDist")
    hourDist
  }))
  
  # distance matrix creation
  distMat <- dcast(distMat, Date1~Date2,  value.var= "dayDist")
  distMat <- distMat[,.SD, .SDcols = 2:ncol(distMat)]
  diag(distMat) <- 0
  as.dist(distMat)
}
