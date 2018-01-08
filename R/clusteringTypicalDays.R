#' @title Generate a set of flow-based typical days
#' @description Run a clustering algorithm on the different classes of the calendar (\link{getCalendar}).
#' Its principle is to create clusters by
#' gathering the most similar days of each class before choosing among them the best
#' representative: it will be a so-called typical day. The metric used to determine the similarity of two days is
#' a weighted sum of 24 hourly distances, meaning the distances between the domains of the two
#' days at the same hour. This distance is defined by projecting all the vertices of each domain
#' on the second one and adding the differences.
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
#' This parameter can be obtained with the function \link{ptdfToVertices}.
#' @param nbClustWeek \code{numeric}, number of clusters for week period(working days). Defaut to 3
#' @param nbClustWeekend \code{numeric}, number of clusters for weekend period. Defaut to 1
#' @param report \code{boolean}, should a .html report be generated with the results of the clustering. Defaut to TRUE
#' @param reportPath \code{character}, path where the report is written. Defaut to \code{getwd()}
#' @param hourWeight \code{numeric}, vector of 24 weights, one for each hour of the day. The clustering algorithm
#' will be more accurate for the flow-based domains of the hours with a relatively higher weight. 
#'
#' @examples
#'
#' \dontrun{
#' library(data.table)
#' 
#' # read vertices (from file, or obtained them with ptdfToVertices() function)
#' vertices <- fread(system.file("dataset/vertices_example.txt",package = "flowBasedClustering"))
#'
#' # build calendar (manually here to adapt to the small dataset in example)
#'  # (for pratical applications getCalendar() function might be more convenient)
#'  
#' calendar <- list()
#' calendar$interSeasonWe <- c("2016-09-17", "2016-09-18")
#' calendar$interSeasonWd <- c("2016-09-19", "2016-09-20", "2016-09-21", "2016-09-22", "2016-09-23")
#' calendar$winterWe <- c("2016-12-10", "2016-12-11")
#' calendar$winterWd <- c("2016-12-12", "2016-12-13", "2016-12-14", "2016-12-15", "2016-12-16")
#' calendar$summerWe <- c("2016-08-06", "2016-08-07")
#' calendar$summerWd <- c("2016-08-08", "2016-08-09", "2016-08-10", "2016-08-11", "2016-08-12")
#'
#' # run clustering algorithm
#' clusterTD <- clusteringTypicalDays(calendar, vertices, nbClustWeek = 2, nbClustWeekend = 1)
#' 
#' # run clustering algorithm distinguishing the days only with the flow-based domains of hour 18
#' w <- rep(0,24)
#' w[18] <- 1
#' clusterTD <- clusteringTypicalDays(calendar, vertices,  hourWeight = w, report = FALSE)
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
                                  hourWeight = rep(1, 24)){
  
  cat("Compute distances\n")
  
  pb <- txtProgressBar(style = 3)
  setTxtProgressBar(pb, 0)
  set.seed(123456)
  
  vertices <- .ctrlVertices(vertices)
  calendar <- lapply(calendar, as.character)
  
  lapply(calendar, function(X){
    .ctrlDates(X, unique(vertices$Date))
  })
  
  
  #control if the format of the vertices file is good
  .ctrlVerticesFormat(vertices)
  
  .ctrlWeight(hourWeight)
  
  #control names of calendar
  .ctrlCalendar(calendar)
  
  
  unVerticeDate <- unique(vertices$Date)
  sapply(names(calendar), function(X){
    if(sum(unVerticeDate%in%as.character(calendar[[X]])) == 0){
      stop(paste0("Intersection between season ", X, "(calendar) and vertices$Date is empty.
                  This job cant be run"))
    }
  })
  
  # generate mesh for each polyhedron, mesh is an object use to calculate distance between polyhedron
  
  #Compute mesh from vertices
  vertices <- .computeMesh(vertices)
  
  # Detect weekend
  We <- rep(FALSE, length(calendar))
  We[grep("We", names(calendar))] <- TRUE
  
  # Apply classification for each period in calendar
  allTypDay <- rbindlist(apply(data.table(calendar, We, nn = names(calendar)),
                               1, function(season){
    
    setTxtProgressBar(pb, getTxtProgressBar(pb) + 1/length(calendar))
    nbClust <- ifelse(season$We, nbClustWeekend, nbClustWeek)
    # get distance for each day pairs
    distMat <- .getDistMatrix(vertices[Date %in% as.character(season$calendar)],
                              hourWeight)
    
    # clustering using PAM
    vect <- cluster::pam(distMat, nbClust, diss = TRUE)$clustering
    
    distMat <- as.matrix(distMat)
    
    
    typicalDay <- rbindlist(sapply(unique(vect), function(X){
      # Found a representative day for each class
      .getDataAndMakeOutput(X, vect, distMat, season$nn)
      
    }, simplify = FALSE))
    typicalDay
  }))
  
  #Generate out data.table
  allTypDay <- .addVerticesToTp(allTypDay, vertices)
  
  ##Ordered result
  allTypDay <- .orderResult(allTypDay)
  
  allTypDay[,idDayType :=1:.N ]
  
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
    .saveRDSS(allTypDay, outL)
    
    setTxtProgressBar(pb, 1)
  }
  

  allTypDay
}

#' function to get mesh3d data from vertices
#'
#' @noRd
#' @importFrom rgl tmesh3d
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
#' @param hourWeight \code{numeric}, weigth vector of weighting for hours

#' @importFrom stats as.dist
#' @importFrom Rvcg vcgClost
#' @importFrom utils combn
#' @noRd
.getDistMatrix <- function(vertices, hourWeight)
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
      weigthPond <- hourWeight[h]
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



.ctrlVertices <- function(vertices)
{
  mths <- c("01", "02", "03", "04", "05" ,"06", "07", "08", "09", "10", "11", "12")
  if(grepl("^[[:digit:]]{2}(/){1}[[:digit:]]{2}(/){1}[[:digit:]]{4}$",  vertices$Date[1])){
    if(!all(substr(vertices$Date, 4, 5)%in%mths)){
      stop("Your date have ambiguous format, waiting is YYYY-MM-DD, you can convert with ?as.Date")
    }
    vertices$Date <- as.Date(vertices$Date, format = "%d/%m/%Y")
  }else{
    if(grepl("^[[:digit:]]{4}(-){1}[[:digit:]]{2}(-){1}[[:digit:]]{2}$",  vertices$Date[1])){
      if(!all(substr(vertices$Date, 6, 7)%in%mths)){
        stop("Your date have ambiguous format, waiting is YYYY-MM-DD, you can convert with ?as.Date")
      }
      
      vertices$Date <- as.Date(vertices$Date)
    }else{
      stop("Your date have ambiguous format, waiting is YYYY-MM-DD, you can convert with ?as.Date")
    }
  }
  vertices$Date <- as.character(vertices$Date)
  
  vertices
}


.ctrlVerticesFormat <- function(vertices)
{ 
  if(any(names(vertices)[1:5] != c("Date", "Period", "BE", "DE", "FR"))){
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
}

.ctrlWeight <- function(hourWeight){
  #control Weigth
  if(length(hourWeight)!=24){
    stop("Length of hourWeight must be 24")
  }
  
}

.computeMesh <- function(vertices){
  vertices <- vertices[, list(out = list(cbind(BE, DE, FR))), by = c("Date", "Period")]
  vertices[, mesh := list(.getMesh(out[[1]])),by = c("Date", "Period") ]
  vertices
}

.addVerticesToTp <- function(allTypDay, vertices)
{
  for(i in 1:nrow(allTypDay)){
    allTypDay$dayIn[[i]] <- list(merge(allTypDay$dayIn[[i]],
                                       vertices[,.SD, .SDcols = 1:3],
                                       by = c("Date", "Period")))
  }
  allTypDay
}

.saveRDSS <- function(allTypDay, outL){
  saveRDS(allTypDay, paste0(outL$outputFile, "/resultClust.RDS"))
}


.getDataAndMakeOutput <- function(X, vect, distMat, className)
{
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
               dayIn = list(data.table(Date = rep(dateIn, each = 24),
                                       Period = rep(1:24, length(dateIn)))),
               distance = list(data.table(Date = dateIn, Distance = distINfo)))
  }
}

.ctrlCalendar <- function(calendar){
  
  if(any(!names(calendar)%in% c("interSeasonWe",
                                "interSeasonWd",
                                "winterWe",
                                "winterWd",
                                "summerWe",
                                "summerWd")) | is.null(names(calendar))){
    
    stop("Names of calendar must be 'interSeasonWe',
         'interSeasonWd', 'winterWe',
         'winterWd', 'summerWe', 'summerWd'")
  }
  
}

.orderResult <- function(allTypDay)
{
  orderVect <- c(which(allTypDay$Class == "summerWd"),
                 which(allTypDay$Class == "summerWe"),
                 which(allTypDay$Class == "winterWd"),
                 which(allTypDay$Class == "winterWe"),
                 which(allTypDay$Class == "interSeasonWd"),
                 which(allTypDay$Class == "interSeasonWe"))
  if(length(orderVect) == nrow(allTypDay)){
    allTypDay <- allTypDay[orderVect]
  }
  allTypDay
}
