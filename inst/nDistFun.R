vertices <- fread(system.file("dataset/vertices_example.txt",package = "flowBasedClustering"))

# build calendar (manually here to adapt to the small dataset in example)
# (for pratical applications getCalendar() function might be more convenient)

calendar <- list()
calendar$interSeasonWe <- c("2016-09-17", "2016-09-18")
calendar$interSeasonWd <- c("2016-09-19", "2016-09-20", "2016-09-21", "2016-09-22", "2016-09-23")
calendar$winterWe <- c("2016-12-10", "2016-12-11")
calendar$winterWd <- c("2016-12-12", "2016-12-13", "2016-12-14", "2016-12-15", "2016-12-16")
calendar$summerWe <- c("2016-08-06", "2016-08-07")
calendar$summerWd <- c("2016-08-08", "2016-08-09", "2016-08-10", "2016-08-11", "2016-08-12")

# run clustering algorithm
clusterTD <- clusteringTypicalDays(calendar, vertices, nbClustWeek = 2, nbClustWeekend = 1)



###Funcitons

.transformTS <- function(dt){
  dt$Period <- hour(dt$timestamp) + 1
  dt$Date <- as.character(as.Date(dt$timestamp))
  dt$timestamp <- NULL
  dt
}


giveDist <- function(Dmat, VE, PL)
{
  mean(sapply(1:nrow(VE), function(X){
    V1 <- VE[X]
    re <- solve.QP(Dmat = Dmat ,
                   dvec = unlist(V1[, .SD, .SDcols = 1:4])*2,
                   Amat = -t(as.matrix(PL[, .SD, .SDcols = 1:4])),
                   bvec = c(-PL$ram),meq = 0)
    
    
    sqrt(sum((re$solution-re$unconstrained.solution)^2))
  }))^2
}

dEnd <- function(VERT, PLAN)
{
  
  
  nbCt <- ncol(VERT) - 2
  Dmat <-matrix(0, nrow = nbCt, ncol = nbCt, byrow=TRUE)
  diag(Dmat) = 2
  
  
  X_Y <- giveDist(Dmat, VERT, PLAN)
  Y_X <- giveDist(Dmat, VERT, PLAN)
  X_Y + Y_X
}


library(fbTools)
library(parallel)
library(data.table)
library(quadprog)
PLAN <- getCnesFb(list.files("inst/PPTDF",full.names = TRUE, recursive = TRUE), 4 )
PLAN2 <- copy(PLAN)
PLAN <- PLAN[presolve == TRUE]

##Test ACP!
library(FactoMineR)
PTTEST <- PLAN[timestamp == '2018-12-03 00:00:00' | timestamp == '2018-12-04 00:00:00' ]
DVD <- PCA(PTTEST[, .SD, .SDcols = 1:5])


DVD$ind$coord[,1:3]
PTTEST$ptdfAT <- DVD$ind$coord[,1]
PTTEST$ptdfFR <- DVD$ind$coord[,2]
PTTEST$ptdfNL <- DVD$ind$coord[,3]
PTTEST$ptdfDE <- NULL
PTTEST$ptdfBE <- NULL
VERT <- fbTools::getVertices(PTTEST)
amPlot(VERT$ptdfAT, VERT$ptdfFR)


VERT <- fbTools::getVertices(PLAN)


vertices <- vertices[as.character(vertices$Date)%in%as.character( do.call("c", calendar))]

PLAN <- .transformTS(PLAN)
VERT <- .transformTS(VERT)
VERT <- VERT[as.character(VERT$Date)%in%as.character( do.call("c", calendar))]
calendar <- lapply(calendar, as.character)

# Detect weekend
We <- rep(FALSE, length(calendar))
We[grep("We", names(calendar))] <- TRUE
hourWeight <- rep(1, 24)
dtable <- .getDistMatrixV2(VERT, PLAN,
                           hourWeight)



dtable2 <- copy(dtable)

.getDistMatrixV2 <- function(VERT, PLAN, hourWeight){
  
  
  res_hour <- data.table(t(combn(unique(VERT$Date), 2)))
  
  
  rbindlist(sapply(1:nrow(res_hour), function(comb){
    
    ##To sapply
    date_1 <- res_hour[comb, V1]
    date_2 <- res_hour[comb, V2]
    v_hours <- intersect(VERT[Date%in% date_1, Period], VERT[Date%in% date_2, Period])
    
    ##To sapply
    # h <- v_hours[1]
    rbindlist(sapply(v_hours, function(h){
      print(h)
      DD <- dEnd(VERT[Date == date_1 & Period == h], PLAN[Date == date_2 & Period == h])
      DD2 <- dEnd(VERT[Date == date_2 & Period == h], PLAN[Date == date_1 & Period == h])
      d <- DD + DD2
      weigthPond <- hourWeight[h]
      d <- weigthPond * d
      data.table(Date1 = c(date_1, date_2), Date2 = c(date_2, date_1), Period = h, dist = d)
    }, simplify = FALSE))
    
  }, simplify = FALSE))
  
  
}




