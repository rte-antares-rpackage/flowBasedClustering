#' Get probabilities and quantiles from climat file for a classification
#'
#' @param climat \code{data.table} climat file, 2 first columns are Date and Period, others are params files
#' @param classif \code{data.table} output of \link{clusteringTypicalDays}
#' @param levelsProba \code{numeric or list}, can be \code{numeric} or \code{list}, if \code{numeric}, all quantiles
#' are the same for all climat variables, if \code{list}, must be names and give quantiles for each climat variable.
#'
#' @examples
#'
#' \dontrun{
#' # climat data
#' climat <- fread(system.file("dev/Climat.txt",package = "flowBasedClustering"))
#'
#' # classification result
#' clusterTD <- readRDS(system.file("dev/ClassifOut.RDS",package = "flowBasedClustering"))
#'
#' levelsProba <- list(`Conso (J-1)` = c(0.2, 0.8), DE_wind = c(0.3, 0.7),DE_solar = c(0.3,0.4,0.8) )
#' MatProb <- getProbability(climat, clusterTD, levelsProba = levelsProba)
#' }
#' @export
getProbability <- function(climat, classif, levelsProba = c(0.333, 0.666))
{

  if(names(climat)[1]!='Date'){
    stop(paste0("First column of climat file must be called 'Date'. Current name : ",
                names(climat)[1]))
  }

  if(length(climat$Date)!=length(unique(climat$Date))){
    stop("Somes dates are not unique, you must give daily information")
  }

  allDateInClassif <- unlist(lapply(classif$dayIn, function(X){
    unique(X[[1]]$Date)
  }))

  DayNoInClimat <- allDateInClassif[!allDateInClassif%in%as.character(climat$Date)]
  if(length(DayNoInClimat)>0){
    stop(paste0("Day(s) no present in climat file : ",paste0(DayNoInClimat, collapse = ", ")))
  }

  datesDeleted <- climat[!climat$Date%in%na.omit(climat)$Date]$Date
  if(length(datesDeleted)>0){
    warning(paste0("Day(s) deleted due to NA : ",paste0(datesDeleted, collapse = ", ")))
  }

  climat <- na.omit(climat)

  # Define all variable to used
  concerneName <- names(climat)[!names(climat)%in%c("Date")]

  # Control
  if(!is.list(levelsProba)){
    levelsProba <- sapply(concerneName, function(X){levelsProba}, simplify = FALSE)
  }
  if(!all(sort(names(levelsProba)) == sort(concerneName))){
    stop(paste0("levelsProba list must have same names than climat variable. Currenty : ",
                paste0(sort(names(levelsProba)), collapse = " , "), " and climat are : ",
                paste0(sort(concerneName), collapse = " , ")))
  }

  # Merge classification result with climat table
  climatAssoClasif <- .mergeClimatClassif(climat, classif)
    allQuantile <- sapply(names(levelsProba), function(X){
      varProba <- levelsProba[[X]]
      varProba <- sort(varProba)
      varProbaCode <- paste0("Q", varProba)
      ClimQuantiles <- climatAssoClasif[,lapply(.SD, function(Y){
        quantile(Y, varProba)
      }), .SDcols = X, by = c("Class")]
      ClimQuantiles$Quantiles <- varProbaCode
      ClimQuantiles
    }, simplify = FALSE, USE.NAMES = FALSE)
    mergeQuantiles <- function(X, Y){merge(x = X, y = Y,  by = c("Class", "Quantiles"),all = TRUE)}
    ClimQuantiles <-  Reduce(mergeQuantiles,allQuantile)

  quantileNonClaire <- NULL
  levelsProbaClair <- levelsProba

  # Define all comparaison to do (<q1, >q1 & <q2, ....)
  quantileNonClaire <-  lapply(levelsProbaClair, function(levelModa){
    quantileNonClaire <- NULL
  for(i in 1:length(levelModa)){

    if(i == 1){
      quantileNonClaire <- c(quantileNonClaire, paste0("I", levelModa[i]))
    }else{
      quantileNonClaire <- c(quantileNonClaire, paste0("B", levelModa[i - 1],"&", levelModa[i]))
    }

    if(i == length(levelModa)){
      quantileNonClaire <- c(quantileNonClaire, paste0("S", levelModa[i]))
    }
  }
    quantileNonClaire
  })

  n <- length(concerneName)
  # quantileNonClaire <- rep(list(quantileNonClaire), n)

  # Extand combinaison to all variables
  allComb <- expand.grid(quantileNonClaire, stringsAsFactors = FALSE)

  nbcomp <- nrow(allComb)
  climQuant <- unique(climatAssoClasif[, .SD, .SDcols =  c("TypicalDay", "idDayType", "Class")])


  # Create the structure for result
  andTableStruct <- data.table(Class = rep(climQuant$Class, each = nbcomp),
                               TypicalDay = rep(climQuant$TypicalDay, each = nbcomp),
                               idDayType = rep(climQuant$idDayType, each = nbcomp),
                               do.call("rbind", rep(list(allComb), nrow(climQuant)))
  )
  names(andTableStruct)[4:ncol(andTableStruct)] <- concerneName

  probaAndEffectifs  <- rbindlist(sapply(1:nrow(andTableStruct), function(VV){
    StructRaw <- andTableStruct[VV]
    constructRequest <- paste0("Class =='", StructRaw$Class,"'")
    constructRequest <-  parse(text = constructRequest)

    quantilesConcern <- ClimQuantiles[eval(constructRequest)]
    climatConcern <- climatAssoClasif[eval(constructRequest)]
    requestQuantile <- StructRaw[, .SD, .SDcols = concerneName]

    allRequest <- lapply(requestQuantile, function(X){
      sens <- substr(X, 1, 1)
      res <- NULL
      if(sens == "I"){
        res <- c("<", substr(X, 2, nchar(X)))
      }
      if(sens == "S"){
        res <- c(">=", substr(X, 2, nchar(X)))
      }
      if(sens == "B"){
        X <- gsub("B", "", X)
        X <- strsplit(X, "&")[[1]]
        res <- list(c(">", X[1]), c("<=", X[2]))
      }

      res
    })


    req <- sapply(names(allRequest), function(X){
      elemReq <- allRequest[[X]]
      converRequest <- function(Y){
        Q <- paste0("Q", Y[2])
        paste(paste0("`", X, "`"), Y[1], as.numeric(quantilesConcern[Quantiles == Q, .SD, .SDcols = X]))
      }

      if(is.list(elemReq))
      {
        tpreq <- paste(unlist(lapply(elemReq, converRequest)), collapse = " & ")
      }else{
        tpreq <- converRequest(elemReq)
      }
    }, simplify = FALSE)


    exp <- parse(text = paste0(unlist(req), collapse = "&"))
    climatConcern <- climatConcern[eval(exp)]

    StructRaw$idDayType

    if(nrow(climatConcern)>0){

      value <- nrow(climatConcern[idDayType ==  StructRaw$idDayType])/nrow(climatConcern)

    }else{
      value <- NA
    }

    data.table(value, nrow(climatConcern))
  }, simplify = FALSE))

  andTableStruct$Proba <- probaAndEffectifs$value
  andTableStruct$effectifClass <- probaAndEffectifs$V2

  return(list(andTableStruct, ClimQuantiles))

}


#' Data transformation (merge climat and classif files)
#' @noRd
.mergeClimatClassif <- function(climat, classif)
{
  dayTypeAsso <- unique(rbindlist(sapply(1:nrow(classif), function(X){
    typeDay <- classif[X]
    data.table(TypicalDay = typeDay$TypicalDay, Date = typeDay$dayIn[[1]][[1]]$Date,
               idDayType = typeDay$idDayType,
               Class = typeDay$Class)
  }, simplify = FALSE)))
  climatAssoClasif <- merge(dayTypeAsso, climat, by = c("Date"))
  climatAssoClasif
}

#' Plot monotone
#'
#' @param climat \code{data.table} climat file, 2 first columns are Date and Period, others are params files
#' @param classif \code{data.table} output of \link{clusteringTypicalDays}
#' @param dayType \code{numeric} typical day
#' @param variable \code{character} name of variable to plot
#'
#' @examples
#'
#' \dontrun{
#' # climat data
#' climat <- fread(system.file("dev/Climat.txt",package = "flowBasedClustering"))
#'
#' # classification result
#' clusterTD <- readRDS(system.file("dev/ClassifOut.RDS",package = "flowBasedClustering"))
#'
#' plotMonotone(climat = climat, classif = clusterTD, dayType = 1, variable = "DE_wind")
#' }
#' @export
plotMonotone <- function(climat, classif, dayType, variable){
  climat <- na.omit(climat)
  climatAssoClasif <- .mergeClimatClassif(climat, classif)
  idDayTypeTp <- dayType
  selData <- sort( unlist(climatAssoClasif[idDayType == idDayTypeTp,
                                           .SD, .SDcols = variable]), decreasing = TRUE)
  
  amPlot(-1+1:length(selData), selData, type = "l", xlab = "monotone", ylab = variable, 
         main = paste0(variable, " day type ", idDayTypeTp), export = TRUE, zoom = TRUE) %>>%
    addGuide(value = 0, toValue = as.numeric(quantile(-1+1:length(selData), 0.333)), fillAlpha = 0.1, fillColor = "#FFFF00") %>>%
    addGuide(value = as.numeric(quantile(-1+1:length(selData), 0.333)), toValue =  as.numeric(quantile(-1+1:length(selData), 0.666)), 
             fillAlpha = 0.1, fillColor = "#FF8000")%>>%
    addGuide(value = as.numeric(quantile(-1+1:length(selData), 0.666)), toValue =  length(selData) - 1, fillAlpha = 0.1,
             fillColor = "#FF0000")
}
