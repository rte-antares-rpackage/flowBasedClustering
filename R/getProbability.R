#' Get probability and quantiles from climat file and classif
#'
#' @param climat \code{data.table} climat file, 2 first columns are Date and Period, others are params files
#' @param classif \code{data.table} output of \link{classifTypicalDay}
#'
#'
#' @examples
#'
#' \dontrun{
#' climat <- fread(system.file("dev/Climat.txt",package = "flowBasedClustering"))
#'
#' classif <- readRDS(system.file("dev/ClassifOut.RDS",package = "flowBasedClustering"))
#'
#' MatProb <- getProbability(climat, classif)
#' }
#' @export
getProbability <- function(climat, classif, levelsProba = c(0.333, 0.666))
{

  ##Control
  levelsProba <- sort(levelsProba)
  if(names(climat)[1]!='Date'){
    stop(paste0("First column of climat file must be calls Date and contains dates actually : ",
                names(climat)[1]))
  }

  if(length(climat$Date)!=length(unique(climat$Date))){
    stop("Somes dates are present not unique, you must give daily information")
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
    warning(paste0("Day(s) dalated due to NA : ",paste0(datesDeleted, collapse = ", ")))
  }



  climat <-na.omit(climat)

  #Define all variable to used
  concerneName <- names(climat)[!names(climat)%in%c("Date")]

  ##Merge classif out wich climat table
  climatAssoClasif <- .climAssoClassif(climat, classif, concerneName)
  levelsProbaCode <- paste0("Q", levelsProba)

  #Calcul quantiles
  ClimQuantiles <- climatAssoClasif[,lapply(.SD, function(X){
    quantile(X, levelsProba)
  }), .SDcols = concerneName, by = c("Class")]
  ClimQuantiles$Quantiles <- levelsProbaCode

  quantileNonClaire <- NULL
  levelsProbaClair <- levelsProba

  #Define all comparaison to do (<q1, >q1 & <q2, ....)
  for(i in 1:length(levelsProbaClair)){
    if(i == 1){
      quantileNonClaire <- c(quantileNonClaire, paste0("I", levelsProbaClair[i]))
    }else{
      quantileNonClaire <- c(quantileNonClaire, paste0("B", levelsProbaClair[i - 1],"&", levelsProbaClair[i]))
    }

    if(i == length(levelsProbaClair)){
      quantileNonClaire <- c(quantileNonClaire, paste0("S", levelsProbaClair[i]))
    }
  }


  n <- length(concerneName)
  l <- rep(list(quantileNonClaire), n)

  #Extand combinaison to all variables
  allComb <- expand.grid(l, stringsAsFactors = FALSE)

  nbcomp <- nrow(allComb)
  climQuant <- unique(climatAssoClasif[, .SD, .SDcols =  c("TypicalDay", "idDayType", "Class")])


  #Creat the structure for result
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
.climAssoClassif <- function(climat, classif, concerneName)
{

  #climat[,c(concerneName) :=lapply(.SD, scale), .SDcols = concerneName]
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
#' @param classif \code{data.table} output of \link{classifTypicalDay}
#' @param hour \code{numeric} hour
#' @param dayType \code{numeric} typical day
#' @param variable \code{character} name of variable to plot
#'
#' @examples
#'
#' \dontrun{
#' climat <- fread("D:/Users/titorobe/Desktop/Antares/flowBasedClustering/inst/dev/Climat.txt")
#' classif <- readRDS("D:/Users/titorobe/Desktop/Antares/flowBasedClustering/inst/dev/ClassifOut.RDS")
#'
#' plotMonotone(climat = climat, classif = classif, hour = 1, dayType = 1, variable = "DE_wind")
#' }
#' @export
plotMonotone <- function(climat, classif, hour, dayType, variable){
  climat <-na.omit(climat)
  hour <- hour + 1
  concerneName <- names(climat)[!names(climat)%in%c("Date", "Period")]
  climatAssoClasif <- .climAssoClassif(climat, classif, concerneName)
  idDayTypeTp <- dayType
  selData <- sort( unlist(climatAssoClasif[Period == hour &
                                             idDayType == idDayTypeTp,
                                           .SD, .SDcols = variable]),
                   decreasing = TRUE)
  amPlot(-1+1:length(selData), selData, type = "l", xlab = "monotone", ylab = variable, main = paste0(variable, " hour ",
                                                                                hour - 1,  " day type ",
                                                                                idDayType))%>>%
    addGuide(value = 0, toValue =  as.numeric(quantile(-1+1:length(selData), 0.333)), fillAlpha = 0.1, fillColor = "#FFFF00")%>>%
    addGuide(value = as.numeric(quantile(-1+1:length(selData), 0.333)), toValue =  as.numeric(quantile(-1+1:length(selData), 0.666)), fillAlpha = 0.1,
             fillColor = "#FF8000")%>>%
    addGuide(value = as.numeric(quantile(-1+1:length(selData), 0.666)), toValue =  length(selData) - 1, fillAlpha = 0.1,
             fillColor = "#FF0000")
}
