#' Get probabilities and quantiles from climate file for a classification
#'
#' @param climate \code{data.table} climate file, 2 first columns are Date and Period, others are params files
#' @param cluster \code{data.table} output of \link{clusteringTypicalDays}
#' @param levelsProba \code{numeric or list}, can be \code{numeric} or \code{list}, if \code{numeric}, all quantiles
#' are the same for all climate variables, if \code{list}, must be names and give quantiles for each climate variable.
#' @param extrapolationNA \code{booelan} extrapolate NA value wich neighbours.
#'
#' @examples
#'
#' \dontrun{
#' # climate data
#' climate <- fread(system.file("dev/climat.txt",package = "flowBasedClustering"))
#'
#' # classification result
#' clusterTD <- readRDS(system.file("dev/ClassifOut.RDS",package = "flowBasedClustering"))
#'
#' levelsProba <- list(`Conso (J-1)` = c(0.2, 0.4, 0.8), DE_wind = c(0.3, 0.7),DE_solar = c(0.3,0.4,0.8) )
#' MatProb <- getProbability(climate, clusterTD, levelsProba = levelsProba)
#' 
#' levelsProba <- list(`Conso (J-1)` = c(0.2,0.25,0.5,0.75,0.90, 0.95), DE_wind = c(0.25,0.5,0.75, 0.9, 0.95),DE_solar = c(0.25,0.5,0.75) )
#' MatProb <- getProbability(climate, clusterTD, levelsProba = levelsProba, extrapolationNA = TRUE)
#' }
#' @export
getProbability <- function(climate, cluster, levelsProba = c(0.333, 0.666), extrapolationNA = TRUE)
{
  
  if(names(climate)[1]!='Date'){
    stop(paste0("First column of climate file must be called 'Date'. Current name : ",
                names(climate)[1]))
  }
  
  if(length(climate$Date)!=length(unique(climate$Date))){
    stop("Somes dates are not unique, you must give daily information")
  }
  
  allDateInClassif <- unlist(lapply(cluster$dayIn, function(X){
    unique(X[[1]]$Date)
  }))
  
  DayNoInclimate <- allDateInClassif[!allDateInClassif%in%as.character(climate$Date)]
  if(length(DayNoInclimate)>0){
    stop(paste0("Day(s) no present in climate file : ",paste0(DayNoInclimate, collapse = ", ")))
  }
  
  datesDeleted <- climate[!climate$Date%in%na.omit(climate)$Date]$Date
  if(length(datesDeleted)>0){
    warning(paste0("Day(s) deleted due to NA : ",paste0(datesDeleted, collapse = ", ")))
  }
  
  climate <- na.omit(climate)
  
  # Define all variable to used
  concerneName <- names(climate)[!names(climate)%in%c("Date")]
  
  # Control
  if(!is.list(levelsProba)){
    levelsProba <- sapply(concerneName, function(X){levelsProba}, simplify = FALSE)
  }
  if(!all(sort(names(levelsProba)) == sort(concerneName))){
    stop(paste0("levelsProba list must have same names than climate variable. Currenty : ",
                paste0(sort(names(levelsProba)), collapse = " , "), " and climate are : ",
                paste0(sort(concerneName), collapse = " , ")))
  }
  
  # Merge classification result with climate table
  climateAssoClasif <- .mergeclimateClassif(climate, cluster)
  allQuantile <- sapply(names(levelsProba), function(X){
    varProba <- levelsProba[[X]]
    varProba <- sort(varProba)
    varProbaCode <- paste0("Q", varProba)
    ClimQuantiles <- climateAssoClasif[,lapply(.SD, function(Y){
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
  climQuant <- unique(climateAssoClasif[, .SD, .SDcols =  c("TypicalDay", "idDayType", "Class")])
  
  
  # Create the structure for result
  andTableStruct <- data.table(Class = rep(climQuant$Class, each = nbcomp),
                               TypicalDay = rep(climQuant$TypicalDay, each = nbcomp),
                               idDayType = rep(climQuant$idDayType, each = nbcomp),
                               do.call("rbind", rep(list(allComb), nrow(climQuant)))
  )
  names(andTableStruct)[4:ncol(andTableStruct)] <- concerneName
  
  #Calculate probability for each quantiles
  probaAndEffectifs  <- rbindlist(sapply(1:nrow(andTableStruct), function(VV){
    
    #Select concern raw
    selectRaw <- andTableStruct[VV]
    constructRequest <- paste0("Class =='", selectRaw$Class,"'")
    constructRequest <-  parse(text = constructRequest)
    
    selectQuantile <- ClimQuantiles[eval(constructRequest)]
    climateConcern <- climateAssoClasif[eval(constructRequest)]
    requestQuantile <- selectRaw[, .SD, .SDcols = concerneName]
    
    #Make request for each quantile, exemple DE_wind < Q1 | Q1 < DE_wind < Q2
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
    
    
    #Conversion in query to fromat R
    req <- sapply(names(allRequest), function(X){
      elemReq <- allRequest[[X]]
      converRequest <- function(Y){
        Q <- paste0("Q", Y[2])
        paste(paste0("`", X, "`"), Y[1], as.numeric(selectQuantile[Quantiles == Q, .SD, .SDcols = X]))
      }
      
      if(is.list(elemReq))
      {
        tpreq <- paste(unlist(lapply(elemReq, converRequest)), collapse = " & ")
      }else{
        tpreq <- converRequest(elemReq)
      }
    }, simplify = FALSE)
    exp <- parse(text = paste0(unlist(req), collapse = "&"))
    climateConcern <- climateConcern[eval(exp)]
    
    #Calculate probability
    if(nrow(climateConcern)>0){
      value <- nrow(climateConcern[idDayType ==  selectRaw$idDayType])/nrow(climateConcern)
    }else{
      value <- NA
    }
    
    data.table(value, nrow(climateConcern))
  }, simplify = FALSE))
  
  
  andTableStruct$Proba <- probaAndEffectifs$value
  andTableStruct$effectifClass <- probaAndEffectifs$V2
  
  if(extrapolationNA)
  {
    ##Apply retro propagation when NA value of probability
    if(length(is.na(andTableStruct$Proba)) > 0){
      rowS <- andTableStruct[is.na(Proba)] 
      warning(paste0("due to lack of historical record, ", round(100*nrow(rowS)/nrow(andTableStruct), 1), "% of the probability matrix has been extrapolated"))
      andTableStruct[is.na(Proba)]$Proba <- sapply(1:nrow(rowS), function(i){
        selRow <- rowS[i]
        quantSel <- selRow[,.SD, .SDcols = names(quantileNonClaire)]
        preparedRequest <- sapply(names(quantSel), function(X){
          quantClaire <- quantileNonClaire[[X]]
          quantClaireBeg <- quantClaire
          placeQuantile <- which(unlist(quantSel[, .SD, .SDcols = X]) == quantClaire)
          placeQuantileBeg <- placeQuantile
          if(length(quantClaire) > 1){
            if(placeQuantile == 1){
              placeQuantile <- c( 2)
            }else if(placeQuantile == length(quantClaire)){
              placeQuantile <- c(placeQuantile - 1)
            }else{
              placeQuantile <- c(placeQuantile - 1, placeQuantile + 1)
            }
          }
          quantRequest <- quantClaire[placeQuantile]
          quantBeg <- quantClaire[placeQuantileBeg]
          list(Beg = quantBeg, New = quantRequest)
        }, simplify = FALSE)
        
        allRequest <- rbindlist(sapply(1:length(preparedRequest), function(j){
          Mv <- which(1:length(preparedRequest) == j)
          data.table(expand.grid(sapply(1:length(preparedRequest), function(k){
            if(Mv == k){
              preparedRequest[[k]]$New
            }else{
              preparedRequest[[k]]$Beg
            }
          }, simplify = FALSE)))
        }, simplify = FALSE))
        
        names(allRequest) <- names(quantSel)
        allRequest$idDayType <- selRow$idDayType
        result <- merge(allRequest, andTableStruct)
        if(length(which(result$effectifClass == 0)) > nrow(result) / 3){
          result <- 1/nrow(unique(andTableStruct[Class == selRow$Class, .SD, .SDcols = c("Class", "idDayType")]))
        }else{
          result <- mean(result$Proba, na.rm = TRUE)
        }
        
        if(is.nan(result))result <- NA
        result
      })
      
    }
  }
  return(list(andTableStruct, ClimQuantiles))
  
}


#' Data transformation (merge climate and cluster files)
#' @noRd
.mergeclimateClassif <- function(climate, cluster)
{
  dayTypeAsso <- unique(rbindlist(sapply(1:nrow(cluster), function(X){
    typeDay <- cluster[X]
    data.table(TypicalDay = typeDay$TypicalDay, Date = typeDay$dayIn[[1]][[1]]$Date,
               idDayType = typeDay$idDayType,
               Class = typeDay$Class)
  }, simplify = FALSE)))
  climateAssoClasif <- merge(dayTypeAsso, climate, by = c("Date"))
  climateAssoClasif
}

#' Plot monotone
#'
#' @param climate \code{data.table} climate file, 2 first columns are Date and Period, others are params files
#' @param cluster \code{data.table} output of \link{clusteringTypicalDays}
#' @param dayType \code{numeric} typical day
#' @param variable \code{character} name of variable to plot
#'
#' @examples
#'
#' \dontrun{
#' # climate data
#' climate <- fread(system.file("dev/climat.txt",package = "flowBasedClustering"))
#'
#' # classification result
#' clusterTD <- readRDS(system.file("dev/ClassifOut.RDS",package = "flowBasedClustering"))
#'
#' plotMonotone(climate = climate, cluster = clusterTD, dayType = 1, variable = "DE_wind")
#' }
#' @importFrom stats quantile na.omit
#' @export
plotMonotone <- function(climate, cluster, dayType, variable){
  climate <- na.omit(climate)
  climateAssoClasif <- .mergeclimateClassif(climate, cluster)
  idDayTypeTp <- dayType
  selData <- sort( unlist(climateAssoClasif[idDayType == idDayTypeTp,
                                            .SD, .SDcols = variable]), decreasing = TRUE)
  
  amPlot(-1+1:length(selData), selData, type = "l", xlab = "monotone", ylab = variable,
         main = paste0(variable, " day type ", idDayTypeTp), export = TRUE, zoom = TRUE) %>>%
    addGuide(value = 0, toValue = as.numeric(quantile(-1+1:length(selData), 0.333)), fillAlpha = 0.1, fillColor = "#FFFF00") %>>%
    addGuide(value = as.numeric(quantile(-1+1:length(selData), 0.333)), toValue =  as.numeric(quantile(-1+1:length(selData), 0.666)),
             fillAlpha = 0.1, fillColor = "#FF8000")%>>%
    addGuide(value = as.numeric(quantile(-1+1:length(selData), 0.666)), toValue =  length(selData) - 1, fillAlpha = 0.1,
             fillColor = "#FF0000")
}
