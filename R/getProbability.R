#' Classify the typical days by searching for external factors (e.g. climate factors) explaning 
#' the occurence of a particular typical day
#'
#' @param climate \code{data.table}, the first column contains the dates (header : Date, format : YYYY-MM-DD), 
#' other columns are external variable whose name must be given in the header. Data should be given with a daily time step.
#' @param cluster \code{data.table} output of \link{clusteringTypicalDays} or \link{clusterTypicalDaysForOneClass}
#' @param levelsProba \code{numeric or list}. if \code{numeric}, contains a vector of quantiles which will be used for all 
#' external variables and classes. 
#' if \code{list}, quantiles are described independenly for each class and each external variable (see examples below)
#' @param extrapolationNA \code{booelan} should NA values of the matrix be extrapolated (\code{TRUE}) or not (\code{FALSE})
#' 
#' @examples
#'
#' \dontrun{
#' # load climate daily time serires
#' library(data.table)
#' climate <- fread(system.file("dataset/climate_example.txt",package = "flowBasedClustering"))
#'
#' # load clustering results (or build them with clusteringTypicalDays function())
#' clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
#'
#' # get Probability matrices
#' MatProb <- getProbability(climate, clusterTD)
#' 
#' # run with specified probability levels, and witout auto-filling the NA cases
#' levelsProba <- list(summerWd = list(FR_load = c(0.5), DE_wind = c(1/3, 2/3), DE_solar = .5),
#'                     summerWe = list(FR_load = c(0.5, 0.7), DE_wind = c(.5))
#' )
#' MatProb <- getProbability(climate, clusterTD, levelsProba = levelsProba, extrapolationNA = FALSE)
#' MatProb2 <- getProbability(climate, clusterTD, levelsProba = levelsProba, extrapolationNA = TRUE)
#' 
#' }
#' @export
getProbability <- function(climate, cluster, levelsProba = c(1/3, 2/3), extrapolationNA = TRUE)
{
  quantiles <- NULL
  class <- unique(cluster$Class)
  clVar <- names(climate)[!names(climate)%in%"Date"]
  # Compute quantiles
  
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
    warning(paste0("Day(s) deleted due to NA in climate file : ",paste0(datesDeleted, collapse = ", ")))
  }
  
  climate <- na.omit(climate)
  
  # Define all variable to used
  concerneName <- names(climate)[!names(climate)%in%c("Date")]
  
  
  levelsProba <- .ctrlAndMergeLevelsProba(levelsProba,clVar, class, cluster )
  
  # Merge classification result with climate table
  climateAssoClasif <- .mergeclimateClassif(climate, cluster)
  
  ##Compute quantiles for eatch season / variable
  ClimQuantiles <- .calclQuantiles(levelsProba, climateAssoClasif)
  
  allComb <- .givequantilesOperation(levelsProba)
  climStru <- unique(climateAssoClasif[,.SD, .SDcols = 2:4])
  allComb <- merge(climStru, allComb, by = "Class", allow.cartesian=TRUE)
  
  #Calcul proba for eatch combinaison
  probaNotExtrapol <- .calcProbByComb(allComb, ClimQuantiles, clVar, climateAssoClasif)
  
  setnames(probaNotExtrapol, "value", "Proba")
  
  
  #If no extrapolation return result
  
  
  
  
  if(!extrapolationNA){
    setnames(ClimQuantiles, "Class", "class")
    setnames(ClimQuantiles, "Quantiles", "quantiles")
    
    setnames(probaNotExtrapol, "Class", "class")
    setnames(probaNotExtrapol, "TypicalDay", "typicalDay")
    setnames(probaNotExtrapol, "idDayType", "idDayType")
    setnames(probaNotExtrapol, "Proba", "probability")
    setnames(probaNotExtrapol, "V2", "sizeClass")
    ClimQuantiles <- ClimQuantiles[!quantiles %in% c("Q0", "Q1")]
    
    
    return(list(probaNotExtrapol, ClimQuantiles))
    }
  
  
  if(extrapolationNA)
  {
    ##Apply retro propagation when NA value of probability
    if(length(is.na(probaNotExtrapol$Proba)) > 0){
      probaToExtrapol <- probaNotExtrapol[is.na(Proba)] 
      warning(paste0("due to lack of historical record, ",
                     round(100*nrow(probaToExtrapol)/nrow(probaNotExtrapol), 1),
                     "% of the probability matrix has been extrapolated"))
    }
    
    
    probaNotExtrapol$Proba <- sapply(1:nrow(probaNotExtrapol),function(roo){
      rowForFile <- probaNotExtrapol[roo]
      if(is.na(rowForFile$Proba)){
        pbForExtrapol <- probaNotExtrapol[idDayType == rowForFile$idDayType]
        probPbTpDay <- probaToExtrapol[idDayType == rowForFile$idDayType]
        levelsProbaSeason <- levelsProba[[probPbTpDay$Class[1]]]
        
        initial <- lapply(strsplit(unlist(rowForFile[, .SD, .SDcols = clVar]), "_"), as.numeric)
        maxireq <- paste0(sapply(names(levelsProbaSeason), function(i){
          initI <- initial[[i]]
          lvlI <- levelsProbaSeason[[i]]
          
          clVal <- .getClosedValue(initI, lvlI)
          
          miniReq <- paste("(", paste(paste0(i, "=='", unlist(sapply(1:(length(clVal)-1), function(j){
            paste0(clVal[j], "_", clVal[j + 1], "'")
          }))), collapse = "|"), ")")
          
          otherReq <- unlist(rowForFile[, .SD, .SDcols = clVar[!clVar%in%i]])
          paste("(", paste(c(miniReq, paste0(names(otherReq), "=='", otherReq, "'")), collapse = "&"), ")")
          
        }), collapse = "|")
        
        newRow <- pbForExtrapol[eval(parse(text = maxireq))]
        newRow <- newRow[!rowForFile, on=c(names(rowForFile))]
        
        if(length(which(newRow$V2 == 0)) > nrow(newRow) / 3){
          newValue <- 1/nrow(unique(probaNotExtrapol[Class == newRow$Class[1], .SD, .SDcols = c("Class", "idDayType")]))
        }else{
          newRow <- newRow[!is.na(Proba)]
          
          
          if(nrow(newRow) > 0){
            newValue <- mean(newRow$Proba)#Here mean 
          }else{
            newValue <- NA
          }
        }
      }else{
        newValue <- rowForFile$Proba
      }
      
      newValue
    })
  }
  
  setnames(ClimQuantiles, "Class", "class")
  setnames(ClimQuantiles, "Quantiles", "quantiles")
  
  setnames(probaNotExtrapol, "Class", "class")
  setnames(probaNotExtrapol, "TypicalDay", "typicalDay")
  setnames(probaNotExtrapol, "idDayType", "idDayType")
  setnames(probaNotExtrapol, "Proba", "probability")
  setnames(probaNotExtrapol, "V2", "sizeClass")
  ClimQuantiles <- ClimQuantiles[!quantiles %in% c("Q0", "Q1")]
  return(list(probaNotExtrapol, ClimQuantiles))
  
}



.getClosedValue <- function(initI, lvlI){
  witchI <- which(initI%in%lvlI)
  endW <- witchI
  if(witchI[1]!=1){
    endW <- c(endW, witchI[1] - 1)
  }
  if(witchI[2]!= length(lvlI)){
    endW <- c(endW, witchI[2] + 1)
    
  }
  lvlI[sort(unique(c(endW, witchI[1]+1, witchI[2]-1)))]
  
}


.calcProbByComb <- function(allComb, ClimQuantiles, clVar, climateAssoClasif){
  
  cbind(allComb, rbindlist(sapply(1:nrow(allComb), function(j)
  {
    tpComb <- allComb[j]
    ClimQuantilesSel <- ClimQuantiles[Class == tpComb$Class]
    
    exp <- paste(sapply(clVar, function(i)
    {
      quantToRequest <- paste0("Q", unlist(strsplit(tpComb[[i]], "_")))
      quantilesValue <- sort(unlist(ClimQuantilesSel[Quantiles %in% quantToRequest, .SD, .SDcols = i]))
      if(quantToRequest[1] == "Q0"){
        paste0(i , "<=", quantilesValue[2]  )
      }else{
        if(quantToRequest[2] == "Q1"){
          paste0(i , ">=", quantilesValue[1]  )
          
        }else{
          paste0(i , ">=", quantilesValue[1] , "&",i , "<", quantilesValue[2]  )
          
        }
      }
    }), collapse = "&")
    
    
    climCsrn <- climateAssoClasif[Class == tpComb$Class]
    climCsrn <- climCsrn[eval(parse(text =exp))]
    
    #Calculate probability
    if(nrow(climCsrn)>0){
      value <- nrow(climCsrn[idDayType ==  tpComb$idDayType])/nrow(climCsrn)
    }else{
      value <- NA
    }
    data.table(value, nrow(climCsrn))
  }, simplify = FALSE)))
}


.ctrlAndMergeLevelsProba <- function(levelsProba, clVar, class, cluster){
  if(!is.list(levelsProba)){
    levelsProba <- sapply(class, function(X){
      sapply(clVar, function(Y){
        levelsProba
      }, simplify = FALSE)
    }, simplify = FALSE)
  }
  #Control levelsProba format
  .ctrlvlPb(levelsProba, clVar, class)
  
  #Round
  levelsProba <- sapply(levelsProba, function(X)sapply(X, function(Y)round(Y, 4), simplify = FALSE), simplify = FALSE)
  
  classNotIn <- class[!class%in%names(levelsProba)]
  if(length(classNotIn)>0)
  {
    for(i in classNotIn){ levelsProba[[i]] <- list()}
  }
  for(i in names(levelsProba)){
    for(j in clVar){
      if(is.null(levelsProba[[i]][[j]])){
        levelsProba[[i]][[j]] <- c(0,1)
      }else{
        levelsProba[[i]][[j]] <- c(0, levelsProba[[i]][[j]], 1)
        
      }
    }
  }
  
  tabCl <- table(cluster$Class)
  cl1 <- names(tabCl)[tabCl == 1]
  if(length(cl1) > 0){
    for(i in cl1){
      totest <- levelsProba[[i]]
      if(!all(unlist(lapply(totest, function(X){
        identical(X , c(0, 1))
      })))){
        warning(paste0("Somes probabilities are defined for season : ",
                       i, " which only contains one typical day. They have been pass to NULL"))
        
        for(j in names(levelsProba[[i]])){
          levelsProba[[i]][[j]] <- c(0, 1)
        }
      }
    }
  }
  levelsProba
}




.ctrlvlPb <- function(levelsProba, clVar, class){
  
  if(!all(names(levelsProba) %in% class)){
    stop("Names of the list levelsProba should be classes of the clustering :",
         paste0(class , collapse = ";"))
  }
  
  lapply(levelsProba, function(X){
    if(class(X) != 'list'){
      stop("A list of variables is expected in each class of levelsProba, here : ",
           paste0(clVar, collapse = ";"))
    }
  })
  
  if(!all(unlist(levelsProba)>0) | ! all(unlist(levelsProba)<1) ){
    stop("All levelsProba must be between 0 ans 1")
  }
  
  
  lapply(levelsProba, function(X){
    if(!all(names(X)%in%clVar)){
      stop(paste0(names(X)[!names(X) %in% clVar], collapse = ";"), "is not a variable of the climate input.")
    }
  })
  NULL
}




.givequantilesOperation <- function(levelsProba)
{
  # Define all comparaison to do (<q1, >q1 & <q2, ....)
  quantileNonClaire <-  lapply(levelsProba, function(season){
    lapply(season, function(var){
      allOut <- NULL
      for(i in 1:(length(var)-1)){
        allOut <- c(allOut, paste0(var[i], "_", var[i+1]))
      }
      allOut
    })
    
  })
  
  # quantileNonClaire <- rep(list(quantileNonClaire), n)
  
  # Extand combinaison to all variables
  allComb <- lapply(quantileNonClaire, function(X){expand.grid(X, stringsAsFactors = FALSE)})
  for(i in names(allComb)){
    allComb[[i]]["Class"] <- i
  }
  allComb <- rbindlist(allComb)
  allComb
}

.calclQuantiles <- function(levelsProba, climateAssoClasif)
{
  mergeQuantiles <- function(X, Y){merge(x = X, y = Y,  by = c("Class", "Quantiles"),all = TRUE)}
  rbindlist(sapply(names(levelsProba), function(X){
    Reduce(mergeQuantiles, sapply(names(levelsProba[[X]]), function(Y){
      varProba <- levelsProba[[X]][[Y]]
      varProba <- sort(varProba)
      varProbaCode <- paste0("Q", varProba)
      ClimQuantiles <- climateAssoClasif[Class == X,lapply(.SD, function(Y){
        quantile(Y, varProba)
      }), .SDcols = Y]
      ClimQuantiles$Class <- X
      ClimQuantiles$Quantiles <- varProbaCode
      
      ClimQuantiles
    }, simplify = FALSE, USE.NAMES = FALSE))
  }, simplify = FALSE, USE.NAMES = FALSE))
  
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
#' climate <- fread(system.file("dataset/climate_example.txt",package = "flowBasedClustering"))
#'
#' # classification result
#' clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
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



# climate <- fread(system.file("dataset/climate_example.txt",package = "flowBasedClustering"))
# 
# # load clustering results (or build them with clusteringTypicalDays function())
# clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
# 
# extrapolationNA <- TRUE
# 
# levelsProba <- list(
#   winterWd = list(FR_load = c(1/3, 2/3),
#                   DE_wind = c(1/3, 2/3)),
#   interSeasonWd = list(FR_load = c(1/3, 2/3),
#                        DE_wind = c(1/3, 2/3),
#                        DE_solar = c(1/2))
# )
# rem <- getProbability2(climate, clusterTD, levelsProba =levelsProba, extrapolationNA = TRUE)
# ram <- getProbability(climate, clusterTD, levelsProba = c(.1, 0.9), extrapolationNA = FALSE)
# 
# 
# 
# 
# 
# microbenchmark(getProbability2(climate, clusterTD, levelsProba = c(.1, 0.9), extrapolationNA = FALSE),
#                getProbability(climate, clusterTD, levelsProba = c(.1, 0.9), extrapolationNA = FALSE))
# 
# microbenchmark(getProbability2(climate, clusterTD, levelsProba = c(.1, 0.9), extrapolationNA = TRUE),
#                getProbability(climate, clusterTD, levelsProba = c(.1, 0.9), extrapolationNA = TRUE))
# 
# 
# rem <- getProbability2(climate, clusterTD, levelsProba =c(.1, 0.9), extrapolationNA = TRUE)
# ram <- getProbability(climate, clusterTD, levelsProba = c(.1, 0.9), extrapolationNA = TRUE)
# 
# 
# 
# 
# ram[[1]]
# rem[[1]]$FR_load <- gsub("0.1_1", "S0.1", rem[[1]]$FR_load )
# rem[[1]]$FR_load <- gsub("0_0.1", "I0.1", rem[[1]]$FR_load )
# rem[[1]]$DE_wind <- gsub("0.1_1", "S0.1", rem[[1]]$DE_wind )
# rem[[1]]$DE_wind <- gsub("0_0.1", "I0.1", rem[[1]]$DE_wind )
# rem[[1]]$DE_solar <- gsub("0.1_1", "S0.1", rem[[1]]$DE_solar )
# rem[[1]]$DE_solar <- gsub("0_0.1", "I0.1", rem[[1]]$DE_solar )
# 
# ctrl <- merge(rem[[1]], ram[[1]], by = c("Class",
#                                          "TypicalDay",
#                                          "idDayType",
#                                          "FR_load",
#                                          "DE_wind",
#                                          "DE_solar"))
# identical(ctrl$Proba.x, ctrl$Proba.y)
# 
# ctrl
# 
# identical(ctrl$V2, ctrl$effectifClass)
# 
# 
# 
# climate[Date%in%c(clusterTD[Class=="interSeasonWd"]$dayIn[[1]][[1]]$Date, clusterTD[Class=="interSeasonWd"]$dayIn[[2]][[1]]$Date)]
# 
# 
# 
# getProbability2(climate, clusterTD)
# 

