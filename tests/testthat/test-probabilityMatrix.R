context("Function getProbability")
library(data.table)
library(pipeR)

test_that("clustering getProbability works", {
  
  # load climate file (with NA)
  climate <- fread(system.file("testdata/climate2016.txt",package = "flowBasedClustering"))
  
  # load previsouly made typical day selection on a few days (cf. test-clustering.R)
  clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",package = "flowBasedClustering"))
  
  # run getProbability function
  probMatrix <- getProbability(climate, cluster = clusterTD, levelsProba = c(0.333, 0.666))
  
  # expect list of size 2
  expect_type(probMatrix, "list")
  expect_length(probMatrix, 2)
  
  # check that : for a combinaison of class AND levels of each variable,
  # the sum of the probabilities for each typical day is one
  
  shoubBeOne <- probMatrix[[1]][,sum(probability), by = c("class", "FR_load", "DE_wind", "DE_solar")]
  
  expect_true(all(abs(shoubBeOne$V1 - 1) < 0.00001))

  # check that there is no NA in output :
  expect_false(any(is.na(probMatrix[[1]]$probability)))
  
  # getProbability without NA extrapolation
  probMatrixWithNA <- getProbability(climate, cluster = clusterTD, levelsProba = c(0.333, 0.666), extrapolationNA = FALSE)
  
  # check that there is a NA when there is no historical record
  expect_true(all(is.na(probMatrixWithNA[[1]][sizeClass == 0]$probability)))
  
  # check that non-NA values are similar
  pNA <- probMatrixWithNA[[1]]$probability
  p <- probMatrix[[1]]$probability
  expect_true(length(pNA) == length(p))
  expect_true(all(pNA[!is.na(pNA)] == p[!is.na(pNA)]))
  
  
  # no automatic test yet exists on how the NA values have been replaced with extrapolationNA = TRUE
  
  # getProbability with different levels
  levelsProba <- list(FR_load = c(0.2,0.25,0.5,0.75,0.90, 0.95), DE_wind = c(1/3, 2/3),DE_solar = c(0.5) )
  expect_error(getProbability(climate, clusterTD, levelsProba = levelsProba))
  
  levelsProba <- list(summerWd = list(FR_load = c(0.5), DE_wind = c(1/3, 2/3), DE_solar = .5),
                      summerWe = list(FR_load = c(0.5, 0.7), DE_wind = c(.5))
                      )
  probMatrixWithNA <- getProbability(climate, cluster = clusterTD,
                                     levelsProba = levelsProba, extrapolationNA = TRUE)
  
  shoubBeOne <- probMatrixWithNA[[1]][,sum(probability), by = c("class", "FR_load", "DE_wind", "DE_solar")]
  
  expect_true(all(abs(shoubBeOne$V1 - 1) < 0.00001))
  
})



test_that("clustering old and new version", {
  getProbability_old <- function(climate, cluster, levelsProba = c(1/3, 2/3), extrapolationNA = TRUE)
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
      warning(paste0("Day(s) deleted due to NA in climate file : ",paste0(datesDeleted, collapse = ", ")))
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
    
    # round levelsProba
    levelsProba <- lapply(levelsProba, FUN = round, digits = 4)
    
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
      exp <- parse(text = paste0(unlist(req), collapse = " & "))
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
  
  
  climate <- fread(system.file("dataset/climate_example.txt",
                               package = "flowBasedClustering"))
  
  # load clustering results (or build them with clusteringTypicalDays function())
  clusterTD <- readRDS(system.file("dataset/cluster_example.RDS",
                                   package = "flowBasedClustering"))
  suppressWarnings(sapply(c(TRUE, FALSE), function(X){
    rem <- getProbability_old(climate, clusterTD, levelsProba =c(.1), extrapolationNA = X)
    ram <- getProbability(climate, clusterTD, levelsProba = c(.1), extrapolationNA = X)
    
    ram[[1]]$FR_load <- gsub("0.1_1", "S0.1", ram[[1]]$FR_load )
    ram[[1]]$FR_load <- gsub("0_0.1", "I0.1", ram[[1]]$FR_load )
    ram[[1]]$DE_wind <- gsub("0.1_1", "S0.1", ram[[1]]$DE_wind )
    ram[[1]]$DE_wind <- gsub("0_0.1", "I0.1", ram[[1]]$DE_wind )
    ram[[1]]$DE_solar <- gsub("0.1_1", "S0.1", ram[[1]]$DE_solar )
    ram[[1]]$DE_solar <- gsub("0_0.1", "I0.1", ram[[1]]$DE_solar )
    
    names(rem[[1]])[1:2] <- c("class", "typicalDay")
    ctrl <- merge(rem[[1]], ram[[1]], by = c("class",
                                             "typicalDay",
                                             "idDayType",
                                             "FR_load",
                                             "DE_wind",
                                             "DE_solar"))
    expect_true(identical(ctrl$Proba.x, ctrl$Proba.y))
  }))
  
})