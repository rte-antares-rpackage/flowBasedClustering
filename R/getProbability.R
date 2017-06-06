#' Get probability and quantiles from climat file and classif
#'
#' @param climat \code{data.table} climat file, 2 first columns are Date and Period, others are params files
#' @param classif \code{data.table} output of \link{classifTipycalDay}
#'
#'
#' @examples
#'
#' \dontrun{
#' climat <- fread("D:/Users/titorobe/Desktop/Antares/flowBasedClustering/inst/dev/Climat.txt")
#' classif <- readRDS("D:/Users/titorobe/Desktop/Antares/flowBasedClustering/inst/dev/ClassifOut.RDS")
#'
#' MatProb <- getProbability(climat, classif)
#' }
#' @export
getProbability <- function(climat, classif)
{

  if(names(climat)[1]!='Date'){
    stop(paste0("First column of climat file must be calls Date and contains dates actually : ",
                names(climat)[1]))
  }

  if(names(climat)[2]!='Period'){
    stop(paste0("First column of climat file must be Period Date and contains id hours actually : ",
                names(climat)[2]))
  }

  climat <-na.omit(climat)
  concerneName <- names(climat)[!names(climat)%in%c("Date", "Period")]
  climatAssoClasif <- .climAssoClassif(climat, classif, concerneName)
  quantiles033 <- climatAssoClasif[,lapply(.SD, function(X){quantile(X, probs = c(0.333), na.rm = TRUE)}),
                                   by = c("Period", "TypicalDay", "idDayType"), .SDcols = concerneName]
  setnames(quantiles033,concerneName, paste0(concerneName, "0.33"))
  var033Nam <-  paste0(concerneName, "0.33")
  climatAssoClasif <- merge(climatAssoClasif, quantiles033, by = c("Period", "TypicalDay", "idDayType"))
  quantiles066 <- climatAssoClasif[,lapply(.SD, function(X){quantile(X, probs = c(0.666), na.rm = TRUE)}),
                                   by = c("Period", "TypicalDay", "idDayType"), .SDcols = concerneName]
  setnames(quantiles066,concerneName, paste0(concerneName, "0.66"))
  var066Nam <-  paste0(concerneName, "0.66")

  climatAssoClasif <- merge(climatAssoClasif, quantiles066, by = c("Period", "TypicalDay", "idDayType"))
  Inf033 <- climatAssoClasif[,.SD, .SDcols = concerneName]<=climatAssoClasif[,.SD, .SDcols = var033Nam]
  Inf033 <- data.table(Inf033)
  setnames(Inf033,concerneName,  paste0(concerneName, "Inf033"))
  Bet033066 <- climatAssoClasif[,.SD, .SDcols = concerneName]>climatAssoClasif[,.SD, .SDcols = var033Nam] & climatAssoClasif[,.SD, .SDcols = concerneName]<=climatAssoClasif[,.SD, .SDcols = var066Nam]
  Bet033066 <- data.table(Bet033066)
  setnames(Bet033066,concerneName,  paste0(concerneName, "Bet033066"))
  Sup066 <- climatAssoClasif[,.SD, .SDcols = concerneName]>climatAssoClasif[,.SD, .SDcols = var066Nam]
  Sup066 <- data.table(Sup066)
  setnames(Sup066,concerneName,  paste0(concerneName, "Sup066"))
  climatquantiles <- cbind(climatAssoClasif[, .SD, .SDcols = c("TypicalDay", "Period")], Inf033, Bet033066, Sup066)

  climatProbs <- climatquantiles[, lapply(.SD, function(X){
    sum(X)/.N
  }), by =  c("TypicalDay", "Period")]


  comb <- which(3:ncol(climatProbs)%%3 == 0) + 2
  comb2 <-  which(3:ncol(climatProbs)%%3 == 1) + 2
  comb3 <-  which(3:ncol(climatProbs)%%3 == 2) + 2

  allComb <- sapply(comb, function(X){
    sapply(comb2, function(Y){
      sapply(comb3, function(Z){
        c(X, Y, Z)
      }, simplify = FALSE)
    }, simplify = FALSE)
  }, simplify = FALSE)
  allComb <- matrix(unlist(allComb), ncol = 3, byrow = TRUE)

  nam <- apply(allComb, 1, function(X){
    paste(names(climatProbs)[X], collapse = "_")
  })

  for( i in 1:length(nam))
  {
    combSel <- allComb[i, ]
    multProb <- climatProbs[, .SD, .SDcols = combSel[1]] *  climatProbs[, .SD, .SDcols = combSel[2]] *
      climatProbs[, .SD, .SDcols = combSel[3]]
    climatProbs$tpcol <-  as.vector(unlist(multProb))
    setnames(climatProbs, "tpcol", nam[i])
  }

  climatProbs[, c(comb, comb2, comb3) := NULL]
  climatProbs <-  merge(quantiles033, climatProbs, by = c("Period", "TypicalDay"))
  climatProbs <-  merge(quantiles066, climatProbs, by = c("Period", "TypicalDay", "idDayType"))
  climatProbs
}


#' Data transformation (merge climat and classif files)
.climAssoClassif <- function(climat, classif, concerneName)
{

  climat[,c(concerneName) :=lapply(.SD, scale), .SDcols = concerneName]
  dayTypeAsso <- rbindlist(sapply(1:nrow(classif), function(X){
    typeDay <- classif[X]
    data.table(TypicalDay = typeDay$TypicalDay, Date = typeDay$dayIn[[1]][[1]]$Date,
               Period = typeDay$dayIn[[1]][[1]]$Period,
               idDayType = typeDay$idDayType)
  }, simplify = FALSE))
  climatAssoClasif <- merge(dayTypeAsso, climat, by = c("Date", "Period"))
  climatAssoClasif
}

#' Plot monotone
#'
#' @param climat \code{data.table} climat file, 2 first columns are Date and Period, others are params files
#' @param classif \code{data.table} output of \link{classifTipycalDay}
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
                                                                                idDayType))
}
