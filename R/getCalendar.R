#' Create a vector of date from a date begin and a date end
#'
#' @param dateBegin \code{character or date}, date, YYYY-MM-DD
#' @param dateEnd \code{character or date}, date, YYYY-MM-DD
#'
#' @examples
#'
#' \dontrun{
#' getSequence("2017-01-01", "2017-12-31")
#' }
#'
#' @export
getSequence <- function(dateBegin, dateEnd){
  allDay <- seq(as.Date(dateBegin), as.Date(dateEnd), by = "day")
  allDay
}


#' Define a calender which season
#'
#' @param dates \code{character or date}, vector of dates YYYY-MM-DD, can be generate which \link{getSequence}
#' @param interSeasonBegin \code{character or date}, date, YYYY-MM-DD, begin of interSaison
#' @param interSeasonEnd \code{character or date}, date, YYYY-MM-DD, end of interSaison
#' @param dayInWeekend \code{numeric}, indice of weekend day
#' @param dayExclude  \code{character or date}, date, YYYY-MM-DD, day to exclude
#' @param holiday \code{character}, see \link[timeDate]{holiday} for holidays names, holidays days default :
#' NewYearsDay,GoodFriday,EasterSunday,
#' EasterMonday,LaborDay,FRAscension,Pentecost,PentecostMonday,
#' FRAssumptionVirginMary,FRAllSaints,FRArmisticeDay,
#' ChristmasEve,ChristmasDay,BoxingDay,DENewYearsEve
#'
#' @examples
#'
#' \dontrun{
#' dates <- getSequence("2017-01-01", "2017-12-31")
#' interSeasonBegin <- c("2017-03-01", "2017-09-01")
#' interSeasonEnd <- c("2017-05-01", "2017-11-01")
#' dayInWeekend = c(6, 7)
#' getCalendar(dates, interSeasonBegin, interSeasonEnd)
#' }
#'
#' @import timeDate
#'
#' @export
getCalendar <- function(dates,
                    interSeasonBegin,
                    interSeasonEnd,
                    dayExclude = NULL,
                    holiday = c("NewYearsDay",
                                "GoodFriday",
                                "EasterSunday",
                                "EasterMonday",
                                "LaborDay",
                                "FRAscension",
                                "Pentecost",
                                "PentecostMonday",
                                "FRAssumptionVirginMary",
                                "FRAllSaints",
                                "FRArmisticeDay",
                                "ChristmasEve",
                                "ChristmasDay",
                                "BoxingDay",
                                "DENewYearsEve"),
                    dayInWeekend = c(6, 7)){

  weekendDay <- function(day, dayInWeekend, holiday){
    daywe <- day[ifelse(data.table::wday(day)==1,7,data.table::wday(day)-1)%in%dayInWeekend]


    allYeay <- unique(year(day))
   HolidayDay <- as.Date(holiday(allYeay, Holiday = holiday))
   dayHilyday <- day[day%in%HolidayDay]

   unique(sort(c(daywe, dayHilyday)))}

  interSeasonBegin <- as.Date(interSeasonBegin)
  interSeasonEnd <- as.Date(interSeasonEnd)

  if(length(interSeasonBegin) != length(interSeasonEnd)){
    stop("You must specify a end begin and end for each intersaison, (interSeasonBegin and interSeasonEnd
         must have same length")}

  if(!(all(interSeasonBegin<interSeasonEnd))){
    stop("All interSeasonBegin must be > to corresponding interSeasonEnd")
  }

  dates <- as.Date(dates)
  allDay <- seq(min(dates), max(dates), by = "day")

  dayRemoveVectorIn <- allDay[!allDay%in%dates]

  if(length(dates)>0){
    if(is.null(dayExclude))
    {
      dayExclude <- dayRemoveVectorIn
    }else{
      dayExclude <- as.Date(dayExclude)
      dayExclude <- unique(c(dayExclude, dayRemoveVectorIn))

    }
  }

  #Trouver les jours de changment d'heure
  MarsChangeHour <- max(allDay[which(month(allDay)==3 & wday(allDay) == 1)])
  OctChangeHour <- max(allDay[which(month(allDay)==10 & wday(allDay) == 1)])
 if(is.null(dayExclude))
 {
  dayExclude <- c(MarsChangeHour, OctChangeHour)
 }else{
   dayExclude <- as.Date(dayExclude)
   dayExclude <- unique(c(dayExclude, MarsChangeHour, OctChangeHour))

 }
  calendarReturn <- list()

  intaisaison <- data.frame(begin = as.Date(interSeasonBegin), end = as.Date(interSeasonEnd))

  interSaisonDay <- sapply(1:nrow(intaisaison), function(X){
    Y <- intaisaison[X,]
    seq(as.Date(Y[,1]), as.Date(Y[,2]), by = "day")
  }, simplify = FALSE)

  #Be carefull unlist break date class.
  interSaisonDay <- do.call("c", interSaisonDay)

  interSaisonDayWeekend <- weekendDay(interSaisonDay, dayInWeekend, holiday)
  calendarReturn$interSaisonWe <- interSaisonDayWeekend
  calendarReturn$interSaisonWd <- interSaisonDay[!interSaisonDay%in%interSaisonDayWeekend]


  saisonDay <- which(!allDay%in%interSaisonDay)

  breakS <- which(diff(saisonDay)!=1)+1
  allSaison <- list()
  saisonAffect <- 0
  for(i in 0:(length(breakS))){
    if(i == length(breakS)){
      CurrentSaison <-(breakS[i]):length(saisonDay)
      saisonAffect <- saisonAffect + 1
      allSaison[[saisonAffect]] <- saisonDay[CurrentSaison]
    }else{

      if(i == 0){
        CurrentSaison <- 1:(breakS[1]-1)
      }else{
        CurrentSaison <-breakS[i]:(breakS[i+1]-1)
      }
      saisonAffect <- saisonAffect + 1
      allSaison[[saisonAffect]] <- saisonDay[CurrentSaison]
    }
  }

  Saison <- lapply(allSaison, function(X){
    allDay[X]
  })


  monthSWinter <- c(1:4, 10:12)
  monthSSummer <- 5:9


  WS <- unlist(lapply(Saison, function(X){
    nbDayInWinter <- sum(month(X)%in%monthSWinter)
    nbDayInSummer <- sum(month(X)%in%monthSSummer)
    if(nbDayInWinter>nbDayInSummer){
      "W"
    }else{
      "S"
    }
  }))

  winter <- do.call("c",(Saison[which(WS == "W")]))

  winterWe <- weekendDay(winter, dayInWeekend, holiday)
  calendarReturn$winterWe <- winterWe
  calendarReturn$winterWd <- winter[!winter%in%winterWe]

  summer <-  do.call("c",(Saison[which(WS == "S")]))


  summerWe <- weekendDay(summer, dayInWeekend, holiday)
  calendarReturn$summerWe <- summerWe
  calendarReturn$summerWd <- summer[!summer%in%summerWe]

  calendarReturn <- lapply(calendarReturn, function(X){
    X[!X%in%dayExclude]
  })
  calendarReturn
}
