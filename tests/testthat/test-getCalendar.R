context("Function getCalendar")

test_that("getSequence works", {
  
  dates <- getSequence("2015-11-01", "2017-01-20")
  expect_true(as.Date("2015-11-01") %in% dates)
  expect_true(as.Date("2015-11-02") %in% dates)
  expect_true(as.Date("2016-06-15") %in% dates)
  expect_true(as.Date("2017-01-20") %in% dates)
  expect_false(as.Date("2015-10-30") %in% dates)
  expect_false(as.Date("2017-01-21") %in% dates)
  
})


test_that("getCalendar works", {
  
  # run function
  dates <- getSequence("2016-01-01", "2016-12-31")
  interSeasonBegin <- c("2016-03-01", "2016-10-01")
  interSeasonEnd <- c("2016-05-31", "2016-10-31")
  calendar <- getCalendar(dates, interSeasonBegin, interSeasonEnd)
  
  # test that calendar type and lenght are ok
  expect_type(calendar, "list")
  expect_length(calendar, 6)
  
  # test one day for each class
  expect_true(as.Date("2016-10-15") %in% calendar$interSeasonWe)
  expect_true(as.Date("2016-05-26") %in% calendar$interSeasonWd)
  expect_true(as.Date("2016-01-09") %in% calendar$winterWe)
  expect_true(as.Date("2016-12-09") %in% calendar$winterWd)
  expect_true(as.Date("2016-07-24") %in% calendar$summerWe)
  expect_true(as.Date("2016-08-30") %in% calendar$summerWd)
  
  # test a few holidays
  expect_false(as.Date("2016-11-11") %in% calendar$winterWd)
  expect_false(as.Date("2016-03-28") %in% calendar$interSeasonWd)
  #does not work (issure #28)
  #calendar_wo_holidays <- getCalendar(dates, interSeasonBegin, interSeasonEnd, holiday = c())
  calendar_wo_holidays <- getCalendar(dates, interSeasonBegin, interSeasonEnd, holiday = c("NewYearsDay"))
  expect_true(as.Date("2016-11-11") %in% calendar_wo_holidays$winterWd)
  expect_true(as.Date("2016-03-28") %in% calendar_wo_holidays$interSeasonWd)
  
  # test dayExclude functionality
  calendar_wo_november <- getCalendar(dates, interSeasonBegin, interSeasonEnd, dayExclude = getSequence("2016-11-01", "2016-11-30"))
  expect_true(as.Date("2016-11-22") %in% calendar$winterWd)
  expect_false(as.Date("2016-11-22") %in% calendar_wo_november$winterWd)
  
  # test dayInWeekEnd functionnality
  calendar_with_3days_WE <- getCalendar(dates, interSeasonBegin, interSeasonEnd,  dayInWeekend = c(1, 6, 7))
  expect_false(as.Date("2016-07-11") %in% calendar$summerWe) # monday
  expect_true(as.Date("2016-07-11") %in% calendar_with_3days_WE$summerWe) # monday
})

test_that("getCalendar returns error when expected", {
  dates <- getSequence("2016-01-01", "2016-12-31")
  # empty arguments
  expect_error(getCalendar(dates, interSeasonBegin = "2016-03-01", interSeasonEnd = c()))
  expect_error(getCalendar(dates, interSeasonBegin = c(), interSeasonEnd = "2016-05-30"))
  # interseason boundaries out of calendar
  expect_error(getCalendar(dates, interSeasonBegin = "2015-03-01", interSeasonEnd = "2016-05-30"))
  expect_error(getCalendar(dates, interSeasonBegin = "2016-03-01", interSeasonEnd = "2017-05-30"))
  # interseason end and beginning have different sizes
  expect_error(getCalendar(dates, interSeasonBegin = c("2016-03-01", "2016-10-01"), interSeasonEnd = "2016-05-30"))
  # end before beginning
  expect_error(getCalendar(dates, interSeasonBegin = c("2016-03-01", "2016-10-01"), interSeasonEnd =  c("2016-05-31", "2016-09-15")))
  # calendar empty
  expect_error(getCalendar(dates, interSeasonBegin = c("2016-03-01", "2016-10-01"), interSeasonEnd =  c("2016-05-31", "2016-10-15"), dayExclude = dates))
  # one season empty
  expect_warning(getCalendar(dates, interSeasonBegin = c("2016-03-01", "2016-10-01"), interSeasonEnd =  c("2016-05-31", "2016-10-15"), dayExclude = getSequence("2016-04-01", "2016-10-15")))
})  


test_that("getCalendar return logical result with holidays", {
  
  calendar <- getCalendar(dates = getSequence("2018-01-01", "2018-12-31"), 
                          interSeasonBegin = "2018-03-01",
                          interSeasonEnd = "2018-05-31", holiday = NULL)
  expect_true(as.Date("2018-01-01")%in%calendar$winterWd)
  calendar <- getCalendar(dates = getSequence("2018-01-01", "2018-12-31"), 
                          interSeasonBegin = "2018-03-01",
                          interSeasonEnd = "2018-05-31", holiday = "NewYearsDay")
  expect_true(as.Date("2018-01-01")%in%calendar$winterWe)
  
})