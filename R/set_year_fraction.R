SetYearFraction <- function(dayCount) {
    dayCountEnum <- switch(toupper(dayCount),
                           "ACT360"  = 0L,
                           "ACT365F" = 1L,
                           "ACT365"  = 2L,
                           "ACTACT"  = 2L,
                           "30360"   = 6L)
    Vectorize(function(endDate, startDate = ValueDate()) RQuantLib::yearFraction(startDate, endDate, dayCountEnum))
}
