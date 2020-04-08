DeltaT <- function() {
    if (PricingDayCount() == "ACT360") {
        1 / 360
    } else {
        1 / 365
    }
}

GetPricingYearFraction <- function() {
    get(x     = ".PricingYearFraction",
        envir = .GlobalEnv)
}

SetPricingDayCount <- function(dayCount) {
    assign(x     = ".dayCount",
           value = dayCount,
           envir = .GlobalEnv)
    
    assign(x     = ".PricingYearFraction",
           value = SetYearFraction(dayCount),
           envir = .GlobalEnv)
}

PricingDayCount <- function() {
    get(x     = ".dayCount",
        envir = .GlobalEnv)
}


