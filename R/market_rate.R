setClass(
    Class          = "MarketRate",
    contains       = "IQuote", 
    representation = representation(
        dayCount = "character",
        spotLag  = "integer"
    )
)

SetCashRateGenerator <- function(dayCount, spotLag, 
                                 unit = "percentage") {
    
    adjScalar <- .AdjustmentScalar(unit)
    
    function(rate, tenor) {
        switch(tenor,
               "ON" = {
                   tenor   <- "1D"
                   spotLag <- 0
               },
               "TN" = {
                   tenor   <- "2D"
                   spotLag <- 1
               })
        
        new(Class    = "MarketRate",
            dayCount = dayCount,
            period   = "NONE",
            rate     = rate * adjScalar,
            spotLag  = as.integer(spotLag),
            tenor    = tenor,
            type     = "CASH")
    }
}

SetIrsRateGenerator <- function(dayCount, period, spotLag, 
                                unit = "percentage") {
    
    adjScalar <- .AdjustmentScalar(unit)
    
    function(rate, tenor) {
        new(Class    = "MarketRate",
            dayCount = dayCount,
            period   = period,
            rate     = rate * adjScalar,
            spotLag  = as.integer(spotLag),
            tenor    = tenor,
            type     = "IRS")
    }
}
