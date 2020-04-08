setClass(
    Class          = "MarketRateTermStructure",
    contains       = "IQuoteTermStructure",
    representation = representation(
        dayCount = "character",
        spotLag  = "integer"
    ),
    prototype      = prototype(
        dayCount = character(0),
        period   = character(0),
        rate     = numeric(0),
        spotLag  = integer(0),
        tenor    = character(0),
        type     = character(0)
    )
)

MarketRateTermStructureFromCsv <- function(file, calendar,
                                           unit = "percentage",
                                           ...) {
    rawData <- read.csv(file             = file,
                        stringsAsFactors = FALSE,
                        ...)

    rate    <- .AdjustmentScalar(unit) * rawData[["rate"]]
    spotLag <- rawData[["spotLag"]]
    tenor   <- rawData[["tenor"]]

    isOvernight          <- tenor == "ON"
    isTom                <- tenor == "TN"
    spotLag[isOvernight] <- 0
    spotLag[isTom]       <- 1
    tenor[isOvernight]   <- "1D"
    tenor[isTom]         <- "2D"

    new(Class    = "MarketRateTermStructure",
        calendar = calendar,
        dayCount = rawData[["dayCount"]],
        period   = rawData[["period"]],
        rate     = rate,
        spotLag  = as.integer(spotLag),
        tenor    = tenor,
        type     = rawData[["type"]])
}

InitializeMarketRateTermStructure <- function(calendar) {
    new(Class    = "MarketRateTermStructure",
        calendar = calendar)
}

setGeneric(
    name = ".InstrumentCashflowList",
    def  = function(termStructure) {
        standardGeneric(".InstrumentCashflowList")
    }
)
setMethod(
    f          = ".InstrumentCashflowList",
    signature  = signature(termStructure = "MarketRateTermStructure"),
    definition = function(termStructure) {
        calendar            <- termStructure@calendar
        ConvertTenorToDate  <- SetTenorConvertor(calendar)
        DateSequence        <- SetDateSequence(ValueDate(), ConvertTenorToDate)
        PricingYearFraction <- GetPricingYearFraction()
        mapply(FUN      = function(dayCount, period, rate, spotLag, tenor) {
                   YearFraction <- SetYearFraction(dayCount)
                   startDate    <- ValueDate()

                   while (spotLag > 0) {
                       startDate <- startDate %m+% days(1)

                       if (isBusinessDay (calendar, startDate)) {
                           spotLag <- spotLag - 1
                       }
                   }

                   if (period == "") {
                       dates <- c(startDate, ConvertTenorToDate(tenor))
                   } else {
                       dates    <- DateSequence(tenor, period)
                       dates[1] <- startDate
                   }
                   compoundingPeriod          <- diff(YearFraction(dates))
                   time                       <- PricingYearFraction(dates)
                   cashflow                   <- c(-1, compoundingPeriod * rate)
                   cashflow[length(cashflow)] <- 1 + cashflow[length(cashflow)]
                   list(time              = time,
                        cashflow          = cashflow,
                        compoundingPeriod = compoundingPeriod)
               },
               dayCount  = termStructure@dayCount,
               period    = termStructure@period,
               rate      = termStructure@rate,
               spotLag   = termStructure@spotLag,
               tenor     = termStructure@tenor,
               SIMPLIFY  = FALSE,
               USE.NAMES = FALSE)
    }
)

setMethod(
    f          = "Data<-",
    signature  = signature(object = "MarketRateTermStructure",
                           value  = "MarketRate"),
    definition = function(object, value) {
        object@dayCount <- value@dayCount
        object@period   <- value@period
        object@rate     <- value@rate
        object@spotLag  <- value@spotLag
        object@tenor    <- value@tenor
        object@type     <- value@type
        object
    }
)

setMethod(
    f          = "show",
    signature  = signature(object = "MarketRateTermStructure"),
    definition = function(object) {
        cat("calendar: ", object@calendar, "\n\n",
            "term structure:\n",
            sep = "")

        print(data.frame(tenor    = object@tenor,
                         type     = object@type,
                         rate     = object@rate,
                         spotLag  = object@spotLag,
                         dayCount = object@dayCount,
                         period   = object@period ))

        invisible()
    }
)

setMethod(
    f          = "SortTermStruucture",
    signature  = signature(termStructure = "MarketRateTermStructure"),
    definition = function(termStructure) {
        quoteOrder      <- QuoteOrder(termStructure)
        object@dayCount <- object@dayCount[quoteOrder]
        object@period   <- object@period[quoteOrder]
        object@rate     <- object@rate[quoteOrder]
        object@spotLag  <- object@spotLag[quoteOrder]
        object@tenor    <- object@tenor[quoteOrder]
        object@type     <- object@type[quoteOrder]
        object
    }
)
