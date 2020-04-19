setClass(
    Class          = "IEquilibriumModel",
    contains       = "IAffineModel",
    representation = representation(
        "VIRTUAL"
    )
)

setMethod(
    f          = "SetObjectiveFunction",
    signature  = signature(marketData = "MarketRateTermStructure",
                           model      = "IEquilibriumModel"),
    definition = function(marketData, model) {
        instrumentCashflowList <- .InstrumentCashflowList(marketData)
        timeList        <- lapply(X   = instrumentCashflowList,
                                  FUN = function(instrument) instrument[["time"]])
        cashflowList    <- lapply(X   = instrumentCashflowList,
                                  FUN = function(instrument) instrument[["cashflow"]])
        uniqueTime      <- unique(unlist(timeList))
        timeIndicesList <- lapply(X   = instrumentCashflowList,
                                  FUN = function(instrument) vapply(X         = instrument[["time"]],
                                                                    FUN       = function(time) which.max(time == uniqueTime),
                                                                    FUN.VALUE = integer(1)))
        function(parameter) {
            Parameter(model) <- parameter
            discountFactor   <- SetZcbClosedForm(model)(uniqueTime)
            squareError      <- mapply(FUN         = function(cashflow, timeIndices) {
                                           value <- sum(cashflow * discountFactor[timeIndices])
                                           value * value
                                       },
                                       cashflow    = cashflowList,
                                       timeIndices = timeIndicesList)

            mean(squareError)
        }
    }
)

