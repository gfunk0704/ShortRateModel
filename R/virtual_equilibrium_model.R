setClass(
    Class          = "IEquilibriumModel",
    contains       = "IAffineModel",
    representation = representation(
        "VIRTUAL"
    )
)

setMethod(
    f          = ".FittingChart",
    signature  = signature(marketData = "MarketRateTermStructure",
                           model      = "IEquilibriumModel"),
    definition = function(marketData, model) {
        marketRate          <- marketData@rate
        PricingYearFraction <- GetPricingYearFraction()
        ConvertTenorToDate  <- SetTenorConvertor(marketData@calendar)
        maturity            <- vapply(X         = marketData@tenor,
                                      FUN       = function(tenor) PricingYearFraction(ConvertTenorToDate(tenor)),
                                      FUN.VALUE = numeric(1))
        ZcbClosedForm       <- SetZcbClosedForm(model)
        modelImpliedRate    <- vapply(X         = .InstrumentCashflowList(marketData),
                                      FUN       = function(instrumentCashflow) {
                                          discountFactor    <- ZcbClosedForm(instrumentCashflow[["time"]])
                                          compoundingPeriod <- instrumentCashflow[["compoundingPeriod"]]
                                          tailIndex         <- length(compoundingPeriod) + 1
                                          impliedRate       <- uniroot(f        = function(rate) {
                                                                           cashflow            <- c(-1, compoundingPeriod * rate)
                                                                           cashflow[tailIndex] <- cashflow[tailIndex] + 1
                                                                           sum(discountFactor * cashflow)
                                                                       },
                                                                       interval = c(-0.1, 0.1))[["root"]]
                                      },
                                      FUN.VALUE = numeric(1))
        maxRate      <- max(c(marketRate, modelImpliedRate))
        minRate      <- min(c(marketRate, modelImpliedRate))
        isCash       <- marketData@type == "CASH"
        cashMaturity <- maturity[isCash]
        irsMaturity  <- maturity[!isCash]
        plot(x    = maturity,
             y    = marketRate,
             ylim = c(0.99 * minRate, 1.01 * maxRate),
             type = "n",
             xlab = "Maturity Years",
             ylab = "Rate")

        points(x   = cashMaturity,
               y   = marketRate[isCash],
               col = "red",
               pch = 1)

        points(x   = irsMaturity,
               y   = marketRate[!isCash],
               col = "red",
               pch = 2)

        points(x   = cashMaturity,
               y   = modelImpliedRate[isCash],
               col = "blue",
               pch = 1)

        points(x   = irsMaturity,
               y   = modelImpliedRate[!isCash],
               col = "blue",
               pch = 2)
    }
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

