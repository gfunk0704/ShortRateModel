setClass(
    Class          = "NelsonSiegel",
    contains       = c("IZeroRateTermStructure", "IParametricModel"),
    representation = representation(
        beta0   = "numeric",
        beta1   = "numeric",
        beta2   = "numeric",
        lambda1 = "numeric"
    ),
    prototype     = prototype(
        beta0    = NaN,
        beta1    = NaN,
        beta2    = NaN,
        lambda1  = NaN,
        maturity = numeric(0)
    )
)

InitializeNelsonSiegel <- function() new(Class = "NelsonSiegel")

setMethod(
    f          = "Calibrate",
    signature  = signature(marketData = "MarketRateTermStructure",
                           model      = "NelsonSiegel"),
    definition = function(marketData, model) {
        model          <- callNextMethod(marketData, model)
        model@maturity <- GetPricingYearFraction()(SetTenorConvertor(marketData@calendar)(marketData@tenor))
        model
    }
)

setMethod(
    f          = "NParameter",
    signature  = signature(model = "NelsonSiegel"),
    definition = function(model) 4L
)

setMethod(
    f          = "Parameter",
    signature  = signature(model = "NelsonSiegel"),
    definition = function(model) {
        par        <- c(model@beta0, model@beta1, model@beta2, model@lambda1)
        names(par) <- c("beta0", "beta1", "beta2", "lambda1")
        par
    }
)

setMethod(
    f          = "Parameter<-",
    signature  = signature(model = "NelsonSiegel",
                           value = "numeric"),
    definition = function(model, value) {
        model@beta0   <- value[1]
        model@beta1   <- value[2]
        model@beta2   <- value[3]
        model@lambda1 <- value[4]
        model
    }
)

setMethod(
    f          = "ParameterConstraints",
    signature  = signature(model = "NelsonSiegel"),
    definition = function(model) function(par) -par[1] - par[2]
)

setMethod(
    f          = "ParameterLowerBound",
    signature  = signature(model = "NelsonSiegel"),
    definition = function(model) c(0, -0.15, -0.3, 0)
)

setMethod(
    f          = "ParameterUpperBound",
    signature  = signature(model = "NelsonSiegel"),
    definition = function(model) c(0.15, 0.3, 0.3, 30)
)

setMethod(
    f          = "SetObjectiveFunction",
    signature  = signature(marketData = "MarketRateTermStructure",
                           model      = "NelsonSiegel"),
    definition = function(marketData, model) {
        instrumentCashflowList <- .InstrumentCashflowList(marketData)
        timeList               <- lapply(X   = instrumentCashflowList,
                                         FUN = function(singleCashflow) singleCashflow[["time"]])
        cashflowList           <- lapply(X   = instrumentCashflowList,
                                         FUN = function(singleCashflow) singleCashflow[["cashflow"]])
        uniqueTime             <- unique(unlist(timeList))
        timeIndicesList        <- lapply(X   = instrumentCashflowList,
                                         FUN = function(singleCashflow) vapply(X         = singleCashflow[["time"]],
                                                                               FUN       = function(time) which.max(time == uniqueTime),
                                                                               FUN.VALUE = integer(1)))

        function(par) {
            Parameter(model)    <- par
            discountFactor      <- SetZeroCouponBondPrice(model)(uniqueTime)
            weightedSquareError <- mapply(FUN         = function(cashflow, timeIndices) {
                                              value <- sum(cashflow * discountFactor[timeIndices])
                                              value * value
                                          },
                                          cashflow    = cashflowList,
                                          timeIndices = timeIndicesList)
            mean(weightedSquareError)
        }
    }
)

setMethod(
    f          = "SetInstForwardRate",
    signature  = signature(zeroRateTermStructure = "NelsonSiegel"),
    definition = function(zeroRateTermStructure) {
        beta0   <- zeroRateTermStructure@beta0
        beta1   <- zeroRateTermStructure@beta1
        beta2   <- zeroRateTermStructure@beta2
        lambda1 <- zeroRateTermStructure@lambda1

        function(tau) {
            tauDivLambda <- tau / lambda1
            beta0 + exp(-tauDivLambda) * (beta1 + beta2 * tauDivLambda)
        }
    }
)

setMethod(
    f          = "SetZeroCouponBondPrice",
    signature  = signature(zeroRateTermStructure = "NelsonSiegel"),
    definition = function(zeroRateTermStructure) {
        ZeroRate <- SetZeroRate(zeroRateTermStructure)

        function(tau) exp(-ZeroRate(tau) * tau)
    }
)

setMethod(
    f          = "SetZeroRate",
    signature  = signature(zeroRateTermStructure = "NelsonSiegel"),
    definition = function(zeroRateTermStructure) {
        beta0    <- zeroRateTermStructure@beta0
        beta2    <- zeroRateTermStructure@beta2
        betaSum  <- zeroRateTermStructure@beta1 + beta2
        lambda1  <- zeroRateTermStructure@lambda1

        function(tau) {
            tauDivLambda <- tau / lambda1
            expTerm      <- exp(-tauDivLambda)
            ifelse(test = tau > 0,
                   yes  = beta0 + betaSum * (1 - expTerm) / tauDivLambda - beta2 * expTerm,
                   no   = beta0)
        }
    }
)
