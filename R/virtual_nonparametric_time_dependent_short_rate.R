setClass(
  Class          = "INonparametricTimeDependentShortRate",
  contains       = "IZeroRateTermStructure",
  representation = representation(
    value = "numeric",
    "VIRTUAL"
  ),
  prototype     = prototype(
      value    = numeric(0),
      maturity = numeric(0)
  )
)

.SetCurveFunction <- function(SetInterpolationEngine) {
    function(zeroRateTermStructure) {
        maturity    <- zeroRateTermStructure@maturity
        value       <- zeroRateTermStructure@value
        lhsMat      <- maturity[1]
        rhsMat      <- tail(maturity, 1)
        lhsVal      <- value[1]
        rhsVal      <- tail(value, 1)
        Interpolate <- SetInterpolationEngine(maturity, value)

        function(tau) {
            lhsPos   <- tau <= lhsMat
            rhsPos   <- tau >= rhsMat
            inRange  <- !(lhsPos | rhsPos)
            valueHat <- numeric(length(tau))
            valueHat[lhsPos] <- lhsVal
            valueHat[rhsPos] <- rhsVal

            if (any(inRange)) {
                afterPos <- vapply(X         = tau[inRange],
                                   FUN       = function(tau) which.max(maturity >= tau),
                                   FUN.VALUE = integer(1))

                valueHat[inRange] <- Interpolate(tau[inRange], afterPos)
            }

            valueHat
        }
    }
}

setMethod(
    f          = "Calibrate",
    signature  = signature(marketData = "MarketRateTermStructure",
                           model      = "INonparametricTimeDependentShortRate"),
    definition = function(marketData, model) {
        instrumentCashflowList <- .InstrumentCashflowList(marketData)
        firstCashflow          <- instrumentCashflowList[[1]]
        model@maturity         <- c(model@maturity, tail(firstCashflow[["time"]], 1))
        model@value            <- c(model@value, log(tail(firstCashflow[["cashflow"]], 1)) / tail(firstCashflow[["time"]], 1))

        for (singleCashflow in instrumentCashflowList[-1]) {

            time     <- singleCashflow[["time"]]
            maturity <- tail(time, 1)
            cashflow <- singleCashflow[["cashflow"]]
            value    <- uniroot(f        = function(guess) {
                                    Data(model) <- ZeroRatePoint(maturity = maturity,
                                                                 value    = guess)
                                    sum(cashflow * SetZeroCouponBondPrice(model)(time))
                                },
                                interval = c(-1, 1))[["root"]]

            Data(model) <- ZeroRatePoint(maturity = maturity,
                                         value    = value)

        }

        model
    }
)

setGeneric(
    name = ".SetIntegration",
    def = function(zeroRateTermStructure) {
        standardGeneric(".SetIntegration")
    }
)

setMethod(
    f          = "SetZeroCouponBondPrice",
    signature  = signature(zeroRateTermStructure = "INonparametricTimeDependentShortRate"),
    definition = function(zeroRateTermStructure) {
        Integration <- .SetIntegration(zeroRateTermStructure)
        function(tau) exp(-Integration(tau))
    }
)

setMethod(
    f          = "SetZeroRate",
    signature  = signature(zeroRateTermStructure = "INonparametricTimeDependentShortRate"),
    definition = function(zeroRateTermStructure) {
        ZeroCouponBondPrice <- SetZeroCouponBondPrice(zeroRateTermStructure)
        function(tau) -log(ZeroCouponBondPrice(tau)) / tau
    }
)

setMethod(
    f          = "Data<-",
    signature  = signature(object = "INonparametricTimeDependentShortRate",
                           value  = "ZeroRatePoint"),
    definition = function(object, value) {
        object@value    <- c(object@value, value@value)
        object@maturity <- c(object@maturity, value@maturity)
        object
    }
)
