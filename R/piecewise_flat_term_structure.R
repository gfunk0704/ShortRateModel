setClass(
    Class    = "PiecewiseFlatTermStructure",
    contains = "INonparametricTimeDependentShortRate"
)

InitializePiecewiseFlatTermStructure <- function() new(Class = "PiecewiseFlatTermStructure")

setMethod(
    f          = "SetInstForwardRate",
    signature  = signature(zeroRateTermStructure = "PiecewiseFlatTermStructure"),
    definition = .SetCurveFunction(
        function(maturity, value) {
            function(tau, afterPos) value[afterPos]
        }
    )
)

setMethod(
    f          = ".SetIntegration",
    signature  = signature(zeroRateTermStructure = "PiecewiseFlatTermStructure"),
    definition = function(zeroRateTermStructure) {
        maturity       <- zeroRateTermStructure@maturity
        value          <- zeroRateTermStructure@value
        lhsMat         <- maturity[1]
        rhsMat         <- tail(maturity, 1)
        lhsVal         <- value[1]
        rhsVal         <- tail(value, 1)
        cumIntegration <- cumsum(c(maturity[1], diff(maturity)) * value)
        rhsIntegration <- tail(cumIntegration, 1)

        function(tau) {
            lhsPos      <- tau <= lhsMat
            rhsPos      <- tau >= rhsMat
            inRange     <- !(lhsPos | rhsPos)
            integration <- numeric(length(tau))

            if (any(lhsPos)) {
                integration[lhsPos] <- lhsVal * tau[lhsPos]
            }

            if (any(rhsPos)) {
                integration[rhsPos] <- rhsVal * (tau[rhsPos] - rhsMat) + rhsIntegration
            }

            if (any(inRange)) {
                afterPos  <- vapply(X         = tau[inRange],
                                    FUN       = function(tau) which.max(maturity >= tau),
                                    FUN.VALUE = integer(1))
                beforePos <- afterPos - 1

                integration[inRange] <- cumIntegration[beforePos] + value[afterPos] * (tau[inRange] - maturity[beforePos])
            }

            integration
        }
    }
)
