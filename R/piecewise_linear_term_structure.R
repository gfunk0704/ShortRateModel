setClass(
    Class    = "PiecewiseLinearTermStructure",
    contains = "INonparametricTimeDependentShortRate"
)

InitializePiecewiseLinearTermStructure <- function() new(Class = "PiecewiseLinearTermStructure")

setMethod(
    f          = "SetInstForwardRate",
    signature  = signature(zeroRateTermStructure = "PiecewiseLinearTermStructure"),
    definition = .SetCurveFunction(
        function(maturity, value) {
            slope <- diff(value) / diff(maturity)
            function(tau, afterPos) {
                beforePos <- afterPos - 1
                value[beforePos] + slope[beforePos] * (tau - maturity[beforePos])
            }
        }
    )
)

setMethod(
    f          = ".SetIntegration",
    signature  = signature(zeroRateTermStructure = "PiecewiseLinearTermStructure"),
    definition = function(zeroRateTermStructure) {
        maturity       <- zeroRateTermStructure@maturity
        value          <- zeroRateTermStructure@value
        nPoint         <- length(value)
        matDiff        <- diff(maturity)
        valueDiff      <- diff(value)
        slope          <- valueDiff / matDiff
        minMat         <- maturity[1]
        maxMat         <- tail(maturity, 1)
        cumIntegration <- cumsum(c(value[1] * maturity[1], (value[-nPoint] + 0.5 * valueDiff) * matDiff))

        function(tau) {
            lhs         <- tau <= minMat
            rhs         <- tau >= maxMat
            inRange     <- !(lhs | rhs)
            integration <- numeric(length(tau))

            if (any(lhs)) {
                integration[lhs] <- tau[lhs] * value[1]
            }

            if (any(rhs)) {
                integration[rhs] <- cumIntegration[nPoint] + (tau[rhs] - maxMat) * value[nPoint]
            }

            if (any(inRange)) {
                targetMat <- tau[inRange]
                afterPos  <- vapply(X         = targetMat,
                                    FUN       = function(targetMat) which.max(maturity > targetMat),
                                    FUN.VALUE = integer(1L))

                beforePos            <- afterPos - 1L
                additionalInterval   <- targetMat - maturity[beforePos]
                valueHat             <- value[beforePos] + slope[beforePos] * additionalInterval
                integration[inRange] <- cumIntegration[beforePos] + 0.5 * (valueHat + value[beforePos]) * additionalInterval
            }

            integration
        }
    }
)
