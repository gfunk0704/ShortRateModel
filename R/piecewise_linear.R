setClass(
    Class    = "PiecewiseLinear",
    contains = "INonparametricCurve"
)

PiecewiseLinear <- function() new(Class = "PiecewiseLinear")

setMethod(
    f          = "SetCurve",
    signature  = signature(curve = "PiecewiseLinear"),
    definition = .SetCurve(
        function(time, value) {
            slope <- diff(value) / diff(time)
            function(tau, rhsPos) {
                lhsPos <- rhsPos - 1L
                value[lhsPos] + slpoe * (tau - time[lhsPos])
            }
        }
    )
)

setMethod(
    f          = "SetIntegration",
    signature  = signature(curve = "PiecewiseLinear"),
    definition = .SetIntegration(
        function(time, value) {
            timeDiff       <- diff(time)
            valueDiff      <- diff(value)
            slope          <- valueDiff / timeDiff
            cumIntegration <- cumsum(c(value[1] * time[1], (value[-length(value)] + 0.5 * valueDiff) * timeDiff))

            list(rhsIntegration  = tail(cumIntegration, 1),
                 InRangeIntegral = function(tau, lhsPos, rhsPos) {
                     additionalInterval   <- tau - time[lhsPos]
                     valueHat             <- value[lhsPos] + slope[lhsPos] * additionalInterval
                     cumIntegration[lhsPos] + 0.5 * (valueHat + value[lhsPos]) * additionalInterval
                 })
        }
    )
)
