setClass(
    Class    = "PiecewiseFlat",
    contains = "INonparametricCurve"
)

PiecewiseFlat <- function() new(Class = "PiecewiseFlat")

setMethod(
    f          = "SetCurve",
    signature  = signature(curve = "PiecewiseFlat"),
    definition = .SetCurve(
        function(time, value) {
            function(tau, rhsPos) value[rhsPos]
        }
    )
)

setMethod(
    f          = "SetIntegration",
    signature  = signature(curve = "PiecewiseFlat"),
    definition = .SetIntegration(
        function(time, value) {
            cumIntegration <- cumsum(c(time[1], diff(time)) * value)
            list(rhsIntegration  = tail(cumIntegration, 1),
                 InRangeIntegral = function(tau, lhsPos, rhsPos) cumIntegration[lhsPos] + value[rhsPos] * (tau - time[lhsPos]))
        }
    )
)

