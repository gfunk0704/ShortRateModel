setClass(
    Class          = "GaussianModel",
    contains       = "IEquilibriumModel",
    representation = representation(
        a     = "numeric",
        sigma = "numeric"
    ),
    prototype      = prototype(
        a            = NaN,
        sigma        = NaN,
        initialValue = NaN
    )
)

InitializeGaussianModel <- function() new(Class = "GaussianModel")

setMethod(
    f          = "NParameter",
    signature  = signature(model = "GaussianModel"),
    definition = function(model) 3L
)

setMethod(
    f          = "Parameter",
    signature  = signature(model = "GaussianModel"),
    definition = function(model) {
        par        <- c(model@a, model@sigma, model@initialValue)
        names(par) <- c("a", "sigma", "r0")
        par
    }
)

setMethod(
    f          = "Parameter<-",
    signature  = signature(model = "GaussianModel",
                           value = "numeric"),
    definition = function(model, value) {
        model@a            <- value[1]
        model@sigma        <- value[2]
        model@initialValue <- value[3]
        model
    }
)

setMethod(
    f          = "ParameterLowerBound",
    signature  = signature(model = "GaussianModel"),
    definition = function(model) rep.int(sqrt(.Machine$double.eps), 3)
)

setMethod(
    f          = "ParameterUpperBound",
    signature  = signature(model = "GaussianModel"),
    definition = function(model) c(10, 1, 1)
)

setMethod(
    f          = ".SetA",
    signature  = signature(model = "GaussianModel"),
    definition = function(model) {
        sigma     <- model@sigma
        a         <- model@a
        aTimesTwo <- 2 * a
        firstCoef <- -sigma * sigma / (2 * aTimesTwo)

        function(startTime, endTime) {
            tau <- endTime - startTime
            firstCoef * (aTimesTwo * tau - exp(-aTimesTwo * tau) + 4 * exp(-a * tau) - 3)
        }
    }
)

setMethod(
    f          = ".SetB",
    signature  = signature(model = "GaussianModel"),
    definition = function(model) {
        a <- model@a

        function(startTime, endTime) (1 - exp(-a * (endTime - startTime))) / a
    }
)
