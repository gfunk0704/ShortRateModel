setClass(
    Class          = "Cir",
    contains       = "IEquilibriumModel",
    representation = representation(
        kappa = "numeric",
        theta = "numeric",
        sigma = "numeric",
        h     = "numeric",
        psi   = "numeric"
    ),
    prototype      = prototype(
        initialValue = NaN,
        kappa        = NaN,
        theta        = NaN,
        sigma        = NaN,
        h            = NaN,
        psi          = NaN
    )
)

InitializeCir <- function() new(Class = "Cir")

setMethod(
    f          = "NParameter",
    signature  = signature(model = "Cir"),
    definition = function(model) 4L
)

setMethod(
    f          = "Parameter",
    signature  = signature(model = "Cir"),
    definition = function(model) {
        par        <- c(model@kappa, model@theta, model@sigma, model@initialValue)
        names(par) <- c("kappa", "theta", "sigma", "r0")
        par
    }
)

setMethod(
    f          = "Parameter<-",
    signature  = signature(model = "Cir",
                           value = "numeric"),
    definition = function(model, value) {
        model@kappa        <- value[1]
        model@theta        <- value[2]
        model@sigma        <- value[3]
        model@initialValue <- value[4]
        sigmaSqr           <- value[3] * value[3]
        model@h            <- sqrt(value[1] * value[1] + 2 * sigmaSqr)
        model@psi          <- (value[1] + model@h) / sigmaSqr
        model
    }
)

setMethod(
    f          = "ParameterLowerBound",
    signature  = signature(model = "Cir"),
    definition = function(model) rep.int(sqrt(.Machine$double.eps), 4)
)

setMethod(
    f          = "ParameterUpperBound",
    signature  = signature(model = "Cir"),
    definition = function(model) c(10, 1, 1, 1)
)

setMethod(
    f          = ".SetA",
    signature  = signature(model = "Cir"),
    definition = function(model) {
        kappa      <- model@kappa
        theta      <- model@theta
        sigma      <- model@sigma
        h          <- model@h
        kappaPlusH <- kappa + h
        hTimesTwo  <- 2 * h
        powerCoef  <- 2 * kappa * theta / (sigma * sigma)

        function(startTime, endTime) {
            tau <- endTime - startTime
            (hTimesTwo * exp(kappaPlusH * tau * 0.5) / (hTimesTwo + kappaPlusH * (exp(h * tau) - 1)))^powerCoef
        }
    }
)

setMethod(
    f          = ".SetB",
    signature  = signature(model = "Cir"),
    definition = function(model) {
        kappa      <- model@kappa
        theta      <- model@theta
        sigma      <- model@sigma
        h          <- model@h
        kappaPlusH <- kappa + h
        hTimesTwo  <- 2 * h

        function(startTime, endTime) {
            tau             <- endTime - startTime
            expTermMinusOne <- exp(h * tau) - 1
            2 * expTermMinusOne / (hTimesTwo + kappaPlusH * expTermMinusOne)
        }
    }
)

setGeneric(
    name = ".SetRho",
    def  = function(model) {
        standardGeneric(".SetRho")
    }
)

setMethod(
    f          = ".SetRho",
    signature  = signature(model = "Cir"),
    definition = function(model) {
        sigma     <- model@sigma
        sigmaSqr  <- sigma * sigma
        h         <- model@h
        hTimesTwo <- 2 * h
        function(startTime, endTime) hTimesTwo / (sigmaSqr* (exp(h * (endTime - startTime)) - 1))
    }
)


