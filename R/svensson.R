setClass(
    Class          = "Svensson",
    contains       = "NelsonSiegel",
    representation = representation(
        beta3   = "numeric",
        lambda2 = "numeric"
    ),
    prototype      = prototype(
        beta3   = NaN,
        lambda2 = NaN
    )
)

InitializeSvensson <- function() new(Class = "Svensson")

setMethod(
    f          = "NParameter",
    signature  = signature(model = "Svensson"),
    definition = function(model) 6L
)

setMethod(
    f          = "Parameter",
    signature  = signature(model = "Svensson"),
    definition = function(model) {
        nsPar      <- callNextMethod(model)
        nsParName  <- names(nsPar)
        par        <- c(nsPar, model@beta3, model@lambda2)
        names(par) <- names(nsPar, "beta3", "lambda2")
        par
    }
)

setMethod(
    f          = "Parameter<-",
    signature  = signature(model = "Svensson",
                           value = "numeric"),
    definition = function(model, value) {
        model       <- callNextMethod(model, value)
        model@beta3 <- value[5]
        model@lambda2  <- value[6]
        model
    }
)


setMethod(
    f          = "ParameterLowerBound",
    signature  = signature(model = "Svensson"),
    definition = function(model) c(callNextMethod(model), -0.3, 0)
)

setMethod(
    f          = "ParameterUpperBound",
    signature  = signature(model = "Svensson"),
    definition = function(model) c(callNextMethod(model), 0.3, 30)
)


setMethod(
    f          = "SetInstForwardRate",
    signature  = signature(zeroRateTermStructure = "Svensson"),
    definition = function(zeroRateTermStructure) {
        NelsonSiegelInstForwardRate <- callNextMethod(zeroRateTermStructure)
        beta3                       <- zeroRateTermStructure@beta3
        lambda2                     <- zeroRateTermStructure@lambda2

        function(tau) {
            tauDivLambda <- tau / lambda2
            NelsonSiegelInstForwardRate(tau) + tauDivLambda * exp(-tauDivLambda) * beta3
        }
    }
)

setMethod(
    f          = "SetZeroRate",
    signature  = signature(zeroRateTermStructure = "Svensson"),
    definition = function(zeroRateTermStructure) {
        NelsonSiegelZeroRate <- callNextMethod(zeroRateTermStructure)
        beta3                <- zeroRateTermStructure@beta3
        lambda2              <- zeroRateTermStructure@lambda2
        function(tau) {
            tauDivLambda <- tau / lambda2
            expTerm      <- exp(-tauDivLambda)
            ifelse(test = tau > 0,
                   yes  = NelsonSiegelZeroRate(tau) + beta3 *((1 - expTerm) / tauDivLambda - expTerm),
                   no   = 0)
        }
    }
)
