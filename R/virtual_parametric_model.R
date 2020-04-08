setClass(
    Class = "IParametricModel"
)

setMethod(
    f          = "Calibrate",
    signature  = signature(marketData = "ANY",
                           model      = "IParametricModel"),
    definition = function(marketData, model) {
        env               <- environment()
        lowerBound        <- ParameterLowerBound(model)
        upperBound        <- ParameterUpperBound(model)
        Constraints       <- ParameterConstraints(model)
        ObjectiveFunction <- SetObjectiveFunction(marketData, model)
        if (!is.null(Constraints)) {
            NewConstraints        <- function(par) -Constraints(par)
            OptimizationAlgorithm <- function(initialValue) {
                nloptr::cobyla(x0      = initialValue,
                               fn      = ObjectiveFunction,
                               lower   = lowerBound,
                               upper   = upperBound,
                               hin     = NewConstraints,
                               control = nl.opts(list(maxeval = length(lowerBound) * 20000)))
            }
        } else {
            NewConstraints        <- NULL
            OptimizationAlgorithm <- function(initialValue) {
                nloptr::lbfgs(x0      = initialValue,
                              fn      = ObjectiveFunction,
                              lower   = lowerBound,
                              upper   = upperBound,
                              control = nl.opts(list(maxeval = length(lowerBound) * 20000)))
            }
        }

        calibrationResult <- switch(OptimizationStrategy(),
                                    {
                                        preCalibrationResult <- DEoptimR::JDEoptim(fn     = ObjectiveFunction,
                                                                                   lower  = lowerBound,
                                                                                   upper  = upperBound,
                                                                                   constr = Constraints,
                                                                                   tol    = sqrt(.Machine$double.eps))

                                        print(preCalibrationResult)
                                        calibrationResult <- OptimizationAlgorithm(preCalibrationResult[["par"]])
                                    },
                                    {
                                        cluster <- parallel::makeCluster(4)
                                        parallel::clusterExport(cluster, "lowerBound", env)
                                        parallel::clusterExport(cluster, "upperBound", env)
                                        parallel::clusterExport(cluster, "NewConstraints", environment())
                                        parallel::clusterExport(cluster, "ObjectiveFunction", env)
                                        parallel::clusterExport(cluster, "OptimizationAlgorithm", env)
                                        parallel::clusterEvalQ(cluster, require("nloptr"))
                                        parallel::clusterEvalQ(cluster, require("rootSolve"))

                                        results <- parallel::parLapply(cl  = cluster,
                                                                       X   = replicate(n        = NParameter(model) * 5,
                                                                                       expr     = runif(NParameter(model),lowerBound, upperBound),
                                                                                       simplify = FALSE),
                                                                       fun = OptimizationAlgorithm)

                                        parallel::stopCluster(cluster)
                                        print(results)
                                        calibrationResult <- Reduce(f    = function(result, elem) {
                                                                        betterValueResult <- `if`(elem[["value"]] < result[["value"]],
                                                                                                  elem,
                                                                                                  result)

                                                                        if (result[["convergence"]] < 0) {
                                                                            betterValueResult
                                                                        } else {
                                                                            if (elem[["convergence"]] > 0)
                                                                                betterValueResult
                                                                            else
                                                                                result
                                                                        }
                                                                    },
                                                                    x    = results[-1],
                                                                    init = results[[1]])
                                    })

        print(calibrationResult)
        Parameter(model) <- calibrationResult[["par"]]

        if (FittingChart()) {
            .FittingChart(marketData, model)
        }

        model
    }
)

setGeneric(
    name = "NParameter",
    def  = function(model) {
        standardGeneric("NParameter")
    }
)

setGeneric(
    name = "Parameter",
    def  = function(model) {
        standardGeneric("Parameter")
    }
)

setGeneric(
    name = "Parameter<-",
    def  = function(model, value) {
        standardGeneric("Parameter<-")
    }
)

setGeneric(
    name = "ParameterConstraints",
    def  = function(model) {
        standardGeneric("ParameterConstraints")
    }
)

setMethod(
    f          = "ParameterConstraints",
    signature  = signature(model = "IParametricModel"),
    definition = function(model) NULL
)

setGeneric(
    name = "ParameterLowerBound",
    def  = function(model) {
        standardGeneric("ParameterLowerBound")
    }
)

setGeneric(
    name = "ParameterUpperBound",
    def  = function(model) {
        standardGeneric("ParameterUpperBound")
    }
)

setGeneric(
    name = "SetObjectiveFunction",
    def  = function(marketData, model) {
        standardGeneric("SetObjectiveFunction")
    }
)

