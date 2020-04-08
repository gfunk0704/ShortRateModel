CalibrationSetting <- function(optimizationStrategy = 1L,
                               fittingChart         = TRUE) {
    
    assign(x     = ".optimizationStrategy",
           value = optimizationStrategy,
           envir = .GlobalEnv)
    
    assign(x     = ".fittingChart",
           value = fittingChart,
           envir = .GlobalEnv)
}

OptimizationStrategy <- function() {
    get(x     = ".optimizationStrategy",
        envir = .GlobalEnv)
}

FittingChart <- function() {
    get(x     = ".fittingChart",
        envir = .GlobalEnv)
}