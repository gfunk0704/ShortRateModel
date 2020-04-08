setGeneric(
    name = "Calibrate",
    def  = function(marketData, model) {
        standardGeneric("Calibrate")
    }
)

setGeneric(
    name = "Chart",
    def  = function(object, chartType) {
        standardGeneric("Chart")
    }
)

setGeneric(
    name = "Data<-",
    def  = function(object, value) {
        standardGeneric("Data<-")
    }
)

setGeneric(
    name = ".FittingChart",
    def  = function(marketData, model) {
        standardGeneric(".FittingChart")
    }
)
