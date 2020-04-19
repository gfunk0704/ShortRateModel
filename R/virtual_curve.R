setClass(
    Class = "ICurve"
)

setGeneric(
    name = "SetCurve",
    def  = function(curve) {
        standardGeneric("SetCurve")
    }
)

setGeneric(
    name = "SetIntegration",
    def  = function(curve) {
        standardGeneric("SetIntegration")
    }
)
