setClass(
    Class          = "IAffineModel",
    contains       = "IParametricModel",
    representation = representation(
        initialValue = "numeric",
        "VIRTUAL"
    )
)

setGeneric(
    name = "InitialValue",
    def  = function(model) {
        standardGeneric("InitialValue")
    }
)

setMethod(
    f          = "InitialValue",
    signature  = signature(model = "IAffineModel"),
    definition = function(model) model@initialValue
)

setGeneric(
    name = "InitialValue<-",
    def  = function(model, value) {
        standardGeneric("InitialValue<-")
    }
)

setMethod(
    f          = "InitialValue<-",
    signature  = signature(model = "IAffineModel",
                           value = "numeric"),
    definition = function(model, value) {
        model@initialValue <- value
        model
    }
)

.CreateAffineModelZcbClosedForm <- function(SetA, SetB) {
    function(model) {
        A            <- SetA(model)
        B            <- SetB(model)
        initialValue <- InitialValue(model)

        function(endTime,
                 startTime = 0,
                 r0        = initialValue) {

            A(startTime, endTime) * exp(-B(startTime, endTime) * r0)
        }
    }
}

setGeneric(
    name = "SetZcbClosedForm",
    def  = function(model) {
        standardGeneric("SetZcbClosedForm")
    }
)

setMethod(
    f          = "SetZcbClosedForm",
    signature  = signature(model = "IAffineModel"),
    definition = .CreateAffineModelZcbClosedForm(.SetA, .SetB)
)

setGeneric(
    name = ".SetA",
    def  = function(model) {
        standardGeneric(".SetA")
    }
)

setGeneric(
    name = ".SetB",
    def  = function(model) {
        standardGeneric(".SetB")
    }
)

setGeneric(
    name = "SetSwaptionClosedForm",
    def  = function(model) {
        standardGeneric("SetSwaptionClosedForm")
    }
)

setGeneric(
    name = "SetZcbCallClosedForm",
    def  = function(model) {
        standardGeneric("SetZcbCallClosedForm")
    }
)

setGeneric(
    name = "SetZcbPutClosedForm",
    def  = function(model) {
        standardGeneric("SetZcbPutClosedForm")
    }
)



