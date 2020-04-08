setClass(
    Class          = "IQuoteTermStructure",
    representation = representation(
        calendar = "character",
        period   = "character",
        rate     = "numeric",
        tenor    = "character",
        type     = "character",
        "VIRTUAL"
    )
)

setGeneric(
    name = ".BootstrappingIterator",
    def  = function(termStructure, supportTermStructure, target) {
        standardGeneric(".BootstrappingIterator")    
    } 
)

setGeneric(
    name = "Date<-",
    def  = function(object, value) {
        standardGeneric("Date<-")
    } 
)

setGeneric(
    name = "Date<-",
    def  = function(object, value) {
        standardGeneric("Date<-")
    } 
)

setGeneric(
    name = "QuoteOrder",
    def  = function(termStructure) {
        standardGeneric("QuoteOrder")
    }
)

setMethod(
    f          = "QuoteOrder",
    signature  = signature(termStructure = "IQuoteTermStructure") ,
    definition = function(termStructure) {
        ConvertTenorToDate <- SetTenorConvertor(termStructure@calendar)
        expiryDates        <- ConvertTenorToDate(termStructure@tenor)
        
        if (length(unique(expiryDates)) != length(expiryDates)) {
            stop("each expiry date must be unique")
        }
        
        order(expiryDates)
    }
)

setGeneric(
    name = "SortTermStruucture",
    def  = function(termStructure) {
        standardGeneric("SortTermStruucture")
    } 
)