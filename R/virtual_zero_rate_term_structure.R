setClass(
    Class          = "IZeroRateTermStructure",
    representation = representation(
        maturity = "numeric",
        "VIRTUAL"
    )
)

setMethod(
    f          = "Chart",
    signature  = signature(object    = "IZeroRateTermStructure",
                           chartType = "character"),
    definition = function(object, chartType) {


        type <- gsub(pattern     = " ",
                     replacement =  "",
                     x           = chartType)

        RateFunction <- switch(tolower(type),
                               "instantaneousforwardrate" = SetInstForwardRate(object),
                               "zerorate"                 = SetZeroRate(object),
                               "zerocouponbondprice"      = SetZeroCouponBondPrice(object))


        maturity      <- object@maturity
        timeSequence  <- seq.int(from = min(maturity),
                                 to   = max(maturity),
                                 by   = DeltaT())

        chartData <- data.frame(rate     = RateFunction(timeSequence),
                                maturity = timeSequence,
                                curve    = rep.int(chartType, length(timeSequence)))

        chart <- ggplot(data    = chartData,
                        mapping = aes(x = maturity, y = rate, group = curve, colour = curve)) +
            geom_line()

        ggplotly(chart)
    }
)

setGeneric(
  name = "SetForwardRate",
  def  = function(zeroRateTermStructure, compoundingFrequency) {
    standardGeneric("SetForwardRate")
  }
)

setMethod(
  f          = "SetForwardRate",
  signature  = signature(zeroRateTermStructure = "IZeroRateTermStructure",
                         compoundingFrequency  = "character"),
  definition = function(zeroRateTermStructure, compoundingFrequency) {
    ZeroCouponBondPrice <- SetZeroCouponBondPrice(zeroRateTermStructure)
    switch(tolower(compoundingFrequency),
           "continuous" = function(startTime, endTime) {
             log(ZeroCouponBondPrice(startTime) / ZeroCouponBondPrice(endTime)) / (endTime - startTime)
           },
           "simple"     = function(startTime, endTime) {
             endTimeZcb <- ZeroCouponBondPrice(endTime)
             (ZeroCouponBondPrice(startTime) - endTimeZcb) / endTimeZcb / (endTime - startTime)
           },
           stop("compoundingFrequency must be 'simple' or 'continuous'"))

  }
)

setMethod(
  f          = "SetForwardRate",
  signature  = signature(zeroRateTermStructure = "IZeroRateTermStructure",
                         compoundingFrequency  = "missing"),
  definition = function(zeroRateTermStructure, compoundingFrequency) SetForwardRate(zeroRateTermStructure, "simple")
)


setGeneric(
    name = "SetInstForwardRate",
    def  = function(zeroRateTermStructure) {
        standardGeneric("SetInstForwardRate")
    }
)

setGeneric(
  name = "SetZeroCouponBondPrice",
  def  = function(zeroRateTermStructure) {
    standardGeneric("SetZeroCouponBondPrice")
  }
)

setGeneric(
  name = "SetZeroRate",
  def  = function(zeroRateTermStructure) {
    standardGeneric("SetZeroRate")
  }
)

