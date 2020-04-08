setClass(
    Class          = "ZeroRatePoint",
    representation = representation(
        maturity = "numeric",
        value    = "numeric"
    )
)

ZeroRatePoint <- function(maturity , value) {
    new(Class    = "ZeroRatePoint",
        maturity = maturity ,
        value    = value)
}
