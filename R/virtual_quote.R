setClass(
    Class          = "IQuote",
    representation = representation(
        period = "character",
        rate   = "numeric",
        tenor  = "character",
        type   = "character",
        "VIRTUAL"
    )
)

.AdjustmentScalar <- function(unit) {
    switch(tolower(unit),
           "bps"        = 0.0001,
           "percentage" = 0.01,
           "value"      = 1,
           stop("unknown unit found"))
}