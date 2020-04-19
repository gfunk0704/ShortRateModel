setClass(
    Class          = "Point",
    representation = representation(
        time  = "numeric",
        value = "numeric"
    ),
    validity       = function(object) {
        if (length(object@time) != 1L) {
            "the length of time must be 1"
        } else if (length(object@value) != 1) {
            "the length of value must be 1"
        } else {
            TRUE
        }
    }
)

Point <- function(time, value) {
    new(Class = "Point",
        time  = time,
        value = value)
}
