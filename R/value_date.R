SetValueDate <- function(date) {
    assign(x     = ".valueDate",
           value = ymd(date),
           envir = .GlobalEnv)
}

ValueDate <- function() {
    get(x     = ".valueDate",
        envir = .GlobalEnv)
}