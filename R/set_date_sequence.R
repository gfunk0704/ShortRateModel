SetDateSequence <- function(from, ConvertTenorToDate) {
    function(to, by) {
        num                <- addNum <- as.integer(substring(by, 1, nchar(by) - 1))
        countBase          <- substring(by, nchar(by), nchar(by))
        dates              <- from
        endDate            <- ConvertTenorToDate(to)

        repeat({
            if (tail(dates, 1) >= endDate)
                break

            newDate <- ConvertTenorToDate(paste0(num, countBase), from)

            if (newDate > endDate)
                break

            dates <- c(dates, newDate)
            num   <- num + addNum
        })

        dates
    }
}
