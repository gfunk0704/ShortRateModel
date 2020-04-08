SetTenorConvertor <- local(
    expr = {
        baseList        <- list(days, weeks, months, years)
        names(baseList) <- c("D", "W", "M", "Y")
        SetBusinessDay  <- function(IsHoliday, Operator) {
            oneDay <- days(1)

            function(date) {
                while (IsHoliday(date))
                    date <- Operator(date, oneDay)

                date
            }
        }

        function(calendar) {
            IsHoliday    <- function(date) isHoliday(calendar, date)
            IsEndOfMonth <- function(date) isEndOfMonth(calendar, date)

            function(tenors,
                     startDate = ValueDate()) {
                nChar  <- nchar(tenors)
                num    <- as.integer(substring(tenors, 1, nChar - 1))
                base   <- substring(tenors, nChar, nChar)
                dayNum <- mapply(FUN  = function(num, base) {
                                     AddBase    <- baseList[[base]]
                                     targetDate <- startDate %m+% AddBase(num)

                                     if (IsHoliday(targetDate)) {
                                         if (base %in% c("M", "Y")) {
                                             if (IsEndOfMonth(targetDate)) {
                                                 SetBusinessDay(IsHoliday, `%m-%`)(targetDate)
                                             } else {
                                                 templateDate <- SetBusinessDay(IsHoliday, `%m+%`)(targetDate)

                                                 if (month(templateDate) != month(targetDate)) {
                                                     SetBusinessDay(IsHoliday, `%m-%`)(targetDate)
                                                 } else {
                                                     templateDate
                                                 }
                                             }
                                         } else {
                                             SetBusinessDay(IsHoliday, `%m+%`)(targetDate)
                                         }
                                     } else {
                                         targetDate
                                     }
                                 },
                                 num  = num,
                                 base = base)

                as.Date(dayNum,
                        origin = "1970-01-01")
            }
        }
    }
)
