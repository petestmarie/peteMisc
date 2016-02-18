# http://stackoverflow.com/users/2055486/jim
# http://stackoverflow.com/questions/3611314/calculating-ages-in-r
#' \code{age_years} Age in years from birthdata and second date.
#' @param birthdate Birthdate.
#' @param to Date to (procedure date).
#' @return Age in whole years.
#' @export
#' @examples
#' age_years(birthdate = "1960-12-20", to = "2016-02-18")
age_years = function(birthdate, to) {
  from_lt = as.POSIXlt(birthdate)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

