#' Schedulize
#'
#' This function converts a dataframe of entry and depart dates to an availability matrix for simulations or calculating association indices
#' @param data a dataframe
#' @param id individual IDs
#' @param start first date of availability 
#' @param end last date of availability
#' @param dates dates in standard character or Date format
#' @param format one of "sim" for SENM or "mask" for calculating association indices
#' @keywords schedule availabilty matrix
#' @export
#' @examples
#' fast_random_points()
#' simple_ratio

schedulize <- function(data,
                       id = "dolphin_id",
                       start = "entry",
                       end = "depart",
                       dates = dates,
                       format = c("sim", "mask")) {
  dolphins <- unique(data[, id])
  
  numdates <- as.Date(dates, origin = "1970-01-01")
  
  data[, start] <- as.numeric(data[, start])
  data[, end] <- as.numeric(data[, end])
  
  alive <- Vectorize(
    FUN = function(r, c)
      isTRUE(r >= data[, start][which(data[, id] == c)]
             & r <= data[, end][which(data[, id] == c)])
  )
  
  schedule <-
    outer(as.numeric(numdates), dolphins, FUN = alive) #takes 2 min to run
  
  matnames <- list(dates, dolphins)
  
  dimnames(schedule) <- matnames
  
  if (format == "mask") {
    tsch <- t(schedule)
    tsch[isTRUE(tsch)] <- 1
    tsch[tsch == 0] <- NA
    return(tsch)
  }
  
  else{
    return(schedule)
  }
}