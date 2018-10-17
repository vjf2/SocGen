#' Create a bounded random walk
#'
#' This function creates a random walk bounded by a maximum distance from the starting position.
#' @param n.times integer, the number of steps to take (default 100)
#' @param maxDist numeric, the maximum distance from starting posistion (default NA)
#' @param start x and y starting position (default (0,0))
#' @keywords random walk, bounded
#' @export
#' @examples
#' bounded_walk()


bounded_walk <- function(n.times=100,
                 maxDist=10,
                 start=c(0,0)) 
                  {
  x <- start[1]
  y <- start[2]
  xl <- list()
  yl <- list()
  
  for (i in 1:n.times) {
    repeat {
      ## pick step sizes
      xi <- rnorm(1, 0, abs(rnorm(1, 0.2, 0.5)))
      yi <- rnorm(1, 0, abs(rnorm(1, 0.2, 0.5)))
      ## new candidate locations
      newx <- x+xi
      newy <- y+yi
      ## IF new locations are within bounds, then
      ##    break out of the repeat{} loop (otherwise
      ##    try again)
      if(is.na(maxDist)) break
      if(sqrt((start[1]-newx)^2+(start[2]-newy)^2)<maxDist) break
    }
    x <- xl[[i]] <- newx
    y <- yl[[i]] <- newy
  }
  mvmt<-cbind(unlist(xl), unlist(yl))
  return(mvmt)
  }
