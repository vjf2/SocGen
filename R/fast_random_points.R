#' Sample relocations from a utilization distribution
#'
#' This function samples relocations given a set of coordinates, number of individuals, and probability of each individual being relocated at each coordinate
#' @param probweights a probability vector from which to select individuals, length must be greater than or equal to nd
#' @param nd number of individuals to sample
#' @param dc a matrix of coordinates
#' @param dgdf a dataframe of probabilities per individual in each cell
#' @param gridrad integer, half of the (square) gridsize
#' @keywords sample relocations
#' @export
#' @examples
#'



fast_random_points <- function(probweights, nd, dc, dgdf, gridrad) {

  daily_dolphins <- sample(names(probweights), size=nd, replace=FALSE, prob=probweights)
  df <- dgdf[,which(colnames(dgdf) %in% daily_dolphins), drop=FALSE]
  df[is.na(df)] <- 0
  cells <- apply(df, 2, function(x) sample(seq_len(nrow(df)), 1, prob=x))
  xy <- dc[cells,,drop=FALSE]
  xy <- xy + c(runif(nd, -gridrad, gridrad), runif(nd, -gridrad, gridrad))
  res <- data.frame(x=xy[,1], y=xy[,2], id=names(cells))
  return(res)

}
