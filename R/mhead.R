#' A Matrix Preview Function
#'
#' This function allows you to view a preview of wide matrices and dataframes.
#' @param x a matrix or dataframe
#' @param n number of row or columns to display (default 10)
#' @keywords matrix head
#' @export
#' @examples
#' mhead()


mhead<-function(x, n=10){
  nr<-nrow(x)
  nc<-ncol(x)
  ifelse(nr<=n, nr<-nr, nr<-n)
  ifelse(nc<=n, nc<-nc, nc<-n)
  return(x[1:nr, 1:nc])


}
