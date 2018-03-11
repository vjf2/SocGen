#' A Matrix Preview Function
#'
#' This function allows you to view a preview of wide matrices and dataframes.
#' @param x a matrix or dataframe
#' @keywords matrix head
#' @export
#' @examples
#' mhead()


mhead<-function(x){
  nr<-nrow(x)
  nc<-ncol(x)
  ifelse(nr<=10, nr<-nr, nr<-10)
  ifelse(nc<=10, nc<-nc, nc<-10)
  return(x[1:nr, 1:nc])


}
