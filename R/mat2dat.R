#' Convert a matrix to a dataframe with ID names
#'
#' This function converts a matrix to a 3-column dataframe with dimnames and values
#' @param x a matrix
#' @keywords unmatrix
#' @export
#' @examples
#' mat2dat()


mat2dat<-function (x) {
  rnames <- rownames(x)
  cnames <- colnames(x)
  nmat<-expand.grid(rnames, cnames, stringsAsFactors = FALSE)
  nmat$values<-c(x)
  nmat<-nmat[complete.cases(nmat),]
  names(nmat)<-c("ID1", "ID2", "values")
  return(nmat)
}
