#' Convert a matrix to a dataframe with ID names
#'
#' This function converts a matrix to a 3-column dataframe with dimnames and values
#' @param x a matrix
#' @param value.name a character string, name of values column
#' @param retainMissing logical, if TRUE retain empty cells as records in dataframe
#' @keywords unmatrix
#' @export
#' @examples
#' mat2dat()


mat2dat<-function (x, value.name="values", retainMissing=FALSE) {
  rnames <- rownames(x)
  cnames <- colnames(x)
  nmat<-expand.grid(rnames, cnames, stringsAsFactors = FALSE)
  nmat$values<-c(x)
  
  if(!retainMissing){
  nmat<-nmat[complete.cases(nmat),]
  }

  names(nmat)<-c("ID1", "ID2", value.name)
  return(nmat)
}
