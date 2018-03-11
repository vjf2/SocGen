#' A Function to Reduce Redundant Dyads
#'
#' This function allows you to merge data based pairs of IDs that can be in either order
#' @param x a matrix or dataframe
#' @param ID1,ID2 specifications of the columns containing the dyad identifiers
#' @keywords reduce pairs
#' @export
#' @examples
#' mhead()
#' merge_pairs()

reduce_pairs<-function(x, ID1, ID2){
  x$original_order<-seq(1, nrow(x),1)
  y<-as.data.frame(t(apply(x[,c(ID1, ID2)], 1, sort)))
  y$original_order<-seq(1, nrow(y),1)
  sy<-y[!duplicated(y[,1:2]),c(1:2)]
  sy$index<-seq(1,nrow(sy),1)
  fy<-merge(y,sy, by=c(1,2))
  x$index<-fy$index[match(x$original_order, fy$original_order)]
  x<-x[!duplicated(x$index),-c(ncol(x)-1,ncol(x))]
  return(x)
}

