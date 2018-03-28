#' Convert a dataframe in a long format to a wide matrix
#'
#' This function converts 3-column dataframe such as produced with mat2dat back into a matrix with the IDs in the first two columns used as dimnames
#' @param x a matrix
#' @keywords rematrix
#' @export
#' @examples
#' mat2dat()


dat2mat<-function(x){
  ro<-sort(unique(x[,1]))
  nro<-length(ro)
  co<-sort(unique(x[,2]))
  nco<-length(co)
  mat<-matrix(nrow=nro, ncol=nco)
  dimnames(mat)<-list(ro, co)
  apply(x, 1, function(w) {mat[as.character(w[1]),
                               as.character(w[2])]<<-w[[3]]})
  return(mat)
}
