#' Convert a dataframe in a long format to a wide matrix
#'
#' This function converts 3-column dataframe such as produced with mat2dat back into a matrix with the IDs in the first two columns used as dimnames
#' @param data a matrix
#' @param forceSymmetric boolean, if matrix has a empty triangle, fill over the diagonal
#' @param diag boolean, should a diagonal be added if absent?
#' @details If value column is type numeric or integer the function will return a numeric matrix, otherwise a character matrix will be returned. 
#' @keywords rematrix
#' @export
#' @examples
#' mat2dat()


dat2mat<-function(data = data, forceSymmetric = FALSE, diag = FALSE){
  
  typ<-class(data[,3])
  
  data<-as.matrix(data)
  data<-apply(data, 2, trimws)
  
  ro<-sort(unique(data[,1]))
  nro<-length(ro)
  co<-sort(unique(data[,2]))
  nco<-length(co)
  mat<-matrix(nrow=nro, ncol=nco)
  dimnames(mat)<-list(as.character(ro), as.character(co))
  
  if(diag)
    {
    rco<-as.character(unique(c(ro, co)))
    nrco<-length(rco)
    mat<-matrix(nrow=nrco, ncol=nrco)
    dimnames(mat)<-list(rco, rco)
    }
  
  apply(data, 1, function(w) {mat[as.character(w[1]),
                               as.character(w[2])]<<-w[[3]]})
  if(forceSymmetric)
  {
    #check that one triangle is empty
    if(all(is.na(mat[upper.tri(mat)]))){
      mat[upper.tri(mat)] = t(mat)[upper.tri(mat)]
    }
    else if(all(is.na(mat[lower.tri(mat)]))){
      mat[lower.tri(mat)] = t(mat)[lower.tri(mat)]
    }
    else {warning("Matrix does not have an empty triangle") }   
      
  }
  
  if(typ %in% c("integer", "numeric")) {
    mode(mat)<-"numeric"
  }
  
  #fix logicals
  if(typ=="logical") 
  {
    mat<-trimws(mat)
    mode(mat)<-"logical"
  }
  
  return(mat)
}