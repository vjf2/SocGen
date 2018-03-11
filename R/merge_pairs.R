#' A Function to Merge Pairs
#'
#' This function allows you to merge data based pairs of IDs that can be in either order
#' @param x a matrix or dataframe
#' @param y a matrix or dataframe
#' @param xID1,xID2,yID1,yID2 specifications of the columns used for merging
#' @param all.x,all.y logical, see merge function for details
#' @keywords merge pairs
#' @export
#' @examples
#' mhead()



#function to merge data based pairs of IDs that can be in either order
#created jan 10 2018
#vivienne foroughirad

merge_pairs<-function(x, y, xID1, xID2, yID1=xID1, yID2=xID2, all.x=TRUE, all.y=FALSE){
  ogx<-x

    #first check if there are duplicated pairs in dataset
  check_row<-nrow(x)
  reduced<-reduce_pairs(x=x, ID1=xID1, ID2=xID2)
  check_reduced<-nrow(reduced)

  if(check_reduced<check_row)
  {warning("Left data contains duplicate pairs. Pairs treated symmetrically.")
    x<-reduced}

  #make lookup table from x
  x$start_pair<-paste0(x[,xID1], x[,xID2])
  x$reverse_pair<-paste0(x[,xID2], x[,xID1])
  x$index<-1:length(x$start_pair)

  p1<-data.frame(pair=x$start_pair, index=1:length(x$start_pair))
  p2<-data.frame(pair=x$reverse_pair, index=1:length(x$reverse_pair))

  plookup<-rbind(p1,p2)

  y$pair<-paste0(y[,yID1], y[,yID2])

  y$index<-plookup$index[match(y$pair, plookup$pair)]

  ogx$pair<-paste0(ogx[,xID1], ogx[,xID2])

  ogx$index<-plookup$index[match(ogx$pair, plookup$pair)]

  z<-merge(ogx,y,by="index", all.x=all.x, all.y=all.y)

  funccol<-c("start_pair", "reverse_pair", "index", "pair", "pair.x", "pair.y",yID1, yID2)

  z<-z[,setdiff(names(z), funccol)]

  return(z)
}
