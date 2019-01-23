#' Simple Ratio Index
#'
#' This function calculates the simple ratio index for data in linear format
#' @param sightings a dataframe in linear format, i.e., one row per individual per observation
#' @param group_variable a unique group identifier
#' @param dates dates
#' @param IDs individual IDs
#' @param diag logical, should the diagonal be included (default FALSE)
#' @param symmetric logical, should a symmetric matrix be returned (default TRUE)
#' @param mask an availability matrix where rows are individuals and columns are sighting dates, cells should be 1 for available or NA if not 
#' @param assocInd whether the association index "SRI", the number of sightings together "X", or total "XY" should be returned
#' @keywords simple ratio association index
#' @export
#' @examples
#' dat2mat()


simple_ratio<-function(sightings=sightings, group_variable=group_variable, dates=dates, IDs=IDs, diag=FALSE, symmetric=TRUE, mask=NULL, assocInd="SRI"){
  
  #calcualate days where both individuals were seen
  
  jd<-data.frame(sightings[,c(IDs, dates)], 1)
  days_seen<-dat2mat(jd)
  mode(days_seen)<-"numeric"
  days_seen[is.na(days_seen)]<-0
  
  #calculate number of groups each pair was seen in together
  
  jg<-data.frame(sightings[,c(IDs, dates, group_variable)], 1)
  jg[,group_variable]<-paste0("g", jg[,group_variable])
  
  if(is.matrix(mask)){
    jg$filter<-apply(jg, 1, function(w) {mask[as.character(w[1]),as.character(w[2])]})
    jg$X1<-ifelse(is.na(jg$filter),0,1) }
  
  groups_seen<-dat2mat(jg[,c(IDs, group_variable, "X1")])
  mode(groups_seen)<-"numeric"
  groups_seen[is.na(groups_seen)]<-0
  
  gs<-tcrossprod(groups_seen) #seen in same group
  
  if(assocInd=="X") {retVal <- gs
  } else{
  
  ds<-tcrossprod(days_seen) #seen on same day
  
  nmatpre<-as.numeric(table(sightings[,IDs])) #number of times each individual sighted
  
  nmat<-outer(nmatpre, nmatpre, FUN="+")
  
  if(assocInd=="SRI" & is.null(mask)){
    retVal<-gs/(nmat - gs - (ds - gs))
    }else if(assocInd=="XY" & is.null(mask)) {retVal <- (nmat - gs - (ds - gs))
    }else if(is.matrix(mask)){
      mask<-mask[rownames(days_seen), colnames(days_seen)]
      obsXavail<-mask * days_seen
      obsXavail[is.na(obsXavail)] <- -1
      inds<-rownames(obsXavail)
      cmb<-t(combn(rownames(obsXavail), 2))
      cmb<-rbind(cmb, cbind(inds, inds))
      input<-t(obsXavail)
      
      res1<-apply(cmb, 1, function(x) {
        xy<-input[,x[1]] * input[,x[2]]
        z<-sum(xy==-1)  
        return(z)
        }) 
      
    rmmat<-data.frame(cmb, res1)
    rmmat<-dat2mat(rmmat, forceSymmetric = TRUE)
    if(assocInd=="SRI" & is.matrix(mask)){retVal<-gs/((nmat-rmmat) - gs - (ds - gs))
    }else if(assocInd=="XY" & is.matrix(mask)){retVal<-((nmat-rmmat) - gs - (ds - gs))}
  }
  
} #end everything after just X
  

  if(!diag) {diag(retVal)<-NA}
  if(!symmetric) {retVal[lower.tri(retVal)]<-NA}
  
  return(retVal)
  
}
