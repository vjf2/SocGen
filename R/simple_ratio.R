#' Simple Ratio Index
#'
#' This function calculates the simple ratio index for data in linear format
#' @param sightings a dataframe in linear format, i.e., one row per individual per observation
#' @param group_variable a unique group identifier
#' @param dates dates
#' @param IDs individual IDs
#' @param diag logical, should the diagonal be included (default FALSE)
#' @param symmetric logical, should a symmetric matrix be returned (default TRUE)
#' @keywords simple ratio association index
#' @export
#' @examples
#' dat2mat()


simple_ratio<-function(sightings=sightings, group_variable=group_variable, dates=dates, IDs=IDs, diag=FALSE, symmetric=TRUE){
  
  #calcualate days where both individuals were seen
  
  jd<-data.frame(sightings[,c(IDs, dates)], 1)
  days_seen<-dat2mat(jd)
  mode(days_seen)<-"numeric"
  days_seen[is.na(days_seen)]<-0
  
  #calculate number of groups each pair was seen in together
  
  jg<-data.frame(sightings[,c(IDs, group_variable)], 1)
  jg[,group_variable]<-paste0("g", jg[,group_variable])
  groups_seen<-dat2mat(jg)
  mode(groups_seen)<-"numeric"
  groups_seen[is.na(groups_seen)]<-0
  
  gs<-tcrossprod(groups_seen) #seen in same group
  
  ds<-tcrossprod(days_seen) #seen on same day
  
  nmatpre<-as.numeric(table(sightings[,IDs])) #number of times each individual sighted
  
  nmat<-outer(nmatpre, nmatpre, FUN="+")
  
  res<-gs/(nmat - gs - (ds - gs))
  
  if(!diag) {diag(res)<-NA}
  if(!symmetric) {res[lower.tri(res)]<-NA}
  
  return(res)
  
}