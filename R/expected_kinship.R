#' Expected Kinship
#'
#' This function calculates the expected kinship value based on maternal, paternal, and combined pedigrees
#' @param id character string specifying column of individuals ids
#' @param mother_id character string specifying column of mother ids
#' @param father_id character string specifying column of father ids
#' @param sex character string specifying column of individuals sexes
#' @param data dataframe
#' @keywords kinship
#' @export
#' @examples
#' mat2dat()

expected_kinship<-function(id="id", mother_id="mother_id", father_id="father_id", sex="sex", data){

    if (!requireNamespace("kinship2", quietly = TRUE)) {
      stop("Package \"kinship2\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
  
  n=length(data[,id])
  
  biparental<-data.frame(id=data[,id], findex=integer(n), mindex=integer(n), sex=data[,sex])
  biparental$findex<-match(data[,mother_id][match(biparental$id, data[,id])], biparental$id)
  biparental$mindex<-match(data[,father_id][match(biparental$id, data[,id])], biparental$id)
  biparental[is.na(biparental)]<-0
  biparental$sex[which(biparental$sex=="")]<-"UNKNOWN"
  biparental$sex<-as.factor(tolower(biparental$sex))
  
  maternal<-data.frame(id=biparental$id, findex=biparental$findex, mindex=0, sex=biparental$sex)
  paternal<-data.frame(id=biparental$id, findex=0, mindex=biparental$mindex, sex=biparental$sex)
  
  dx3<-lapply(list(maternal, paternal, biparental), function(x){
    ped<-as.list(x)
    class(ped)<-"pedigree"
    kinmat<-kinship2::kinship(ped)
    diag(kinmat)<-NA
    kinmat[lower.tri(kinmat)]<-NA
    dx<-mat2dat(kinmat)
    dx[,3]<-dx[,3]*2
    return(dx)
  })

final<-data.frame(dx3[[1]], 
                  paternal=dx3[[2]][[3]], 
                  biparental=dx3[[3]][[3]])

names(final)[3]<-"maternal"
rownames(final)<-seq_len(nrow(final))

return(final)
}








