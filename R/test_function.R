#' Test Function
#'
#' This function prepares students for the exercise "Using genome-wide SNP data to investigate social structure in pilot whales"
#' @export


test_function<-function(){
  
  #check R version
  rv<-paste0(version$major, substr(version$minor, 1,1))
  if(rv!="35") {
    print(paste0("You are using ", 
                version$version.string,
                ". You should update to the latest version (3.5.1) available at www.r-project.org."))
    
  }
  
  else if(!suppressWarnings(require(SNPRelate, quietly=TRUE))){
  source("https://bioconductor.org/biocLite.R")
  biocLite(c("gdsfmt", "SNPRelate"),
                   suppressUpdates=TRUE, suppressAutoUpdate=TRUE, ask=FALSE, 
                   quiet=TRUE)
    
  }
  
  if(!suppressWarnings(require(igraph, quietly = TRUE, warn.conflicts=FALSE))){
  
    install.packages("igraph", quiet=TRUE, verbose=FALSE)
      
  }
    
  #check working directory
  
  if(!"test_file.txt" %in% list.files()){
    message(paste("You do not have the file \"test_file.txt\" in your working directory. Your current working directory is", getwd())) 
  }
    
  else{
    art<-readLines("test_file.txt")
    cat(art, sep = "\n")
  }
  
}
