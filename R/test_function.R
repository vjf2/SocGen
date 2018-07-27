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
                ". You should update to the latest version available at www.r-project.org."))
    
  }
  
  else if(!suppressWarnings(require(SNPRelate, quietly=TRUE))){
  source("https://bioconductor.org/biocLite.R")
  biocLite(c("gdsfmt", "SNPRelate"),
                   suppressUpdates=TRUE, suppressAutoUpdate=TRUE, ask=FALSE, 
                   quiet=TRUE)
    
  }
  
  else if(!suppressWarnings(require(igraph, quietly = TRUE, warn.conflicts=FALSE))){
  
    install.packages("igraph", dependencies = TRUE, quiet=TRUE, verbose=FALSE)
      
  }
    
  #check working directory
  
  else if(!"test_file.txt" %in% list.files()){
    message(paste("You do not have the file \"test_file.txt\" in your working directory. Your current working directory is", getwd())) 
  }
    
  else{
    art<-readLines("test_file.txt")
    cat(art, sep = "\n")
  }
  
}
