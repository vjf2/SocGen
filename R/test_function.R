#' Test Function
#'
#' This function prepares students for the exercise "Using genome-wide SNP data to investigate social structure in pilot whales"



test_function<-function(){
  
  #check R version
  rv<-paste0(version$major, substr(version$minor, 1,1))
  if(rv!="35") {
    print(paste0("You are using ", 
                version$version.string,
                ". You may need to update to the latest version available at www.r-project.org."))
    
  }
  
  else if(!require(SNPRelate)){
  source("https://bioconductor.org/biocLite.R")
  tryCatch(biocLite("SNPRelate"), error=function(e) message("Please install the SNPRelate package"))
    
  }
  
  else if(!require(igraph)){
  
    tryCatch(install.packages("igraph", dependencies = TRUE), error=function(e) message("Please install the igraph package"))
      
  }
    
  #check working directory
  
  else if(!"dna_ascii.txt" %in% list.files()){
    message(paste("You do not have the file \"dna_ascii.txt\" in your working directory. Your current working directory is", getwd())) 
  }
    
  else{
    art<-readLines("dna_ascii.txt")
    cat(art, sep = "\n")
  }
  
}