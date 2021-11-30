#' merge_minipam
#'
#' @param input_path character, the path to your minipam csv output to be merged
#' @param filename character, the name of the merged output
#' @return a csv, row binding all the files contained it your input path
#' @export
merge_minipam <- function(input_path,filename){
  mini_list <- list.files(paste0(input_path),pattern = "csv")
  big_data <- NULL
  for( i in 1:length(mini_list)){
    a <- read.csv2(paste0(input_path,mini_list[i]), header = T,skip=1)
    big_data <- rbind(big_data,a)
  }
  write.csv2(big_data,file=paste0(input_path,filename,".csv"), row.names = F)
}
