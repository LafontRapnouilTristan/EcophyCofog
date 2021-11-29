#' merge_minipam
#'
#' @param input_path character, the path to your minipam csv output to be merged
#'
#' @return a csv, row binding all the files contained it your input path
#' @export
merge_minipam <- function(input_path){
  mini_list <- list.files(paste0(input_path),pattern = "csv")
  big_data <- NULL
  for( i in 1:length(mini_list)){
    a <- read.csv2(paste0(input_path,mini_list[i]), header = T,skip=1)
    big_data <- rbind(big_data,a)
  }
  write.csv(big_data,file=paste0(input_path,"minipam_tot.csv"), row.names = F)
}
