#' plate_layout
#'
#' @param samples a vector containing all your samples ID, they will fill the plate in the order they are in this vector
#' @param proj name of your porject to name your plates as : "proj-PLx"
#' @param name_file a name to your output file
#' @param save_file_path  path to where you want to save the excel output
#' @param starting_plate_number where from start plate numbering
#'
#' @return an xlsx workbook with you plate plan
#' @export
#' @import openxlsx


plate_layout <- function(samples,
                         proj,
                         name_file,
                         save_file_path,
                         starting_plate_number = 1){


splitted_samples <- split(samples,ceiling(seq_along(samples)/88))
nbplates <- round(length(samples)/88)
plates_names <- c(paste0("PL",starting_plate_number:(starting_plate_number+nbplates-1),"-",proj))


master_plan <- NULL
two_empty_rows <- matrix(nrow=2,ncol=13," ")
for(i in 1:length(plates_names)){
  plate <- matrix(nrow=8,ncol=12,0)

  #fill diags with control
  if(i %% 2 ==0 | length(samples)<88){
    plate[1,1] <- plate[2,2] <- plate[3,3] <- plate[4,4] <- plate[5,5] <- "PCR-tags"
    plate[6,6] <- "PCR-T+"
  }
  else{
    plate[5,8] <- plate[4,9] <- plate[3,10] <- plate[2,11] <- plate[1,12] <- "PCR-tags"
    plate[6,7] <- "PCR-T+"
  }



  #select random placement for CTAB and Xtraction control
  CT_XT_position <- sample(which(plate[,1:12]==0L),2,replace=F)
  plate[CT_XT_position[1]] <- "CTAB"
  plate[CT_XT_position[2]] <- "EXT"

  #fill all 0 with samples ID
  ech_ID <- as.vector(splitted_samples[[i]])
  plate[which(plate[,1:12]==0L)[1:length(ech_ID)]] <- ech_ID

  #names columns
  plate <- rbind(1:12,plate)
  plate <- cbind(c(plates_names[i],LETTERS[1:8]),plate)
  plate[which(plate==0)] <- " "
  master_plan <- rbind(master_plan,two_empty_rows,plate)

}

# xtract cell pos in workbook
your_file <- paste0(save_file_path, '/',name_file,'.xlsx')
openxlsx::write.xlsx(as.data.frame(master_plan),file = your_file,sheetName = "plate_plan",colNames = F,rowNames = F)
wb <- openxlsx::loadWorkbook(file = your_file)

# create formating style according to control type
cstag <- openxlsx::createStyle(bgFill = "#9fc5e8")
cstpos <- openxlsx::createStyle(bgFill = "#e33838")
csctab <- openxlsx::createStyle(bgFill = "#e3e338")
csext <- openxlsx::createStyle(bgFill = "#70dc40")

#format according to cell content
openxlsx::conditionalFormatting(wb,
                                sheet = 1,
                                cols = 1:ncol(master_plan),
                                rows = 1:nrow(master_plan),
                                style = cstag,
                                rule = "tag",
                                type = "contains")
openxlsx::conditionalFormatting(wb,
                                sheet = 1,
                                cols = 1:ncol(master_plan),
                                rows = 1:nrow(master_plan),
                                style = cstpos,
                                rule = "T+",
                                type = "contains")
openxlsx::conditionalFormatting(wb,
                                sheet = 1,
                                cols = 1:ncol(master_plan),
                                rows = 1:nrow(master_plan),
                                style = csctab,
                                rule = "CTAB",
                                type = "contains")
openxlsx::conditionalFormatting(wb,
                                sheet = 1,
                                cols = 1:ncol(master_plan),
                                rows = 1:nrow(master_plan),
                                style = csext,
                                rule = "EXT",
                                type = "contains")
openxlsx::saveWorkbook(wb, your_file, overwrite = T)
}
