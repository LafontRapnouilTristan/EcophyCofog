#' tag_layout
#'
#' @param tag_list a dataframe with 3 column : 'tag_name' (e.g. f1 to fx and r1 to rx), 'tag_sequence' (e.g. ACACACAC) and 'tag_type' (i.e. forward or reverse)
#' @param PCR_plates a matrix object representing your plates map/layout, output of "plate_layout" function of this package.
#' @param output_path path to an output folder that will receive to new files
#' @param file_corresp_tag name of the sample-tagpairs correspondance dataframe
#' @param file_tag_layout name of your xlsx output, having the map of your tagz.
#'
#' @return an xlsx workbook with you tag plate plan and a csv file with your correspondance between tags and samples
#' @export
#' @import openxlsx rlist
tag_layout <- function(tag_list,
                       PCR_plates,
                       output_path,
                       file_corresp_tag,
                       file_tag_layout){

tag_type <- NULL

# RM sample duplicate indexes
for(i in which(grepl("\ ",PCR_plates))){
  PCR_plates[i] <- strsplit(PCR_plates[i],"\ ")[[1]][1]
}

# compute unique tag_pair plates

f_tags <- dplyr::filter(tag_list,tag_type=="forward")
r_tags <- dplyr::filter(tag_list,tag_type=="reverse")

nb_possible_plates <- (nrow(f_tags)*nrow(r_tags))/96

# which one to put in rows or columns

if(nrow(f_tags)%%12==0){
  col_tags <- f_tags
  row_tags <- r_tags
}
 else{
   col_tags <- r_tags
   row_tags <- f_tags
}

# create and store tag pairz
tag_pair_list <- NULL
plates_names <- paste0("PL-",1:nb_possible_plates)
for (i in 1:nb_possible_plates){
  plate <- matrix(nrow = 8,ncol = 12,0) # draw the plate

  j <- rep(1:(nrow(row_tags)/8),nrow(col_tags)/12)
  k <- c(rep(1:(nrow(col_tags)/12),each=(nrow(row_tags)/8)))

  col_pos <- c((12*k[i]-11):(12*k[i])) # tagz for columns of plate i
  row_pos <- c(((8*j[i])-7):(8*j[i])) # tagz for rows of plate i

  col_tags_temp <- col_tags$tag_name[col_pos]
  row_tags_temp <- row_tags$tag_name[row_pos]

  x1 <- rep(row_tags_temp, times=length(col_tags_temp))
  y1 <- rep(col_tags_temp, each=length(row_tags_temp))
  pair_tag_name <- paste0(x1,":",y1)

  col_tags_seq <- col_tags$tag_sequence[col_pos]
  row_tags_seq <- row_tags$tag_sequence[row_pos]
  x1 <- rep(row_tags_seq, times=length(col_tags_seq))
  y1 <- rep(col_tags_seq, each=length(row_tags_seq))
  pair_tag_seq <- paste0(x1,":",y1)



  tag_pair <- cbind(rep(plates_names[i],length(col_tags_seq)),pair_tag_name,pair_tag_seq)
  tag_pair_list <- rbind(tag_pair_list,tag_pair)
}

# Now get sample list

sample_list <- as.data.frame(PCR_plates) #get the matrix to a df
sample_list <-  sample_list [-c((which(is.na(sample_list[,1])==T)),which(grepl("PL",sample_list[,1]))),2:13] #rm non sample cells


# get x first pair of tags, x being our total sample number
tag_pair <- tag_pair_list[1:(nrow(sample_list)*ncol(sample_list)),]
tag_pair <- as.data.frame(tag_pair)

# get current number of plates
nb_plates <- length(as.matrix(sample_list))/96

# rearrange plate for matrix indexation purpose
PCR_df <- sample_list
PCR_df <- split(PCR_df,sort(c(1:nrow(PCR_df))%%nb_plates))
sample_vec <- as.matrix(rlist::list.cbind(PCR_df))

# split sample and tag by plates
splitted_sample<- split(as.vector(sample_vec),sort(c(1:length(as.vector(sample_vec)))%%nb_plates)) #each list element is a plate column afet column
splitted_tag <- split(tag_pair,sort(c(1:nrow(tag_pair))%%nb_plates)) #each list element contains tag info col after col

# combine for each plate the tag and samples
corresp_tag <- NULL
for(i in 1:nb_plates){
  tagz <- splitted_tag[[i]] #tagz are all tags of the plate
  samplez <- unique(as.matrix(PCR_df[[i]])) # samples can be shorter with duplicate samples when you have triplicate. Requires one tag pairs for multiple wells
  samples <- c(samplez,rep(NA,nrow(tagz)-length(samplez))) #some tags combinations are not used and associated with NA
  match <- cbind(tagz,samples=samples) # combine tag pairs and samples
  corresp_tag <- rbind(corresp_tag,match) # for each plates
}

# create tag pcr plates (we will put tag_pairs instead of sample names)
tag_PCR_plates <- PCR_plates

# get samples position
sample_pos <- which(tag_PCR_plates %in% na.omit(corresp_tag)$samples) #samples are cells with a correspondance in corresp_tag
for(i in sample_pos){
  tag_PCR_plates[i] <- corresp_tag$pair_tag_name[match(tag_PCR_plates[i],corresp_tag$samples)] # for these cells, get the tag_pair
}

tag_PCR_plates <- as.data.frame(unname(tag_PCR_plates))

# create a xlsx workbook
wb <- openxlsx::buildWorkbook(tag_PCR_plates,rownames = F,colNames=F)

#create a style to apply to all non empty cells
csglobal<- openxlsx::createStyle(border = "TopBottomLeftRight ",halign = "center")

#apply it
 openxlsx::conditionalFormatting(wb,
                                 sheet = 1,
                                 cols = 1:ncol(tag_PCR_plates),
                                 rows = 1:nrow(tag_PCR_plates),
                                 style = csglobal,
                                 rule = '<>""',
                                 type = "expression")

# get from samples plates the positions of CTRL to color them
tag_ctrl_pos <- which(matrix(grepl("tag", PCR_plates), dim(PCR_plates)), arr.ind = TRUE)
pos_ctrl_pos <- which(matrix(grepl("T\\+", PCR_plates), dim(PCR_plates)), arr.ind = TRUE)
ctab_ctrl_pos <- which(matrix(grepl("CTAB", PCR_plates), dim(PCR_plates)), arr.ind = TRUE)
ext_ctrl_pos <- which(matrix(grepl("EXT", PCR_plates), dim(PCR_plates)), arr.ind = TRUE)

# create formating style according to control type
cstag <- openxlsx::createStyle(fgFill = "#9FC5E8")
cstpos <- openxlsx::createStyle(fgFill = "#E33838")
csctab <- openxlsx::createStyle(fgFill = "#E3E338")
csext <- openxlsx::createStyle(fgFill = "#70DC40")

#format according to cell content

openxlsx::addStyle(wb,
                   sheet = 1,
                   cols = tag_ctrl_pos[,2],
                   rows = tag_ctrl_pos[,1],
                   style = cstag,
                   stack = F)

openxlsx::addStyle(wb,
                   sheet = 1,
                   cols = pos_ctrl_pos[,2],
                   rows = pos_ctrl_pos[,1],
                   style = cstpos,
                   stack = F)
openxlsx::addStyle(wb,
                   sheet = 1,
                   cols = ctab_ctrl_pos[,2],
                   rows = ctab_ctrl_pos[,1],
                   style = csctab,
                   stack = F)
openxlsx::addStyle(wb,
                   sheet = 1,
                   cols = ext_ctrl_pos[,2],
                   rows = ext_ctrl_pos[,1],
                   style = csext,
                   stack = F)

# save the workbook and the sample/tag pairs file
openxlsx::saveWorkbook(wb, paste0(output_path,"/",file_tag_layout,".xlsx"), overwrite = T)
write.csv(corresp_tag,paste0(output_path,"/",file_corresp_tag,".csv"),row.names = F)

}
