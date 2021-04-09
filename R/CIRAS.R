#' merge_ciras
#'
#' @description get all xls ciras output from a directory and transform them into
#' csv in a 'csv' subfolder. Additionally bind them into to files (one complete, one
#' with only relevant variables) in a 'final' subfolder.
#' @param path_to_xls a character string with your path to all your ciras xls output.
#' their name must always end as _treatment_sampleID.xls (e.g. CIRAS_3_Aechmea m _DP_1.xls).
#'
#' @return csv individual and merged frames. Full and reducted to relevant variables.
#' @export
#'
#' @importFrom readxl read_excel
#' @importFrom readr read_delim
#' @importFrom dplyr nth
merge_ciras <- function(path_to_xls = "C:/Users/trist/AppData/Local/ProjetsR/Greenhouse_Holobrom/Ciras/clean"){

  wd <- path_to_xls
  # set a path to the files you want to analyse
  setwd (dir = wd)
  Mypath <-paste0(path_to_xls,"/final")
  # Output file

  #colnames
  names <- c("RecType",	"ExcelTime",	"Comment","CO2r",	"CO2a",
             "CO2d",	"H2Or",	"H2Oa",	"H2Od",	"PARi",	"PARe",
             "Red",	"Green",	"Blue",	"White",	"Tamb",	"Tcuv",
             "Tleaf",	"Aleaf",	"Flow",	"Patm",	"RH",	"Ci",	"gs",
             "VPD",	"A",	"E",	"WUE",	"rb",	"StomataR",	"Tsensor",
             "Tcontrol",	"Lcontrol",	"PLC",	"Status")


  # convert xls into csv
  data_list_xls <- list.files(path = wd ,pattern=".xls")
  for (i in 1:length(data_list_xls)){
    data_tempo <- readxl::read_excel(data_list_xls[i], skip=3)
    colnames(data_tempo)<- names
    write.csv(data_tempo, paste0(wd,"/csv/",BBmisc::explode(toString(data_list_xls[[i]]),sep = "[.]")[1],".csv"),row.names = F)
  }
  setwd (dir = paste0(wd,"/csv"))
  data_list <- list.files(pattern=".csv")

  Trtmt1 <- NULL
  id_plant1 <- NULL
  data_list_f <- list()
  for(i in 1:length(data_list)) {
    data_list_f[[i]] <-readr::read_delim(file = data_list[[i]],",", escape_double = FALSE, trim_ws = T)
    # data_list_f[[i]] <- data_list_f[[i]][,1:35]
    Trtmt1 <- c(Trtmt1,dplyr::nth(BBmisc::explode(toString(data_list[[i]]),sep = "_"),-2L))
    id_plant1 <-c(id_plant1,BBmisc::explode(tail(BBmisc::explode(toString(data_list[[i]]),sep = "_"),1), sep = "[.]")[1])
  }


  data_ciras <- list()
  j <- 1
  for (i in 1:length(data_list_f)) {


    data_ciras[[j]] <- cbind(id_plant = rep(id_plant1[i],nrow(data_list_f[[i]])),Trtmt = rep(Trtmt1[i],nrow(data_list_f[[i]])),data_list_f[[i]])
    j <- j+1
  }


  df_ciras <- dplyr::bind_rows(data_ciras, .id = 'index')
  # df_ciras <- df_ciras[,1:38]
  df_ciras <- df_ciras[,colSums(is.na(df_ciras)) == 0]
  anyNA(df_ciras)

  setwd ( dir = Mypath)
  write.csv(df_ciras, "df_ciras.csv",row.names = F)
  df_ciras_reduc <- df_ciras[,c(1,2,3,5,20,22,24)]
  write.csv(df_ciras_reduc,"df_ciras_reduc.csv",row.names = F)
}

