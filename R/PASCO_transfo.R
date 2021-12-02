#' PASCO data transformation for gasfluxes package
#'
#' @param data_pasco a csv file output of sparkview (PASCO app).
#' @param ech a character vector of the sample ID, must be ordered as it was in the sparkview app!!!
#' @param A area of a cross section of the chamber used for measurements (check units!!)
#' @param V Volume of the chamber used for measurements (check units!!)
#'
#' @return a csv file prepared for gasfluxes package
#' @export
#' @import dplyr magrittr tibble
#' @importFrom stringr str_sub
#' @importFrom hms as_hms
#'
PASCO_transfo <-
  function(data_pasco,
           ech,
           A,
           V){

    Date_Time_stab <- Date_Time_RECO <- Date_Time_NEE <-
      Time_Stab <- Time_RECO <- Time_NEE <- Ech_ID <- Flux_type <-
        Date <- Time <- Rec_ID <- Time_h <- CO2_mg_m3 <- NULL

    nb_ech <- length(ech)
    ncol_data <- c(1:(6+3*nb_ech))

    colnames(data_pasco)[1] <- "Date_Time_stab"
    colnames(data_pasco)[2] <- "Time_Stab"

    colnames(data_pasco)[3+nb_ech] <- "Date_Time_RECO"
    colnames(data_pasco)[4+nb_ech] <- "Time_RECO"

    colnames(data_pasco)[5+(2*nb_ech)] <- "Date_Time_NEE"
    colnames(data_pasco)[6+(2*nb_ech)] <- "Time_NEE"

    co2_columns <- which(!(colnames(data_pasco)%in% colnames(data_pasco[,c(1,2,(3+nb_ech),(4+nb_ech),(5+(2*nb_ech)),6+(2*nb_ech))])))

    colnames(data_pasco[co2_columns])

    for (i in which(1:length(co2_columns) %% nb_ech==0)){

      if (i<=nb_ech){
      colnames(data_pasco)[co2_columns][i:(i-nb_ech+1)] <- c(paste0("CO2_ppm_",ech[4:1],"_Stab"))
      }
      if (i>((2*nb_ech)+1)){
        colnames(data_pasco)[co2_columns][i:(i-nb_ech+1)] <- c(paste0("CO2_ppm_",ech[4:1],"_NEE"))
      }
      if (i<nb_ech & i<((2*nb_ech)+1)){
        colnames(data_pasco)[co2_columns][i:(i-nb_ech+1)] <- c(paste0("CO2_ppm_",ech[4:1],"_RECO"))
      }
    }

    data_pasco <- dplyr::mutate(.data=data_pasco,
                                Date= stringr::str_sub(`Date_Time_stab`,1,10),
                                Hour_time_stab= ifelse(`Date_Time_stab`!="", stringr::str_sub(`Date_Time_stab`,12,19),NA),
                                Hour_time_RECO= ifelse(`Date_Time_RECO`!="", stringr::str_sub(`Date_Time_RECO`,12,19),NA),
                                Hour_time_NEE= ifelse(`Date_Time_NEE`!="", stringr::str_sub(`Date_Time_NEE`,12,19),NA))

    data_pasco <- dplyr::mutate(.data=data_pasco,
                                hour_diff_stab=Time_Stab*(1/3600),
                                hour_diff_RECO=Time_RECO*(1/3600),
                                hour_diff_NEE=Time_NEE*(1/3600)
    )

    CO2_ppm <- NULL
    j <- 1
    for (i in co2_columns){
      CO2_ppm[j] <- na.omit(data_pasco[,i])
      j <- j+1
    }

    CO2_length <- NULL
    for (i in 1:length(CO2_ppm)){
      CO2_length <- c(CO2_length,length(CO2_ppm[[i]]))
    }


    length_stab<- min(c(CO2_length[1:nb_ech]))
    length_RECO<- min(c(CO2_length[(1+nb_ech):(2*nb_ech)]))
    length_NEE<- min(c(CO2_length[(1+2*nb_ech):(3*nb_ech)])) # to shorten everything to the shortest probe signal for the same run

    for (i in 1:length(CO2_ppm)){
      if (i <= nb_ech){
        CO2_ppm[[i]] <- CO2_ppm[[i]][1:length_stab]
      }
      if (i>((2*nb_ech)+1)){
        CO2_ppm[[i]] <- CO2_ppm[[i]][1:length_NEE]
      }
      if (i>nb_ech & i<((2*nb_ech)+1)){
        CO2_ppm[[i]] <- CO2_ppm[[i]][1:length_RECO]
      }
    }

    ech_ID_stab <- ech_ID_RECO <- ech_ID_NEE <- NULL
    for (i in 1:nb_ech){
      ech_ID_stab <- c(ech_ID_stab,rep(ech[i],length(c(na.omit(data_pasco$Hour_time_stab[1:length_stab])))))

      ech_ID_RECO <- c(ech_ID_RECO,rep(ech[i],length(c(na.omit(data_pasco$Hour_time_RECO[1:length_RECO])))))

      ech_ID_NEE <- c(ech_ID_NEE,rep(ech[i],length(c(na.omit(data_pasco$Hour_time_NEE[1:length_NEE])))))
    }

    gasfluxes_data_pasco <- tibble::tibble(
      CO2_ppm=unlist(CO2_ppm),
      Time=hms::as_hms(c(rep(na.omit(data_pasco$Hour_time_stab[1:length_stab]),nb_ech),
                         rep(na.omit(data_pasco$Hour_time_RECO[1:length_RECO]),nb_ech),
                         rep(na.omit(data_pasco$Hour_time_NEE[1:length_NEE]),nb_ech))),
      Time_h=c(rep(na.omit(data_pasco$hour_diff_stab[1:length_stab]),nb_ech),
               rep(na.omit(data_pasco$hour_diff_RECO[1:length_RECO]),nb_ech),
               rep(na.omit(data_pasco$hour_diff_NEE[1:length_NEE]),nb_ech)),
      Flux_type=c(rep("STAB",length(rep(na.omit(data_pasco$Hour_time_stab[1:length_stab]),nb_ech))),
                  rep("RECO",length(rep(na.omit(data_pasco$Hour_time_RECO[1:length_RECO]),nb_ech))),
                  rep("NEE",length(rep(na.omit(data_pasco$Hour_time_NEE[1:length_NEE]),nb_ech)))),
      Ech_ID=c(ech_ID_stab,ech_ID_RECO,ech_ID_NEE)
    )


    gasfluxes_data_pasco <- dplyr::mutate(.data=gasfluxes_data_pasco,
                                          Rec_ID=paste0(Ech_ID,"_",Flux_type),
                                          CO2_mg_m3=((CO2_ppm/1000)*44.01)/24.45,
                                          Date=data_pasco$Date[1],
                                          A=A,
                                          V=V
    )

    gasfluxes_data_pasco <- dplyr::relocate(.data=gasfluxes_data_pasco,
                                            Date,
                                            Time,
                                            Ech_ID,
                                            Flux_type,
                                            Rec_ID,
                                            Time_h,
                                            CO2_ppm,
                                            CO2_mg_m3
    )
    return(gasfluxes_data_pasco)
  }
