#' Pasco_transfo2
#'
#' @param data a data frame output from Sparkview (usually read from .csv)
#' @param ech a character vector with either the probe or sample name
#' @param name_run a character vector with the name of all your runs (e.g., c("stab1","RECO","NEE"))
#' @param select a numeric vector of the runs you want to keep (e.g., c(2,3))
#' @param A the Area
#' @param V the Volume
#'
#' @return a csv file prepared for gasfluxes package
#' @export
#' @import dplyr magrittr tibble
#' @importFrom stringr str_sub
#' @importFrom hms as_hms
#'
PASCO_transfo2 <- function(data,
                           ech,
                           name_run,
                           select,
                           A=1,
                           V=5){

    Date <- Time <- Rec_ID  <- CO2_mg_m3<- NULL

  n_run <- length(name_run) # number of runs
  nb_ech <- length(ech) # number of ech


  ncol_data <- c(1:(2*n_run+n_run*nb_ech)) # compute length of your data

  a<-split(ncol_data,sort(ncol_data%%(n_run))) # get all columns corresponding to unwanted runs
  b <- NULL
  for (i in select){
    b <- c(b,unlist(a[i]))
  }

  data <- data[,b] # remove them
  data_pasco <- data # rename

  name_run <- name_run[select] #remove unwanted run names
  n_run <- length(name_run) # change the number of run so it correspond to new data




  col_names <- rbind(paste0("Date_Time_",name_run),paste0("Time_s_",name_run))

  for (i in 1:length(col_names)){

    if (i<=2){
      colnames(data_pasco)[i] <- col_names[i]
    }
    else{
      colnames(data_pasco)[i+(ceiling(i/2)-1)*nb_ech] <- col_names[i]
    }
  }

  co2_columns <- which(grepl("Concentration",colnames(data_pasco)))


  for (i in which(1:length(co2_columns) %% nb_ech==0)){

    if (i<=nb_ech){
      colnames(data_pasco)[co2_columns][i:(i-nb_ech+1)] <- c(paste0("CO2_ppm_",ech[nb_ech:1],"_Stab"))
    }
    if (i>((2*nb_ech)+1)){
      colnames(data_pasco)[co2_columns][i:(i-nb_ech+1)] <- c(paste0("CO2_ppm_",ech[nb_ech:1],"_NEE"))
    }
    if (i>nb_ech & i<((2*nb_ech)+1)){
      colnames(data_pasco)[co2_columns][i:(i-nb_ech+1)] <- c(paste0("CO2_ppm_",ech[nb_ech:1],"_RECO"))
    }
  }

  New_Col_Names <- paste0("Hour_time_",name_run)
  position <- which(grepl("Date_Time_",colnames(data_pasco)))


  extra_col <- 1:nrow(data_pasco)
  for (i in position){
    tempo <- ifelse(data[[i]]!="",stringr::str_extract(data[[i]],"([0-1]?\\d|2[0-3]):([0-5]?\\d):([0-5]?\\d)"),NA)
    extra_col <- data.frame(extra_col,tempo)
  }

  names(extra_col) <- c("x",New_Col_Names)
  data_pasco <- cbind(data_pasco,extra_col[,2:length(extra_col)])

  data_pasco <- dplyr::mutate(.data=data_pasco,
                              Date= stringr::str_sub(data_pasco[[1]],1,10))

  position <- which(grepl("Time_s",colnames(data_pasco)))
  data_pasco <- dplyr::mutate(.data=data_pasco,
                              dplyr::across(.cols = position,.fns = ~.x*(1/3600),.names = "hour_{.col}"))

  #get all CO2 measurements
  CO2_ppm <- NULL
  j <- 1
  for (i in co2_columns){
    CO2_ppm[[j]] <- na.omit(data_pasco[,i])
    j <- j+1
  }

  #get the length of CO2
  CO2_length <- NULL
  for (i in 1:length(CO2_ppm)){
    CO2_length <- c(CO2_length,length(CO2_ppm[[i]]))
  }

  # Get the shortest length for each run
  length_vec <- NULL
  for(i in name_run){

    u<-split(CO2_length,sort(c(1:length(CO2_length))%%n_run))
    a <- min(unlist(u[which(name_run==i)]))
    length_vec <- c(length_vec,a)
  }

  CO2_ppm<-split(CO2_ppm,sort(c(1:length(CO2_length))%%n_run))

  for (i in 1:length(CO2_ppm)){

    for (j in 1:nb_ech){
      CO2_ppm[i][[1]][[j]] <- CO2_ppm[i][[1]][[j]][1:length_vec[i]]
    }

  }

  Ech_ID <- list()
  for (i in 1:n_run){
    a <- NULL
    for(j in 1:nb_ech){
      a <- c(a,rep(ech[j],length_vec[i]))
    }
    Ech_ID[[i]] <- a
  }

  Time_hms <- list()
  position <- which(grepl("Hour_time_",colnames(data_pasco)))
  for (i in 1:n_run){
    a <- unlist(rep(head(data_pasco[position[i]],length_vec[i]),nb_ech))

    Time_hms[[i]] <- a
  }

  Time_h <- list()
  position <- which(grepl("hour_Time_s",colnames(data_pasco)))
  for (i in 1:n_run){
    a <- unlist(rep(head(data_pasco[position[i]],length_vec[i]),nb_ech))

    Time_h[[i]] <- a
  }

  Flux_type <- list()
  for (i in 1:n_run){
    a <- unlist(rep(rep(name_run[i],length_vec[i]),nb_ech))

    Flux_type[[i]] <- a
  }

  gasfluxes_data_pasco <- tibble::tibble(
    CO2_ppm = unlist(CO2_ppm),
    Time = hms::as_hms(unlist(Time_hms)),
    Time_h = unlist(Time_h),
    Flux_type= unlist(Flux_type),
    Ech_ID = unlist(Ech_ID)
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

  # proby <- NULL
  # for (i in 1:nrow(gasfluxes_data_pasco)){
  #   proby_temp <- match(gasfluxes_data_pasco$Ech_ID[i],ech)
  #   proby <- c(proby,proby_temp)
  # }
  #
  # gasfluxes_data_pasco <- dplyr::mutate(gasfluxes_data_pasco,Probe=proby )
  return(gasfluxes_data_pasco)

}
