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



#RENAME
  col_names <- rbind(paste0("Date_Time_",name_run),paste0("Time_s_",name_run)) #create a vector containing date and time columns names

  for (i in 1:length(col_names)){ #for each names
    if (i<=2){ # for the two first column just get the two first names
      colnames(data_pasco)[i] <- col_names[i]
    }
    else{ # for any other columns, skip a number of columns corresponding to the number of probes/samples
      colnames(data_pasco)[i+(ceiling(i/2)-1)*nb_ech] <- col_names[i]
    }
  }

  co2_columns <- which(grepl("Concentration",colnames(data_pasco))) #get Co2 columns position (contains "Concentration")
  col_to_change<-split(co2_columns,sort(c(1:length(co2_columns))%%n_run)) #split them in n_run equalsized groups

  for (i in 1:length(col_to_change)){ # for each Co2  columns
    colnames(data_pasco)[col_to_change[[i]]] <- paste0("CO2_ppm_",ech,"_",name_run[i]) #rename them
  }

  New_Col_Names <- paste0("Hour_time_",name_run) #create names for columns time in hour
  position <- which(grepl("Date_Time_",colnames(data_pasco))) #get position of date_time columns
  extra_col <- 1:nrow(data_pasco) # create a column to be completed

  for (i in position){ #for each date_time column
    tempo <- ifelse(data[[i]]!="",stringr::str_extract(data[[i]],"([0-1]?\\d|2[0-3]):([0-5]?\\d):([0-5]?\\d)"),NA) #extract time
    extra_col <- data.frame(extra_col,tempo) #append extracol
  }

  names(extra_col) <- c("x",New_Col_Names) #rename these new col
  data_pasco <- cbind(data_pasco,extra_col[,2:length(extra_col)]) #add them to data_pasco, rming useless "x" col

  data_pasco <- dplyr::mutate(.data=data_pasco,
                              Date= stringr::str_sub(data_pasco[[1]],1,10)) #create a date only column

  position <- which(grepl("Time_s",colnames(data_pasco))) #get position of time in second columns
  data_pasco <- dplyr::mutate(.data=data_pasco,
                              dplyr::across(.cols = position,.fns = as.numeric))%>%
                dplyr::mutate(.data=data_pasco,
                              dplyr::across(.cols = position,.fns = ~.x*(1/3600),.names = "hour_{.col}")) #create hour columns

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

  #for each run, cut it to the shortest co2 probe measurements
  CO2_ppm<-split(CO2_ppm,sort(c(1:length(CO2_length))%%n_run))

  for (i in 1:length(CO2_ppm)){

    for (j in 1:nb_ech){
      CO2_ppm[i][[1]][[j]] <- CO2_ppm[i][[1]][[j]][1:length_vec[i]]
    }

  }

  #repeat each ID
  Ech_ID <- list()
  for (i in 1:n_run){
    a <- NULL
    for(j in 1:nb_ech){
      a <- c(a,rep(ech[j],length_vec[i]))
    }
    Ech_ID[[i]] <- a
  }


  #repeat time
  Time_hms <- list()
  position <- which(grepl("Hour_time_",colnames(data_pasco)))
  for (i in 1:n_run){
    a <- unlist(rep(head(data_pasco[position[i]],length_vec[i]),nb_ech))

    Time_hms[[i]] <- a
  }

  #repeat time "2"
  Time_h <- list()
  position <- which(grepl("hour_Time_s",colnames(data_pasco)))
  for (i in 1:n_run){
    a <- unlist(rep(head(data_pasco[position[i]],length_vec[i]),nb_ech))

    Time_h[[i]] <- a
  }

  #repeat flux type
  Flux_type <- list()
  for (i in 1:n_run){
    a <- unlist(rep(rep(name_run[i],length_vec[i]),nb_ech))

    Flux_type[[i]] <- a
  }

  # create gasfluxes compatible table
  gasfluxes_data_pasco <- tibble::tibble(
    CO2_ppm = unlist(CO2_ppm),
    Time = hms::as_hms(unlist(Time_hms)),
    Time_h = unlist(Time_h),
    Flux_type= unlist(Flux_type),
    Ech_ID = unlist(Ech_ID)
  )

  # mutate to add new columns
  gasfluxes_data_pasco <- dplyr::mutate(.data=gasfluxes_data_pasco,
                                        Rec_ID=paste0(Ech_ID,"_",Flux_type),
                                        CO2_mg_m3=((CO2_ppm/1000)*44.01)/24.45,
                                        Date=data_pasco$Date[1],
                                        A=A,
                                        V=V
  )

  # arrange columns position
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
