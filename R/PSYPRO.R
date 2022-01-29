#' psypro
#'
#' @description Transform psypro output files into csv dataframe with
#' mean water potential of your triplicate.
#'
#' @param usedset the predetermined name of your set 0,1,2 or 3.
#' @param lim min and max values expected out of the psypro for you
#' samples. Used to standardized graphs for faster reading. Discuss with
#' lab members to understand!!
#' @param ID_vec a vector of length 8 (number of sensors) with your samples' ID.
#' Empty sensors are named 0
#' @param path_to_calibration path to you calibration file.
#' @param psypro_output path to your psypro output
#' @return dataframes and graphs.
#' @export
#' @importFrom stats coef lm na.omit
#' @importFrom utils head read.csv2 tail write.csv write.csv2
#' @importFrom magrittr %<>% %>%
#' @import ggplot2
psypro <-
  function(usedset ,
           lim = c(-3,2),
           ID_vec,
           path_to_calibration ,
           psypro_output) {

    todaysdate <- Serie <- sec <- `psy uV` <- NULL

    The_path_to_calibration <- path_to_calibration
    # The path that lead to the file containing the sensor's calibration of your psypro sensor
    The_path_to_psypro_output <- paste0(psypro_output,"/",usedset)
    # The path to the file containing psypro output you want to analyze
    # This file should have 16 files. 2 per sensor of the psypro
    The_path_to_save_the_graphics <-paste0(The_path_to_psypro_output,"/graph")

    ifelse(!dir.exists(file.path(The_path_to_save_the_graphics)), dir.create(file.path(The_path_to_save_the_graphics)), FALSE)



    CalibrationPsypro <- readr::read_delim(The_path_to_calibration, ";", escape_double = FALSE, trim_ws = TRUE)
    # get the excel file with wire's calibrations

    a <- toString(file.info(list.files(path=The_path_to_psypro_output,pattern=".csv", full.names=TRUE))$mtime)
    # Get the informations of the files in the directory
    # Here we are looking for the date and time of creation

    a1 <- BBmisc::explode(a,sep=", ")[1]
    # Take only the date and time of the first file (they've all
    # been created at the same moment)
    a2 <- BBmisc::explode(a1,sep=" ")[1]
    # From this we extract the date
    a3 <- BBmisc::explode(a1,sep=" ")[2]
    # And then the time

    calib <- CalibrationPsypro
    # Rename the calib file
    set <- subset(calib, Serie == usedset)
    # We make a subset of the file with the sensors
    # that are in the used set.
    # If you don't have defined serie in your calibration file
    # but only the "name" of your sensor use the other script

    digits <- strsplit(set$Sensor, "")
    # create a list containing each character of the name of a sensor

    digits2 <- NULL
    # create a vector to store the final digit of sensor's name
    # digit that indicates the position of the sensor and thus
    # the corresponding output file of the psypro
    v <- NULL

    for (i in 1:length(digits)) {
      # for each element of the list
      if (is.na(as.numeric(tail(digits[[i]],1)))== T){
        last_digit <- head(tail(digits[[i]],2),1)
        v <- last_digit
      }
      else {
        last_digit <- tail(digits[[i]],1)
        v <- last_digit
      } # take the last character

      digits2 <- c(digits2,last_digit) # and store it
    }
    set <- cbind(set,as.numeric(digits2)) # then add it to the data frame

    colnames(set) <- c("sensor","serie","slope","intercept","R2","pos_sensor")
    # rename col to be cleaner

    set <- set[order(set$pos_sensor),]
    # Order the data frame so the first row contains the
    # parameter of the sensor in position one on the psypro
    # All this must be done for "homemade" series of wire.
    # If you used the default one (11,12...18 ; 21,22...28 ; etc...)
    # Then you can skip all the part about the digits.



    #let's have fun####


    for (i in set$pos_sensor) { # For each sensor in your set
      df <-readr::read_csv(paste0(The_path_to_psypro_output,"/P02_Ps#",i,"_50point.csv")) #load the sensor's data file
      assign(paste0("sensor_",i),tail(df[,5:6],50) )
      #renaming it and taking only "PsyuV" and "sec" columns and 50 last points
    }

    for (i in set$pos_sensor){ #for each sensor
      a <- get(paste0("sensor_",i)) # take corresponding data
      b <- set[i,1] # take corresponding sensor ID
      g <- a %>%  ggplot2::ggplot(ggplot2::aes(x = sec,y =`psy uV`))+
        ggplot2::geom_point( ) +
        ggplot2::ggtitle ( label = paste0("sensor_",b,"  Pos_",i))+
        ggplot2::ylim(lim)
      ggplot2::ggsave(filename = paste0("sensor_",i,'.jpg'), plot = g, device = "jpg", path = The_path_to_save_the_graphics)
      g2 <- a %>% ggplot2::ggplot(ggplot2::aes(x = sec,y =`psy uV`))+
        ggplot2::geom_point( ) +
        ggplot2::ggtitle ( label = paste0("sensor_",b,"  Pos_",i))
      ggplot2::ggsave(filename = paste0("sensor_",i,'_non_standard.jpg'), plot = g2, device = "jpg", path = The_path_to_save_the_graphics)
      # Plot psy against the time in sec
      assign(paste0("curve_sensor_",i),a[2:5,]) # take the 2nd to 5th points of the curve (is it enough??)

    }

    fitted_intercepts <- NULL
    Water_potential <- NULL # Create an empty vector to store Waterpot values
    for(i in 1:nrow(set)){ #for each sensor
      j <- set[i,6]
      b <- get(paste0("curve_sensor_",j)) # take the dots of the corresponding curve

      intercept_fit <- coef(lm(b$`psy uV`~b$sec))[["(Intercept)"]] # intercept of the fit on 2nd to fifth point

      Water_potential <- c(Water_potential,as.numeric(set[i,3])*intercept_fit+as.numeric(set[i,4]))
      #the water potential is sensor's slope*intercept of the lm + sensor's intercept
      fitted_intercepts <- c(fitted_intercepts,intercept_fit)
    }


    #Bind#####

    data <- cbind(date = a2,time = a3,ID_plant = ID_vec,set[,c(1,2,6,3,4,5)],fitted_intercept=fitted_intercepts ,Water_potential, calib_file = basename(path_to_calibration))
    # bind water pot values to the set dataframe, date and time column contain
    # value extracted and stored in a2 and a3
    todaysdate <- stringr::str_replace_all(a2,"/","_")
    write.csv(data, paste0(The_path_to_psypro_output,"/Z_Data_set_",usedset,"__",todaysdate,".csv"),row.names = F)
  }

