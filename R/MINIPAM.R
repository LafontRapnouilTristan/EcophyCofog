#' minipam
#'
#' @description take as input a csv dataframe containing output
#' of the minipam. For ETR and FvFm measurements only. And return
#' clean files containing each type of measurements + the ETR curves.
#'
#' @param Name_of_the_input_file a character string (or vector), name of the csv input (if possible the output of merge_minipam).
#' @param input_path the path leading to the input file.
#' @param rec_vec a vector containing the REC id of the measurements you want to keep in the final frame. Default is NULL, it will keep all the measures from the original file.
#' @return dataframes and graphs.
#' @export
#' @importFrom stats coef lm na.omit
#' @importFrom utils head read.csv2 tail write.csv write.csv2
#' @importFrom magrittr %<>% %>%
#' @import ggplot2 dplyr
#'
minipam <-
  function(Name_of_the_input_file,
           input_path,
           rec_vec = NULL) {

    Date <- y <- data_list <- data_type <- PAR <- ETR <- REC <- NULL

    ifelse(!dir.exists(file.path(input_path, paste0("results"))), dir.create(file.path(input_path, paste0("results"))), FALSE)
    ifelse(!dir.exists(file.path(input_path, paste0("graph"))), dir.create(file.path(input_path, paste0("graph"))), FALSE)

    output_path <- paste0(input_path,"/graph")
    my_data<- read.csv2(paste0(input_path,"/",Name_of_the_input_file), header=TRUE)

    if (ncol(my_data)<5){
      my_data<- read.csv(paste0(input_path,"/",Name_of_the_input_file), header=TRUE)
    }
    #skip first row, as it is useless

    #Separate two files#####
    mysmalldata<-my_data[2:nrow(my_data),2:11]
    mysmalldata%<>%mutate(as.Date(Date))
    #cut off the first row and column (useless metadata)
    mysmalldata %<>% mutate(y = 0)
    # create a vector y of a length equal to the length
    # of your frame, filled with 0. Will be used to index
    # your rows and sort them.
    # Bind your data with the vector


    #Index Type SLCS/E#####
    i<-1
    # This script uses "while" loop. You need a value to count
    # the number of loops you need. This is "i", it'll be reseted
    # to one numerous time.

    while (i<=nrow(mysmalldata)){
      # While "i" is < or = to the lenght of my frame
      if (mysmalldata$Type[i]=="SLCS"){
        while (mysmalldata$Type[i]!="SLCE"){
          mysmalldata$y[i]<-1
          i=i+1
        }
      }
      # Put a 1 in the "y" column if the "type" of the row "i"
      # is "SLCS", and add 1 to "i" so you can go to the next row.
      # Put a 1 in y untill we reach a SLCE (end of Light curve measure)
      else if (mysmalldata$Type[i]=="SLCE"){
        mysmalldata$y[i]<-1
        i<-i+1
      }
      # When we reach the end of the light curve "SLCE" put a 1
      else {
        i=i+1
      }
    }
    # Otherwise, just go to the next row with i+1

    # Split the data in FvFm and RLC results####
    #the split is done accordingly to the index of y
    dataFvFm <- mysmalldata %>% filter(y!=1)
    dataRLC <- mysmalldata %>% filter(y!=0)
    data_list <- list(dataFvFm,dataRLC)

    ## Create FvFm file ####
    if (nrow(dataFvFm !=0)){
      # Remove "SCHS" and "SHCE" rows ####
      # Rows of type == SCHS/SHCE are metadata rows
      # They are useless for our analysis and
      # can be removed
      dataFvFm<-dataFvFm[dataFvFm$Type=="FO"|dataFvFm$Type=="C",]

      # Merge rows informations ####

      # if you look at the dataframe you can
      # see that pair of rows could be merged
      i<-1
      while (i<=nrow(dataFvFm)){
        if(dataFvFm$Type[i]=="C"){
          # for all C rows
          dataFvFm$Mark[i+1]<-dataFvFm$Mark[i]
          # Take the informations of the next row and put it
          # in the C row
          dataFvFm$y[i]<-1
          # and change Y to 0 to remove
          # C row that are now useless
          i<-i+1
          # And so on
        } else
        {i<-i+1}
      }

      # Removing useless rows #####
      dataFvFm<-dataFvFm %>% filter(y!=1)
      # keep rows with y different from 0
      dataFvFmred<-dataFvFm[,c(5:7,9)]
      # Remove some columns

      # Clean and sort data #####

      # To keep track of our measures we add
      # a number before each row (The number of
      # the REC from the MiniPAM)
      dataFvFmred<-cbind(dataFvFmred,rec = 0)
      # create a column with 0

      i<-1
      while (i <= nrow(dataFvFmred)){
        char<-dataFvFmred$Mark[i]
        char2<-toString(char)
        char3<-BBmisc::explode(char2,sep="\\ ")[3]
        # extract the REC values from Mark column
        dataFvFmred$rec[i]<-char3
        # Fill the column REC with these values
        i=i+1
      }

      FvFmfinal<-dataFvFmred[,c(5,2:4)]
      colnames(FvFmfinal) <- c("REC","F","Fm","Y_II")

      # one last cut to keep values of interest
      if (is.null(rec_vec)==F){
        FvFmfinal %<>% filter(REC %in% rec_vec)
      }


      # Save the file #####
      name<-paste(FvFmfinal$REC[1],"_a_",FvFmfinal$REC[nrow(FvFmfinal)],".csv")
      write.csv2(FvFmfinal,paste0(input_path,"/results/", name), row.names = F)
    }

    #### Create RLC file ####
    colnames(dataRLC) <- c("Date","Time","Type","No.","Mark","F","Fm","PAR","YII","ETR","date2","y")
    dataRLC <- subset(dataRLC,dataRLC$YII!= "-")
    assign("dataRLC",dataRLC, envir = .GlobalEnv)
    #remove RLC failure (incomplete)
    clean_vec <- NULL
    for (i in 1:nrow(dataRLC)){

      if (dataRLC$Type[i]=="SLCS"){
        b <- ifelse(dataRLC$Type[i+1]!="REG1",0,1)
      }
      clean_vec <- c(clean_vec,b)
    }
    dataRLC$clean <- clean_vec
    dataRLC %<>% filter(clean!=0)
    assign("dataRLCclean",dataRLC, envir = .GlobalEnv)

    # Clean the file ######
    if(dim(dataRLC)[1]!=0){
      # Each RLC measure has a start "SLC Start" and an end "SLC end"
      # here we will put a number for each measure of RLC in the order
      # they appear (i.e. in the chronological order). From 1 to the
      # total number of measure in the file.
      i<-1
      compt<-0
      #set the counters
      while (i<=nrow(dataRLC)){
        if (dataRLC$Type[i]=="SLCS"){
          compt<-compt+1
          # if you have a start,it means that you are starting a new measure
          # add one to the previous value.
          dataRLC$y[i]<-compt
          # count this measure
          while (dataRLC$Type[i]!="SLCE")
          {dataRLC$y[i]<-compt
          i=i+1} }
        # same number for all the row between a start and an end
        # then move to the next row
        else if (dataRLC$Type[i]=="SLCE"){
          dataRLC$y[i]<-compt
          # you reach the end, count it
          i=i+1
          #go to the next row
        }
      }


      # Delete useless entries
      dataRLCred<-dataRLC[,c(1:3,5:11)]



      assign("dataRLCred",dataRLCred, envir = .GlobalEnv)

      # Add columns to be filled #####

      # The ETR curve that we are interested in is fitted with
      # two models : REG1 and REG2.
      # REG1 is taking in account the photoinhibition
      # REG2 is not

      # Create columns for all the values of interest
      # Each of these is indexed with 1 or 2 depending the
      # model from which we get them
      #   alpha  <-electrons/photons, initial RLC slope. Quantum efficiency
      #             of the photosynthesis
      #   ETRmax <- Maximum electron transport rate
      #   Ik     <- Minimum saturating irradiance
      #   beta   <- photoinhibition parameter
      #   ETRmPot<- Maximum potential light saturated ETR
      #   REG    <- Will stock the mean squarre of both models to see
      #             the best fitted one
      #   PARmax <- The value of PAR for which the ETR is max, according to
      #             the models (1 & 2) and measured by the miniPAM
      #   photoinhib <- Will be used to say if there is photoinhib or not

      dataRLCf<-cbind(dataRLCred,alpha1 = 0, ETRmax1 = 0, Ik1 = 0, beta = 0, ETRmPot = 0,
                      alpha2 = 0, ETRmax2 = 0, Ik2 = 0,REG1 = 0,REG2 = 0,PARmax1 = 0,
                      PARmax2 = 0,photoinhib = 0,ETRmax = 0,PARmax = 0)
      # Create the frame


      # Fill columns with REG1 and 2######

      i<-1
      while (i <= nrow(dataRLCf)){
        # for all rows
        if(dataRLCf$Type[i]=="REG1"){
          # correponding to REG1
          char<-dataRLCf$Mark[i]
          char2<-toString(char)
          # consider Mark content as character
          char3<-BBmisc::explode(char2,sep="\\ ")[3]
          char4<-BBmisc::explode(char3,sep=",")[1]
          # Extract the value from this string of character

          if (char4=="-"){
            # if char4 is empty then, fill the row with NAs
            # we use i-1 because we want the values to jump from
            # the row REG1 to the row SLCS
            # If not clear, check the dataframe RLCf
            dataRLCf[i-1,11:15]<-NA
          }
          else{
            # if char4 contains informations, then we want to
            # extract it and put it in the corresponding columns
            dataRLCf$alpha1[i-1]<-char4   #alpha1
            charb3<-BBmisc::explode(char2,sep="\\ ")[5]
            charb4<-BBmisc::explode(charb3,sep=",")[1]
            dataRLCf$ETRmax1[i-1]<-charb4   #ETRmax1
            charc3<-BBmisc::explode(char2,sep="\\ ")[7]
            charc4<-BBmisc::explode(charc3,sep=",")[1]
            dataRLCf$Ik1[i-1]<-charc4   #Ik1
            chard3<-BBmisc::explode(char2,sep="\\ ")[12]
            chard4<-BBmisc::explode(chard3,sep=",")[1]
            dataRLCf$beta[i-1]<-chard4   #beta
            chare3<-BBmisc::explode(char2,sep="\\ ")[14]
            chare4<-BBmisc::explode(chare3,sep=",")[1]
            dataRLCf$ETRmPot[i-1]<-chare4   #ETRmPot
          }
          i=i+1
        }
        # we do the same as for REG1 but with REG2
        # As REG2 is the second row after SLCS we use
        # i-2 to fill the row.
        else if(dataRLCf$Type[i]=="REG2"){
          char<-dataRLCf$Mark[i]
          char2<-toString(char)
          char3<-BBmisc::explode(char2,sep="\\ ")[3]
          char4<-BBmisc::explode(char3,sep=",")[1]
          dataRLCf$alpha2[i-2]<-char4   #alpha2
          charb3<-BBmisc::explode(char2,sep="\\ ")[5]
          charb4<-BBmisc::explode(charb3,sep=",")[1]
          dataRLCf$ETRmax2[i-2]<-charb4   #ETRmax2
          charc3<-BBmisc::explode(char2,sep="\\ ")[7]
          charc4<-BBmisc::explode(charc3,sep=",")[1]
          dataRLCf$Ik2[i-2]<-charc4   #Ik2
          i=i+1
        }
        else
        {i<-i+1}
      }


      # Same for REG2 but really need to check it


      # Deleting REG1 and 2 rows#####
      dataRLCf<-dataRLCf[!(dataRLCf$Type=="REG1"|dataRLCf$Type=="REG2"),]
      # As we just extracted datas from the rows with
      # Type = REG1 or REG2, they are now useless
      # We thus can delete them

      # Computing ETR value according REG1 and 2 models ######
      cols <- names(dataRLCf)[c(5:9,11:18)]
      dataRLCf %<>% mutate(across(.cols=all_of(cols),.fns=as.numeric))
      assign("dataRLCf",dataRLCf, envir = .GlobalEnv)
      #REG1 is taking in account the photoinhibition
      # the formula to compute it is in the Manual of the
      # MiniPAM II
      i<-1
      while (i <= nrow(dataRLCf)){
        if(dataRLCf$Type[i]=="SLCS"){
          a<-i
          # search for a Starting row and trigger the counter a
          if (is.na(dataRLCf$alpha1[i])==TRUE){
            i<-i+15
            # if we have no alpha1 value for this light curve, jump to
            # the next light curve (i.e. 15 rows later)
          }
          else {
            while (dataRLCf$Type[i]!="SLCE"){
              # otherwise, untill you reach the end of the LC
              b<-i+1
              dataRLCf$REG1[b]<-as.numeric(dataRLCf$ETRmPot[a])*(1-exp(-((as.numeric(dataRLCf$alpha1[a])*dataRLCf$PAR[b])/as.numeric(dataRLCf$ETRmPot[a]))))*exp(-((as.numeric(dataRLCf$beta[a])*dataRLCf$PAR[b])/as.numeric(dataRLCf$ETRmPot[a])))
              # use the formula with the alpha1 for each PAR that the mimiPAM
              # tested. a is referring to the row containing the alpha value
              # and b is used to fetch each PAR value (13,from 0 to 3000)
              if (is.na(dataRLCf$alpha2[i])==FALSE){
                dataRLCf$REG2[b]<-as.numeric(dataRLCf$ETRmax2[a])*tanh((as.numeric(dataRLCf$alpha2[a])*dataRLCf$PAR[b])/as.numeric(dataRLCf$ETRmax2[a]))
              }
              #We will now do the same but with the REG2 formula
              # from the MiniPAM II manual
              i=i+1

            }
          }
        } else
        {i<-i+1}
      }

      assign("dataRLCfETR",dataRLCf, envir = .GlobalEnv)



      # Compute PARmax according to REG1 and REG2#####
      i<-1
      while (i <= nrow(dataRLCf)){
        if(dataRLCf$Type[i]=="SLCS"){
          dataRLCf$PARmax2[i]<-(atanh((0.998*as.numeric(dataRLCf$ETRmax2[i]))/as.numeric(dataRLCf$ETRmax2[i]))*as.numeric(dataRLCf$ETRmax2[i]))/as.numeric(dataRLCf$alpha2[i])
          dataRLCf$PARmax1[i]<-(-(as.numeric(dataRLCf$ETRmPot[i]))/as.numeric(dataRLCf$alpha1[i]))*log((as.numeric(dataRLCf$beta[i]))/(as.numeric(dataRLCf$beta[i])+as.numeric(dataRLCf$alpha1[i])))
          i<-i+1
        }
        else
        {i<-i+1}
      }
      assign("dataRLCfPAR",dataRLCf, envir = .GlobalEnv)
      #Check the formulae

      # Which model fit the best our data ####

      # In order to choose one model we will use the least square
      # method. ("Moindre carr?" in french, explaining the choice
      # of the names used in the following section)

      for(i in 1:nrow(dataRLCf)){
        if(dataRLCf$Type[i]=="FO"){
          a<-i-1
          # we use this so the Least square value is put in the
          # SLCS row instead of the FO
          # For PAR = 0 there is no difference between the models,
          # least square value is thus 0
          # For the FO
          b <- (i+1):(i+13)
          moindre1<-sum(na.omit(((dataRLCf$REG1[b]-as.numeric(dataRLCf$ETR[b]))^2)))
          moindre2<-sum(na.omit(((dataRLCf$REG2[b]-as.numeric(dataRLCf$ETR[b]))^2)))
          # For all the PAR value a one light curve (untill we reach SLCE, the end),
          # we compute the least square for REG1 and REG2 with the formula of the least square
          # the square of the difference between the observed values and
          # the model predicted ones
          dataRLCf$REG1[a]<-moindre1
          dataRLCf$REG2[a]<-moindre2
        }
      }
      # We enter these values in the dataframe, col REG1 and REG2, first row for each

      #Set character col as numeric####
      dataRLCf[5:ncol(dataRLCf)]%<>% mutate_if(is.character,as.numeric)
      # Plot ETR against PAR ####
      i<-1
      while (i <= nrow(dataRLCf)){
        if (dataRLCf$Type[i]=="FO"){
          x<-c()
          z<-c()
          r1<-c()
          r2<-c()
          #renaming variable, x are PAR values, z observed ETR,
          # r1 and r2 ETRs from REG1 and 2
          date<-toString(dataRLCf$Date[i-1])
          heure<-toString(dataRLCf$Time[i-1])
          # Get date and time into characters
          titre<-paste("d",date,"h",heure,sep="_")
          titre<-gsub(":","/",titre)
          titre<-gsub("/","_",titre)
          # Give a title to the graph
          ajout<-paste("REG1 : S=",round(dataRLCf$REG1[i-1],3)," PARmax=",round(dataRLCf$PARmax1[i-1],3))
          ajout2<-paste("REG2 : S=",round(dataRLCf$REG2[i-1],3)," PARmax=",round(dataRLCf$PARmax2[i-1],3))
          # The round values of least squares and PARmax of both models
          while(dataRLCf$Type[i]!="SLCE"){
            x<-c(x,dataRLCf$PAR[i])
            z<-c(z,dataRLCf$ETR[i])
            r1<-c(r1,dataRLCf$REG1[i])
            r2<-c(r2,dataRLCf$REG2[i])
            i=i+1
            # Extracting the values to be plotted
          }
          dat <- data.frame("measured" = z,"PAR" = x, "reg1"=r1,"reg2"=r2)
          # Create a dataframe with these values
          dat <- dat %>%
            reshape2::melt(id.vars = "PAR", value.name = "ETR", variable.name = "data_type")


          g <- dat %>% ggplot2::ggplot(aes(x=PAR,y=ETR,color=data_type))+
            geom_line()+
            # plot lines
            ggtitle(label=titre)+
            # add a title
            theme(axis.text.x =  element_text(angle=0))+
            annotate(geom="text", x=1000, y=7, label=ajout,
                     color="black")+
            annotate(geom="text", x=1000, y=5, label=ajout2,
                     color="black")
          # add PARmax and S? values
          ggsave(filename = paste0(titre,'.jpg'), plot = g, device = "jpg", path = output_path)
          #Save a pdf
        }
        else{
          i<-i+1
          print("yes")
        }
      }

      # Creating the final dataframe #####

      # Now that we have the plots we can get rid of
      # many rows (only keep SLCS.
      dataRLCf<-dataRLCf[dataRLCf$Type == "SLCS",]
      dataRLCf<-dataRLCf[,c(1,2,11:25)]
      # Now we delete useless columns)

      # Then we just determine the ETR and PAR max values
      # that we want to keep (the ones from the best model)
      # To assess if there is photoinhibition in our plants
      # we substract the least square of our models
      i<-1
      while (i<=nrow(dataRLCf)){
        if ((dataRLCf$REG2[i]-(dataRLCf$REG1[i])>0)){
          dataRLCf$photoinhib[i]<-1
          # If the difference is positive (i.e. REG2 has more residuals
          # than REG1) we enter 1 in the photoinhib column.
          # This means that a model with photoinhib (REG1) fit better
          # with our data (because it has less residuals)
          i=i+1}
        else {
          (dataRLCf$photoinhib[i]<-0)
          i=i+1}
        # On the other end we fill this column with 0 when it is negative
      }

      # We now extract ETRmax and PARmax values
      i<-1
      while (i<=nrow(dataRLCf)){
        if(dataRLCf$photoinhib[i]==1)
        {dataRLCf$ETRmax[i]<-dataRLCf$ETRmax1[i]
        dataRLCf$PARmax[i]<-dataRLCf$PARmax1[i]
        i=i+1}
        # if we have a 1, then REG1 is the best model and
        # we take ETRmax1 and PARmax1
        else{
          dataRLCf$ETRmax[i]<-dataRLCf$ETRmax2[i]
          dataRLCf$PARmax[i]<-dataRLCf$PARmax2[i]
          i=i+1
        }
        # If not we take REG2 values
      }

      i<-1
      while (i<=nrow(dataRLCf)){
        if(dataRLCf$PARmax[i]==Inf)
        {dataRLCf$PARmax[i]<-dataRLCf$PARmax2[i]
        i=i+1}
        # I don't know how it is possible but if PARmax is infinite,
        # then we use the one from REG2
        else{
          i=i+1
        }
      }

      # Save the dataframe ####

      # we save it in the set directory
      # the name is the date and time of both first
      # and last rows of the dataframe
      # it is in csv

      name<-paste(dataRLCf$Date[1],"_",dataRLCf$Time[1],"_a_",dataRLCf$Date[nrow(dataRLCf)],"_",dataRLCf$Time[nrow(dataRLCf)],".csv")
      name<-gsub(":","_",name)
      name<-gsub("/","_",name)
      write.csv2(dataRLCf,paste0(input_path,"/results/", name), row.names = F)
    }
  }
