#' minipam
#'
#' @description take as input a csv dataframe containing output
#' of the minipam. For ETR and FvFm measurements only. And return
#' clean files containing each type of measurements + the ETR curves.
#'
#' @param Name_of_the_input_file a character string (or vector), name of the csv input (if possible the output of merge_minipam).
#' @param input_path the path leading to the input file.
#' @param path_to_ID_match a csv file containing the measure ID, date, time and REC of the measure.
#' @return dataframes and graphs.
#' @export
#' @importFrom stats coef lm na.omit
#' @importFrom utils head read.csv2 tail write.csv write.csv2 read.csv
#' @importFrom magrittr %<>% %>%
#' @import ggplot2 dplyr
#'
minipam <-
  function (Name_of_the_input_file,
            input_path,
            path_to_ID_match) {

# Set thingz up

## objects
  Date <- y <- data_list <- data_type <- PAR <- ETR <- REC <- Time <- clean <- ID <- NULL # create objects required by the function to avoid warning message

## output folders
  ifelse(!dir.exists(file.path(input_path, paste0("results"))),
         dir.create(file.path(input_path, paste0("results"))),
         FALSE)
  ifelse(!dir.exists(file.path(input_path, paste0("graph"))),
         dir.create(file.path(input_path, paste0("graph"))),
         FALSE) # create results and graph folder after checking if they already existed

  output_path <- paste0(input_path, "/graph") # create the path

## import data
  my_data <- read.csv2(paste0(input_path, "/", Name_of_the_input_file), # read csv data file, change from csv2 to csv if needed
                       header = TRUE)
  if (ncol(my_data) < 2) {
    my_data <- read.csv(paste0(input_path, "/", Name_of_the_input_file),
                        header = TRUE)
  }

## import idmatch file
  ID_match <- read.csv2(paste0(path_to_ID_match), # read csv rec/ID maatching file, change from csv2 to csv if needed
                        header = TRUE)
  if (ncol(ID_match) < 2) {
    ID_match <- read.csv(paste0(path_to_ID_match),
                         header = TRUE)
  }
  ID_match <- data.frame(apply(ID_match,2,as.character)) # everything as character

  for (i in 1:nrow(ID_match)){
    date_stockage <- BBmisc::explode(ID_match$Date[i], sep = "/")
    ID_match$Date[i] <- paste0(substr(date_stockage[3],3,4),"-",date_stockage[2],"-",date_stockage[1])
  } # extract date


  mysmalldata <- my_data[2:nrow(my_data), 2:11] # remove useless row and data
  mysmalldata <- mysmalldata %>% mutate(as.Date(Date)) # change date from character to date
  mysmalldata <- mysmalldata %>% mutate(y = 0) # create a variable that will be used to remove row

## SPLIT FvFm from RLC data
  i <- 1
  while (i <= nrow(mysmalldata)) { # for every row in my data
    if (mysmalldata$Type[i] == "SLCS") { # if measure type is SLCS -> Light curve start
      while (mysmalldata$Type[i] != "SLCE") { # While it is not SLCE -> Light curve end
        mysmalldata$y[i] <- 1 # y = 1
        i = i + 1
      }
    }
    else if (mysmalldata$Type[i] == "SLCE") { # If SLCE y = 1
      mysmalldata$y[i] <- 1
      i <- i + 1
    }
    else { # Just move on and let y with 0 for all other rows (FvFm data)
      i = i + 1
    }
  }

  dataFvFm <- mysmalldata %>% filter(y != 1) # y = 0 -> dataFvFm
  dataRLC <- mysmalldata %>% filter(y != 0) # y = 1 -> dataRLC
  colnames(dataFvFm) <- c("Date", "Time", "Type",
                          "No.", "Mark", "F", "Fm", "PAR",
                          "YII", "ETR", "date2", "y") # rename


  if (nrow(dataFvFm != 0)) { # check if theres actually some data in dataFvFm (we might only have RLC data)
    dataFvFm <- dataFvFm[dataFvFm$Type == "FO" | dataFvFm$Type ==
                           "C", ] # Take only rows of type Fo or C (only one with wanted data)
    i <- 1
    while (i <= nrow(dataFvFm)) { # Take information of row C and place it in FO row
      if (dataFvFm$Type[i] == "C") {
        dataFvFm$Mark[i + 1] <- dataFvFm$Mark[i]
        dataFvFm$y[i] <- 1 # mark C rows with y = 1
        i <- i + 1
      }
      else {
        i <- i + 1
      }
    }

    dataFvFm <- dataFvFm %>% filter(y != 1) # remove C rows while we stored them in FO
    dataFvFmred <- dataFvFm[, c(5:7, 9)] # rm useless rows
    dataFvFmred <- cbind(dataFvFmred, rec = 0) # add a rec columns filled with 0

    i <- 1
    while (i <= nrow(dataFvFmred)) { # extract rec and add it to the file
      char <- dataFvFmred$Mark[i]
      char2 <- toString(char)
      char3 <- BBmisc::explode(char2, sep = "\\ ")[3]
      dataFvFmred$rec[i] <- char3
      i = i + 1
    }

    FvFmfinal <- dataFvFmred[, c(5, 2:4)] # select wanted rows
    colnames(FvFmfinal) <- c("REC", "F", "Fm", "Y_II") # rename
    FvFmfinal <- left_join(ID_match,FvFmfinal,by=c("REC")) # combine the ID/rec match file with the datafile
    name <- paste0(FvFmfinal$ID[1], "-", FvFmfinal$ID[nrow(FvFmfinal)],"_FvFm.csv") # create a name
    write.csv2(FvFmfinal, paste0(input_path, "/results/", name), row.names = F) # write a csv
  }

  # Job's done for DataFvFm lets go with DataRLC

  if (dim(dataRLC)[1] != 0) { # if there is RLC data

    colnames(dataRLC) <-
      c("Date", "Time", "Type","No.", "Mark", "F", "Fm", "PAR","YII", "ETR", "date2", "y") # rename RLC file
    dataRLC <- subset(dataRLC, dataRLC$YII != "-") # remove all row were data failed to be acquired (filled with "-" by the minipam software)
    dataRLC <- dataRLC %>% mutate(Time=substr(Time,1,5),Date=substr(Date,3,10)) # get time and date to the desired format


    clean_vec <- NULL # create a clean vec
    for (i in 1:nrow(dataRLC)) {
      if (dataRLC$Type[i] == "SLCS") {
        b <- ifelse(dataRLC$Type[i + 1] != "REG1",
                    0, 1)
      }
      clean_vec <- c(clean_vec, b) # create a vector containing 1 when the SLCS row isnt followed by a REG1 row
      # the software is supposed to compute REG1, if it fails to, data cannot be used and can be discarded
    }


    dataRLC$clean <- clean_vec # add the clean vec to data
    dataRLC <- dataRLC %>% filter(clean != 0) # and filter the rows where it equals 1


    # Delete useless entries
    dataRLCred <- dataRLC[, c(1:3, 5:11)]

    # create new columns where well store RLC parameters
    dataRLCf <- cbind(dataRLCred, alpha1 = 0, ETRmax1 = 0,
                      Ik1 = 0, beta = 0, ETRmPot = 0, alpha2 = 0, ETRmax2 = 0,
                      Ik2 = 0, REG1 = 0, REG2 = 0, PARmax1 = 0, PARmax2 = 0,
                      photoinhib = 0, ETRmax = 0, PARmax = 0)


    i <- 1
    while (i <= nrow(dataRLCf)) {
      if (dataRLCf$Type[i] == "REG1") {
        char <- dataRLCf$Mark[i]
        char2 <- toString(char) # transform the mark column to character
        char3 <- BBmisc::explode(char2, sep = "\\ ")[3] # explode the string
        char4 <- BBmisc::explode(char3, sep = ",")[1]

        if (char4 == "-") {
          dataRLCf[i - 1, 11:15] <- NA # if there is an error (e.g. "-") fill with NA
        }
        else {
          dataRLCf$alpha1[i - 1] <- char4
          charb3 <- BBmisc::explode(char2, sep = "\\ ")[5]
          charb4 <- BBmisc::explode(charb3, sep = ",")[1]
          dataRLCf$ETRmax1[i - 1] <- charb4
          charc3 <- BBmisc::explode(char2, sep = "\\ ")[7]
          charc4 <- BBmisc::explode(charc3, sep = ",")[1]
          dataRLCf$Ik1[i - 1] <- charc4
          chard3 <- BBmisc::explode(char2, sep = "\\ ")[12]
          chard4 <- BBmisc::explode(chard3, sep = ",")[1]
          dataRLCf$beta[i - 1] <- chard4
          chare3 <- BBmisc::explode(char2, sep = "\\ ")[14]
          chare4 <- BBmisc::explode(chare3, sep = ",")[1]
          dataRLCf$ETRmPot[i - 1] <- chare4 # get the different values
        }
        i = i + 1
      }
      else if (dataRLCf$Type[i] == "REG2") { # same for reg2
        char <- dataRLCf$Mark[i]
        char2 <- toString(char)
        char3 <- BBmisc::explode(char2, sep = "\\ ")[3]
        char4 <- BBmisc::explode(char3, sep = ",")[1]
        dataRLCf$alpha2[i - 2] <- char4
        charb3 <- BBmisc::explode(char2, sep = "\\ ")[5]
        charb4 <- BBmisc::explode(charb3, sep = ",")[1]
        dataRLCf$ETRmax2[i - 2] <- charb4
        charc3 <- BBmisc::explode(char2, sep = "\\ ")[7]
        charc4 <- BBmisc::explode(charc3, sep = ",")[1]
        dataRLCf$Ik2[i - 2] <- charc4
        i = i + 1
      }
      else {
        i <- i + 1
      }
    }

    # remove row REG1 and 2 as we extract all we needed
    dataRLCf <- dataRLCf[!(dataRLCf$Type == "REG1" | dataRLCf$Type == "REG2"), ]

    # get column names
    cols <- names(dataRLCf)[c(5:9, 11:18)]

    # transform as.numeric these columns
    dataRLCf <- dataRLCf %>% mutate(across(.cols = all_of(cols), .fns = as.numeric))


    i <- 1
    while (i <= nrow(dataRLCf)) {
      if (dataRLCf$Type[i] == "SLCS") { # at the beginning of the curve
        a <- i
        if (is.na(dataRLCf$alpha1[i]) == TRUE) { # if there is na go to the next curve (+15 rows)
          i <- i + 15
        }
        else {
          while (dataRLCf$Type[i] != "SLCE") { # while this is not the end of the measurements
            b <- i + 1
            dataRLCf$REG1[b] <- dataRLCf$ETRmPot[a] *
              (1 - exp(-((dataRLCf$alpha1[a] *
                            dataRLCf$PAR[b])/dataRLCf$ETRmPot[a]))) *
              exp(-((dataRLCf$beta[a] * dataRLCf$PAR[b])/dataRLCf$ETRmPot[a])) #compute the model

            if (is.na(dataRLCf$alpha2[i]) == FALSE) {
              dataRLCf$REG2[b] <- dataRLCf$ETRmax2[a] *
                tanh((dataRLCf$alpha2[a] *
                        dataRLCf$PAR[b])/dataRLCf$ETRmax2[a])
            }
            i = i + 1
          }
        }
      }
      else {
        i <- i + 1
      }
    }

    i <- 1
    while (i <= nrow(dataRLCf)) {
      if (dataRLCf$Type[i] == "SLCS") {
        dataRLCf$PARmax2[i] <- (atanh((0.998 * dataRLCf$ETRmax2[i])/dataRLCf$ETRmax2[i]) *
                                  dataRLCf$ETRmax2[i])/dataRLCf$alpha2[i]
        dataRLCf$PARmax1[i] <- (-(dataRLCf$ETRmPot[i])/dataRLCf$alpha1[i]) *
          log((dataRLCf$beta[i])/(dataRLCf$beta[i] +
                                    dataRLCf$alpha1[i]))
        i <- i + 1
      }
      else {
        i <- i + 1
      }
    }
    for (i in 1:nrow(dataRLCf)) {
      if (dataRLCf$Type[i] == "FO") {
        a <- i - 1
        b <- (i + 1):(i + 13)
        moindre1 <- sum(na.omit(((dataRLCf$REG1[b] -
                                    dataRLCf$ETR[b])^2)))
        moindre2 <- sum(na.omit(((dataRLCf$REG2[b] -
                                    dataRLCf$ETR[b])^2)))
        dataRLCf$REG1[a] <- moindre1
        dataRLCf$REG2[a] <- moindre2
      }
    }

    # change character asnumeric if needed
    dataRLCf[5:ncol(dataRLCf)] <- dataRLCf[5:ncol(dataRLCf)] %>% mutate_if(is.character,as.numeric)

    # combine with the idmatch file
    dataRLCf <- left_join(dataRLCf,ID_match %<>% select(-REC),by=c("Date","Time"))

    # replace ID that are NA with 0
    dataRLCf <- mutate_at(dataRLCf, "ID", ~replace(., is.na(.), "0"))


    for (i in 1:length(dataRLCf$ID)) {
      if (dataRLCf$Type[i] != "SLCE")  {
        if (dataRLCf$ID[i] == "0") {
          dataRLCf$ID[i] <- dataRLCf$ID[i-1]
        }
      }
    }
    for (i in 1:nrow(dataRLCf)) {
      if (dataRLCf$Type[i] == "SLCE") {
        dataRLCf$ID[i] <- 1
      }
    }
    # place the ID in all rows


    dataRLCf <- dataRLCf %>% filter(ID!=0) # get all the rows with an ID (e.g. not 0)


    i <- 1 # create graph for each RLC
    while (i < nrow(dataRLCf)) {
      if (dataRLCf$Type[i] == "FO") {
        x <- c()
        z <- c()
        r1 <- c()
        r2 <- c()
        titre <- paste(dataRLCf$ID[i])
        ajout <- bquote("REG1 : "~ S^2==.(round(dataRLCf$REG1[i - 1], 3))~" PARmax="~.(round(dataRLCf$PARmax1[i - 1], 3)))
        ajout2 <- bquote("REG2 : "~ S^2==.(round(dataRLCf$REG2[i - 1], 3))~" PARmax="~.(round(dataRLCf$PARmax2[i - 1], 3)))
        while (dataRLCf$Type[i] != "SLCE") {
          x <- c(x, dataRLCf$PAR[i])
          z <- c(z, dataRLCf$ETR[i])
          r1 <- c(r1, dataRLCf$REG1[i])
          r2 <- c(r2, dataRLCf$REG2[i])
          i = i + 1
        }
        dat <- data.frame(measured = z, PAR = x, reg1 = r1,
                          reg2 = r2)
        dat <- dat %>% reshape2::melt(id.vars = "PAR",
                                      value.name = "ETR", variable.name = "data_type")
        g <- dat %>% ggplot2::ggplot(aes(x = PAR, y = ETR,
                                         color = data_type)) + geom_line() + ggtitle(label = titre) +
          theme(axis.text.x = element_text(angle = 0)) +
          annotate(geom = "text", x = 1100, y = 8,
                   label = ajout, color = "black") + annotate(geom = "text",
                                                              x = 1100, y = 5, label = ajout2, color = "black")
        ggsave(filename = paste0(titre, "_minipam.jpg"),
               plot = g, device = "jpg", path = output_path)

        print(paste("Graphique", dataRLCf$ID[i-1]))
      }
      else {
        i <- i + 1
      }
    }


    dataRLCf <- dataRLCf[dataRLCf$Type == "SLCS", ] # take only the SLCS rows
    dataRLCf <- dataRLCf[, c(1, 2, 11:25)] # take only some columns


    i <- 1
    while (i <= nrow(dataRLCf)) { # look the best fitting model
      if ((dataRLCf$REG2[i] - (dataRLCf$REG1[i]) > 0)) { # REG1 has lower sum of square, then it is photoinhibition
        dataRLCf$photoinhib[i] <- 1
        i = i + 1
      }
      else {
        (dataRLCf$photoinhib[i] <- 0) # else fill with 0
        i = i + 1
      }
    }

    i <- 1
    while (i <= nrow(dataRLCf)) {
      if (dataRLCf$photoinhib[i] == 1) { # if photoinhib <- 1 then ETRmax is computed according to ETRmax1
        dataRLCf$ETRmax[i] <- dataRLCf$ETRmax1[i]
        dataRLCf$PARmax[i] <- dataRLCf$PARmax1[i]
        i = i + 1
      }
      else { # else ETRmax is computed according to ETRmax2
        dataRLCf$ETRmax[i] <- dataRLCf$ETRmax2[i]
        dataRLCf$PARmax[i] <- dataRLCf$PARmax2[i]
        i = i + 1
      }
    }

    i <- 1
    while (i <= nrow(dataRLCf)) { # deal with some rare cases
      if (dataRLCf$PARmax[i] == Inf) {
        dataRLCf$PARmax[i] <- dataRLCf$PARmax2[i]
        i = i + 1
      }
      else {
        i = i + 1
      }
    }

    dataRLCf <- left_join(ID_match,dataRLCf,by=c("Date","Time")) # join with ID match
    name <- paste0(dataRLCf$ID[1], "-", dataRLCf$ID[nrow(dataRLCf)], "_RLC.csv") # create file name
    write.csv2(dataRLCf, paste0(input_path, "/results/", name), row.names = F) # write the file
  }
}
