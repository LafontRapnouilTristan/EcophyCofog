#' createdummy
#'
#' @description create a dummy numeric data frame
#'
#' @param nbcol integer, the number of columns (i.e. variables) in your dummy data
#' @param nbrow integer, the number of rows (i.e. observations) in your dummy data
#' @param VarName character vector of length ncol, the name of your variables
#' @return a dummy data frame
#' @importFrom stats rbinom rnorm rpois runif
#'
dummy_data <- function(nbcol,nbrow,VarName=paste0("var_",1:nbcol)){

  dumat <- matrix(nrow = nbrow, ncol = nbcol, 0)
  colnames(dumat) <- VarName
  low_range <- round(runif(1,-100,100),4)
  upp_range <- round(runif(1,low_range+1,low_range+101))
  boolean <- sample(1:2,1)
  Distrib <- sample(1:5,nbcol,replace = T)

  for(i in 1:length(Distrib)){
    if (Distrib[i]==1){
      rounder <- sample(1:5,1)
      dumat[,i] <- round(runif(nbrow,low_range,upp_range),rounder)
    }
    else if (Distrib[i]==2){
      rounder <- sample(1:5,1)
      dumat[,i] <- sample(low_range:upp_range,nbrow,replace = T)
    }
    else if (Distrib[i]==3){
      rounder <- sample(1:5,1)
      lambda <- round(runif(1,0,200),rounder)
      dumat[,i] <- rpois(nbrow,lambda)
    }
    else if (Distrib[i]==4){
      rounder <- sample(1:5,1)
      mean <- round(runif(1,low_range,upp_range),rounder)
      sd <- round(runif(1,0,upp_range),rounder)
      dumat[,i] <- round(rnorm(nbrow,mean,sd))
    }
    else{
      rounder <- sample(1:5,1)
      size <- sample(1:20,1)
      proba <- round(runif(1,0,1),rounder)
      dumat[,i] <- round(rbinom(nbrow,size = size,prob=proba))
    }
  }
  return(as.data.frame(dumat))
}
