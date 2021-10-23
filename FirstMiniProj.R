## CMSC 197 - First Mini Project

## Problem 1

setwd("/Users/Excell Joy Magno/Desktop/First-Mini-Project")   ## Sets up the working directory

pollutantMean <- function(directory, pollutant, id = 1:332){  ## Creates a function named pollutantMean with directory, pollutant, and id as arguments
  file_list <- list.files(path = directory, pattern = ".csv", full.names = T)   ## Produces character vectors of the names of .csv files in the current directory
  values <- numeric()   ## Creates an empty numeric vector                      ## The argument full.names = T prepends the directory path to the file names to give a relative file path
  for(i in id){        ## Iterates across all monitor ids
    airPollution_data <- read.csv(file_list[i])              ## Reads the ith .csv file in vector file_list and the data frame generated from it is assigned to the vector airPollution_data
    values <- c(values, airPollution_data[[pollutant]])      ## Updates the empty vector values with the pollutant element read from airPollution_data
  }
  
  mean(values, na.rm = T )     ## Returns the mean of the pollutant across all monitor list (NA values are stripped before computation proceeds)
}          

## Sample Run

## Input
pollutantMean("specdata", "nitrate", 40:50)

## Output
## [1] 1.618771


##################################################################################################################################################

## Problem 2

setwd("/Users/Excell Joy Magno/Desktop/First-Mini-Project")   ## Sets up the working directory

complete <- function(directory, id = 1:332){    ## Creates a function named complete with arguments directory and id as arguments
  file_list <- list.files(path = directory, pattern = ".csv", full.names = T)   ## Produces character vectors of the names of .csv files in the current directory
  nobs <- numeric()    ## Creates an empty numeric vector  for the number of observations
  for(i in id){        ## Iterates across all monitor ids
    airPollution_data <- read.csv(file_list[i])              ## Reads the ith .csv file in vector file_list and the data frame generated from it is assigned to the vector airPollution_data
    nobs <- c(nobs, sum(complete.cases(airPollution_data)))  ## Updates the empty vector nobs with sum of all complete cases read from airPollutant_data
  }
  
  data.frame(id, nobs)     ## Returns a data frame where the first column is the name of the file, and the second column is the number of complete cases
}

## Sample Run

## Input
complete("specdata", c(25,35,45,55,65))

## Output
##    id  nobs
## 1  25  463
## 2  35  509
## 3  45  424
## 4  55  372
## 5  65   66


####################################################################################################################################################

## Problem 3

setwd("/Users/Excell Joy Magno/Desktop/First-Mini-Project")   ## Sets up the working directory

corr <- function(directory, threshold=0){    ## Creates a function named corr with directory and threshold as arguments
  file_list <- list.files(path = directory, pattern = ".csv", full.names = T)   ## Produces character vectors of the names of .csv files in the current directory
  result <- c()   ## Creates an empty vector
  for(i in 1:332){   ## Iterates across all monitor ids
    airPollution_data <- read.csv(file_list[i])       ## Reads the ith .csv file in vector file_list and the data frame generated from it is assigned to the vector airPollution_data
    obs <- airPollution_data[complete.cases(airPollution_data),]   ## vector obs will store completely observed cases read from airPollutant_data
    if(nrow(obs) > threshold){                            ## If the number of completely observed cases is greater than the threshold,
      result <- c(result,cor(obs$sulfate,obs$nitrate))    ## vector result will be updated in every iteration with the computed correlation
    }                                                     ## between sulfate and nitrate monitor locations
    else 0
  }
  
  return(result)
}

## Sample Run

## Input
cr <- corr("specdata", 120)
head(cr); summary(cr)

## Output
##  [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
##      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##  -0.28827 -0.05735  0.08940  0.11728  0.26339  0.76313 


################################################################################################################################################################

## Problem 4

setwd("/Users/Excell Joy Magno/Desktop/First-Mini-Project")   ## Sets up the working directory

## Read the outcome data into R via the read.csv function
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

##Given the code below, modify it so that you can plot the 30-day mortality rates 
## for heart attack given the data set outcome-of-care-measures.csv
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

## Modifying the code above
deaths <- as.numeric(outcome[, 11]) ## Creates a vector named deaths
                                    ## Since we originally read the data as character,
                                    ## as.numeric function is used to coerce the column to be numeric
hist(deaths,
     main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
     xlab = "Deaths",
     col = "light blue")

## Histogram with main and axis titles; default ylab is "Frequency"
## Light blue is the color used to fill the bars