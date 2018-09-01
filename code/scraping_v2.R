##---------------------------------------------------------------##
##-----------Code to scrape and reformat rainfall data-----------##
##---------------------------------------------------------------##

#----#
# Clear workspace and load required files and libraries
#----#

rm(list=ls())

# install.packages("rvest", dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages("rvest")
# install.packages("stringi")
# install.packages("reshape2")
# install.packages('zoo')
# install.packages('tidyr', dependencies=TRUE, repos='http://cran.rstudio.com/')


library(rvest)
# library(reshape2)
library(zoo)
library(data.table)
library(tidyr)

# library(stringi)

# filestoread <- fread("../rawdata/filestoread.csv")

#----#
# read all the raw files at once into a list instead of reading one by one into separate variables
#----#

filestoread <- list.files(path = "../rawdata/", pattern="\\.html$")
setwd("../rawdata/")
allfiles <- lapply(filestoread, function(x) try(read_html(x, ... = "../rawdata/")))
setwd("../code/")

# x <- list()
# for (i in nrow(filestoread)) {
#   x[[i]] <- read_html(paste0("../rawdata/", filestoread[i]))
# }

#----#
# create function to process all the files(read previously into list) one by one 
#----#
reshape_rainfall <- function(unformattedrainfall){
  # Convert html to table
  monthly_rainfall <- as.data.frame(unformattedrainfall %>% html_table(fill=TRUE))
  
  # remove unrequired columns
  monthly_rainfall <- monthly_rainfall[4:length(colnames(monthly_rainfall))]
  # convert to data table
  monthly_rainfall <- data.table(monthly_rainfall)
  # first row as colname
  colnames(monthly_rainfall) <- as.character(monthly_rainfall[1,])
  # remove first row to avoid duplicacy
  monthly_rainfall <- monthly_rainfall[2:nrow(monthly_rainfall), ]
  monthly_rainfall$Sr <- NULL
  
  # rename the rows
  rownames(monthly_rainfall) <- 1:nrow(monthly_rainfall)
  # namesofcols <- c(colnames(monthly_rainfall[3:ncol(monthly_rainfall)]))
  
  # convert all blank rows to NA to enable autofill
  monthly_rainfall$District[monthly_rainfall$District == ""] <- NA
  # autofilling the NAs using zoo package
  monthly_rainfall$District <- na.locf(monthly_rainfall$District)
  
  # distal <- monthly_rainfall[,. (District, Taluka)]
  # monthly_rainfall$District <- NULL
  
  # remove unrequired columns
  monthly_rainfall$`Total Rain for Year` <- NULL
  monthly_rainfall$`Total Rain for Year` <- NULL
  monthly_rainfall$`Total Rain for Year` <- NULL
  
  colnames(monthly_rainfall) <- as.character(monthly_rainfall[1,])
  # remove duplicated row
  monthly_rainfall <- monthly_rainfall[2:nrow(monthly_rainfall), ]
  
  # create a blank list to generate district & taluka names repeated 12(number of months in a year) times 
  taluka <- list()
  # save the output in the blank list using for loop
  for (i in 1:nrow(monthly_rainfall)) {
    taluka[[i]] <- do.call(cbind, list(rep(monthly_rainfall$District[i], 12), rep(monthly_rainfall$Taluka[i], 12)))
  }
  # do a rbind to get the final table with each district and taluka repeated
  taluka <- do.call(rbind, taluka)
  
  #monthstal <- list()
  # repeat the names of months as required in the final format to be added to the taluka table
  for (i in 1:nrow(taluka)) {
    monthstal <- do.call(cbind, list(rep(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), nrow(taluka)/12)))
  }
  
  taluka <- cbind(taluka, monthstal)
  taluka <- data.table(taluka)
  colnames(taluka) <- c("District", "Taluka", "Month")
  # monthly_rainfall_data <- data.frame(matrix(NA, nrow = 4260, ncol = 3))
  monthly_rainfall$Taluka <- NULL
  monthly_rainfall$District <- NULL
  
  # convert the whole monthly_rainfall data to numeric
  monthly_rainfall <- data.table(sapply(monthly_rainfall, as.numeric))
  
  # generate a blank rainfall_list to reshape the data into the required format
  rainfall_list <- list()
  # this loop pics the 3 columns namely Normal Rain, Actual Rain and Rainy Days from each row for each month and reshapes it
  for (i in 1:nrow(monthly_rainfall)) {
    for (j in seq(from = 1, to = ncol(monthly_rainfall), by = 3)) {
      if (j < 37) {
        rainfall_list[[length(rainfall_list) + 1]] <- do.call(cbind, monthly_rainfall[i, j:(j+2)])
      }else{
        j = 1
      }
      
    }
  }
  # rbind the rainfall_list to get the final reshaped data
  rainfall_list <- do.call(rbind, rainfall_list)
  # cbind with taluka to get the final data
  monthly_rainfall_data <- cbind(taluka, rainfall_list)
  
  return(monthly_rainfall_data)
}

# create a blank list that will capture the data returned by our function
yearwise_monthly_compiled_data <- list()

# send the raw html files in all_files variable one by one to our reshape_rainfall function
for (i in 1:length(filestoread)) {
  yearwise_monthly_compiled_data[[i]] <- reshape_rainfall(allfiles[[i]])
  # add the year for which the data is returned
  yearwise_monthly_compiled_data[[i]]$Year <- as.numeric(substr(filestoread[i], 0, 4))
  # save year-wise files in cleaned folder
  write.csv(yearwise_monthly_compiled_data[[i]], file = paste0("../cleaned/", substr(filestoread[i], 0, 4), "_rainfall_data.csv"))
}

# rbind the final data
yearwise_monthly_compiled_data <- do.call(rbind, yearwise_monthly_compiled_data)

# send final output to output folder
write.csv(yearwise_monthly_compiled_data, file = "../output/yearwise_monthly_compiled_data.csv")
