rm(list=ls())

# install.packages("rvest")
# install.packages("stringi")
# install.packages("reshape2")
# install.packages('zoo')
# install.packages('tidyr')

library(rvest)
# library(reshape2)
library(zoo)
library(data.table)
library(tidyr)

# library(stringi)

# filestoread <- fread("../rawdata/filestoread.csv")

filestoread <- list.files(path = "../rawdata/", pattern="\\.html$")
setwd("../rawdata/")
allfiles <- lapply(filestoread, function(x) try(read_html(x, ... = "../rawdata/")))
setwd("../code/")

# x <- list()
# for (i in nrow(filestoread)) {
#   x[[i]] <- read_html(paste0("../rawdata/", filestoread[i]))
# }

return_rainfall_data <- function(unformattedrainfalldata){
  monthly_rainfall <- as.data.frame(unformattedrainfalldata %>% html_table(fill=TRUE))
  
  monthly_rainfall <- monthly_rainfall[4:length(colnames(monthly_rainfall))]
  monthly_rainfall <- data.table(monthly_rainfall)
  colnames(monthly_rainfall) <- as.character(monthly_rainfall[1,])
  
  monthly_rainfall <- monthly_rainfall[2:nrow(monthly_rainfall), ]
  monthly_rainfall$Sr <- NULL
  
  rownames(monthly_rainfall) <- 1:nrow(monthly_rainfall)
  namesofcols <- c(colnames(monthly_rainfall[3:ncol(monthly_rainfall)]))
  
  monthly_rainfall$District[monthly_rainfall$District == ""] <- NA
  monthly_rainfall$District <- na.locf(monthly_rainfall$District)
  
  distal <- monthly_rainfall[,. (District, Taluka)]
  # monthly_rainfall$District <- NULL
  
  monthly_rainfall$`Total Rain for Year` <- NULL
  monthly_rainfall$`Total Rain for Year` <- NULL
  monthly_rainfall$`Total Rain for Year` <- NULL
  
  colnames(monthly_rainfall) <- as.character(monthly_rainfall[1,])
  monthly_rainfall <- monthly_rainfall[2:nrow(monthly_rainfall), ]
  
  taluka <- list()
  
  for (i in 1:nrow(monthly_rainfall)) {
    taluka[[i]] <- do.call(cbind, list(rep(monthly_rainfall$District[i], 12), rep(monthly_rainfall$Taluka[i], 12)))
  }
  
  taluka <- do.call(rbind, taluka)
  
  #monthstal <- list()
  for (i in 1:nrow(taluka)) {
    monthstal <- do.call(cbind, list(rep(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), nrow(taluka)/12)))
  }
  
  taluka <- cbind(taluka, monthstal)
  taluka <- data.table(taluka)
  colnames(taluka) <- c("District", "Taluka", "Month")
  # monthly_rainfall_data <- data.frame(matrix(NA, nrow = 4260, ncol = 3))
  monthly_rainfall$Taluka <- NULL
  monthly_rainfall$District <- NULL
  
  monthly_rainfall <- data.table(sapply(monthly_rainfall, as.numeric))
  rainfall_list <- list()
  
  for (i in 1:nrow(monthly_rainfall)) {
    for (j in seq(from = 1, to = ncol(monthly_rainfall), by = 3)) {
      if (j < 37) {
        rainfall_list[[length(rainfall_list) + 1]] <- do.call(cbind, monthly_rainfall[i, j:(j+2)])
      }else{
        j = 1
      }
      
    }
  }
  
  rainfall_list <- do.call(rbind, rainfall_list)
  monthly_rainfall_data <- cbind(taluka, rainfall_list)
  return(monthly_rainfall_data)
}

yearwise_monthly_compiled_data <- list()

for (i in 1:length(filestoread)) {
  yearwise_monthly_compiled_data[[i]] <- return_rainfall_data(allfiles[[i]])
  yearwise_monthly_compiled_data[[i]]$Year <- as.numeric(substr(filestoread[i], 0, 4))
  write.csv(yearwise_monthly_compiled_data[[i]], file = paste0("../cleaned/", substr(filestoread[i], 0, 4), "_rainfall_data.csv"))
}

yearwise_monthly_compiled_data <- do.call(rbind, yearwise_monthly_compiled_data)

write.csv(yearwise_monthly_compiled_data, file = "../output/yearwise_monthly_compiled_data.csv")
# monthly_rainfall_data <- 