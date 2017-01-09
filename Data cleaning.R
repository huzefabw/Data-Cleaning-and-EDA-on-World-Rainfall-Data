##########################################################################################################
#                                      PACKAGES REQUIRED TO RUN THE CODE                                 
##########################################################################################################

install.packages("openxlsx")
install.packages("plyr")
install.packages("lattice")
install.packages("mongolite")

require(openxlsx)
require(plyr)
require(lattice)
require(mongolite)

##########################################################################################################
#                                         DATA LOAD AND CLEAN                                  
##########################################################################################################


data.cleaning <- function() {
  # This function loads the data into R and then cleans it
  #
  #Args:
  #  None
  #
  #Retuns:
  #  A data frame and a csv file is created with cleaned data
  
  # Below the data is loaded from Excel Sheets and combined
  a <- NULL
  for (i in 1:61) {
    data <- read.xlsx("PROJECT.xlsx", startRow = 5, colNames = T, sheet= i, 
                      cols= NULL, skipEmptyRows= F)
    a <- rbind(a, data)
  }
  
  # Empty coloumns are deleted 
  a[, 9] = NULL
  a[, 21] = NULL
  a[, 20] = NULL
  
  # New coloumn names are given 
  colnames(a)[1]  <- "ID"
  colnames(a)[4]  <- "LONGITUDE"
  colnames(a)[5]  <- "ELEVATION"
  colnames(a)[7]  <- "STATION_PRESSURE"
  colnames(a)[8]  <- "SEA_LEVEL_PRESSURE"
  colnames(a)[9]  <- "MEAN_TEMP"
  colnames(a)[10] <- "DEPART_TEMP"
  colnames(a)[11] <- "MEAN_VAPOUR_P"
  colnames(a)[12] <- "DEPART_VAPOUR_P"
  colnames(a)[13] <- "DAYS_1MM"
  colnames(a)[14] <- "TOTAL_RAIN"
  colnames(a)[15] <- "DEPART_RAIN"
  colnames(a)[16] <- "QUINTILE"
  colnames(a)[17] <- "TOTAL_SUNSHINE"
  colnames(a)[18] <- "SUNSHINE_%AVG"
  colnames(a)[19] <- "JUNK"
  
  # Values from the "Junk" column are shifted one coloumn back    
  
  len <- length(a[, 1])
  for (j in 1:len) {
    if (is.na(a[j, 19]) == FALSE) {
      a[j, 16] <- a[j, 17]
      a[j, 17] <- a[j, 18]
      a[j, 18] <- a[j, 19]
      a[j, 19] <- NA
    }
  }
  
  # Now the "Junk" and other columns which were missplaced are deleted
  a[, 19] = NULL
  a[, 16] = NULL
  a[, 6] = NULL
  
  # One empty column is created to add country names  
  a <- as.data.frame(append(a, list(C = NA), after = 2)) 
  colnames(a)[3] <- "COUNTRY"                            
  
  # Conversion of columns which are factors to character
  a[, 1] <- as.character(levels(a[, 1]))[a[, 1]]
  a[, 15] <- as.character(levels(a[, 15]))[a[, 15]]
  
  # Removes all the rows which have NA's from the ID column
  a <- a[(!is.na(a[, 1])), ]
  
  # Here the country names from ID column and places it in the County" column
  len <- length(a[, 1])
  for (i in 1:len) {
    temp <- (a[i, 1])
    if (grepl("[A-Z]", temp, ignore.case= T) == TRUE){
      j <- i + 1
      while (grepl("[0-9]", a[j, 1], ignore.case= T) == TRUE){
        a[j, 3] <- temp
        j <- j + 1
      }
    }
  }
  
  # This removes all the rows which had NA's in station column
  a <- a[(!is.na(a[, 2])), ]
  
  # This will shift the vaules one column back, which had NA's in "DAYS.1mm" column 
  len <- length(a[, 1])
  for (i in 1:len) {
    if (is.na(a[i, 13]) == TRUE) {
      a[i, 13] <- a[i, 14]
      a[i, 14] <- a[i, 15]
    }
  }
  
  # Column no longer requried
  a[, 15] = NULL
  
  # Next all rows which had NA's in "TOTAL.RAIN" and "DAYS.1mm" column are deleted 
  a <- a[(!is.na(a[, 14])), ]
  a <- a[(!is.na(a[, 13])), ]
  
  # The "T" are replaced by 0.5 in "TOTAL.RAIN" column
  len <- length(a[, 1])
  for (i in 1:len) {
    if (a[i, 14] == "T") {
      a[i, 14] <- 0.5
    }
  }
  
  # Conversion of columns which are cahracter to numeric
  a[, 14] <- as.numeric(a[, 14])
  a[, 13] <- as.numeric(a[, 13])
  
  # Negative values form "TOTAL.RAIN" column is deleted
  a <- a[(a[, 14] >= 0), ]  
  
  # Removes the rows which has value more than 31 in "DAYS.1mm" column
  a <- a[(a[, 13] < 32), ] 
  
  # Columns no longer required
  a[, 12] = NULL
  a[, 10] = NULL
  
  # Rounding off the values and conversion from factors to numeric 
  a[, 8] <- as.character(levels(a[, 8]))[a[, 8]] 
  a[, 8] <- as.numeric(a[, 8]) 
  a[, 6] <- as.numeric(levels(a[, 6]))[a[, 6]]
  a[, 9] <- as.numeric(levels(a[, 9]))[a[, 9]]
  a[, 1] <- as.numeric(a[, 1])
  
  # Removing NA's from "ELEVATION" column
  a <- a[(!is.na(a[, 6])) ,]
  
  # Last column is deleted
  a[, 14] = NULL
  rownames(a) <- NULL
  View(a)
  
  # An excel file is cereated in the R directory 
  write.csv(a, "World_Rainfall.csv", row.names = FALSE)
  
}



#########################################################################################################

data.cleaning()

#########################################################################################################