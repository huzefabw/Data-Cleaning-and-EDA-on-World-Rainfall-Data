#########################################################################################################
#                                      DATA ANALYSIS USING R FUNCTIONS
#########################################################################################################


# ANSWER1

Answer.1 <- function() {
  # This function finds the average rainfall in each country 
  #
  #Args:
  #   None
  #
  #Retuns:
  #  A data frame of countries and thier average rainfall
  
  a <- read.csv("World_Rainfall.csv", header = TRUE)
  a[, 3] <- as.character(levels(a[, 3]))[a[, 3]] 
  temp.2 <- 0
  count <- 0
  Country <- 0
  j <- 1
  Average.Rain <- 0
  len <- length(a[, 1])
  
  for (i in 2:len) {
    x <- a[i, 3]
    y <- a[i - 1, 3] 
    if (x == y) {
      temp.1<- a[i - 1, 12]
      temp.2<- temp.1 + temp.2
      count <- count + 1
    } else {
      temp.2 <- temp.2 + a[i - 1, 12]
      Country[j] <- a[i - 1, 3]
      count <- count + 1
      Average.Rain[j] <- temp.2 / count
      temp.2 <- 0
      count <- 0
      j <- j + 1
    }
  } 
  
  temp.2 <- temp.2 + a[i, 12]
  Country[j] <- a[i, 3]
  Average.Rain[j] <- temp.2 / count
  
  F0<-as.data.frame(Average.Rain) 
  F1<-as.data.frame(Country)   
  final <- cbind(F1,F0)
  
  final <- final[order(final$Country),]
  final[, 1] <- as.character(levels(final[, 1]))[final[, 1]]
  final <- final[(!duplicated(final[, 1], fromLast = T)), ]
  final$Average.Rain <- round(final$Average.Rain, 2)
  
  ANSWER1 <- final 
  ANSWER1<- ANSWER1[order(-ANSWER1$Average.Rain), ]
  rownames(ANSWER1) <- NULL
  
  View(ANSWER1)
  write.csv(ANSWER1, "ANSWER_1.csv", row.names = FALSE)
}



#ANSWER2

Answer.2 <- function() {
  # This function gives a bar plot of the top 30 countries which had the highest rainfall 
  #
  #Args:
  #   None
  #
  #Retuns:
  #  A barplot of countries and rainfall
  
  ANSWER1 <- read.csv("ANSWER_1.csv", header = TRUE)
  ANSWER1[, 1] <- as.character(levels(ANSWER1[, 1]))[ANSWER1[, 1]] 
  ans2 <- ANSWER1[1:30, 2]
  con2 <- ANSWER1[1:30, 1]
  counts <- (ANSWER1[, 2])
  x11()
  bar.plot <- barplot(ans2, main ="Highest Rainfall Amongst Different Countries", 
                      ylab = "Rainfall amount", col = "red", space = 0.3, 
                      ylim = c(0,550), axis.lty = 1, xaxs = "r", yaxs = "i", offset = 6)
  
  text(bar.plot, y = -25, labels = con2, cex = 0.7, srt = 30, xpd = TRUE, pos = 2)
  axis(side = 1, at = bar.plot, labels = F)
  box()
}



# ANSWER3

Answer.3 <- function() {
  # This function finds the station that recorded the most rainfall
  #
  #Args:
  #   None
  #
  #Retuns:
  #  Name and the rainfall amount of country
  
  a <- read.csv("World_Rainfall.csv", header = TRUE)
  a[, 3] <- as.character(levels(a[, 3]))[a[, 3]] 
  ans3 <- max(a$TOTAL_RAIN)
  t1 <- a[which(a$TOTAL_RAIN == ans3), 2]
  t2 <- a[which(a$TOTAL_RAIN == ans3), 3]
  ANSWER3 <- paste("The highest rainfall was of",ans3, "mm and it occured at",t1,"in",t2)
  ANSWER3
}




#ANSWER4

Answer.4 <- function() {
  # This function finds the rainfall at the highest elevation
  #
  #Args:
  #   None
  #
  #Retuns:
  #  Amount of rainfall and elevation
  
  a <- read.csv("World_Rainfall.csv", header = TRUE)
  a[, 3] <- as.character(levels(a[, 3]))[a[, 3]] 
  ans4 <- max(a$ELEVATION)
  t11 <- a[which(a$ELEVATION == ans4), 12]
  t22 <- a[which(a$ELEVATION == ans4), 2]
  t33 <- a[which(a$ELEVATION == ans4), 3]
  ANSWER4 <- paste("Rainfall at the highest elevation of",ans4,"mm was recorded as",
                   t11,"mm, Station name and Country is:",t22,",",t33)
  ANSWER4
}



#ANSWER5

Answer.5 <- function() {
  # This function finds the average rainfall in the world in one month
  #
  #Args:
  #   None
  #
  #Retuns:
  #  The average rainfall amount
  
  ANSWER1 <- read.csv("ANSWER_1.csv", header = TRUE)
  ANSWER1[, 1] <- as.character(levels(ANSWER1[, 1]))[ANSWER1[, 1]] 
  ANS5 <- round(sum(ANSWER1[, 2])/(length(ANSWER1[, 2])), 2)
  ANSWER5 <- paste("The average rainfall for the month of JUNE 2015, in the world (from given data) was",ANS5,"mm")
  ANSWER5 
}


#ANSWER6

Answer.6 <- function() {
  # This function finds the sattion that recorded the most days when rainfall was more than 1mm 
  #
  #Args:
  #   None
  #
  #Retuns:
  #  Station name, country name and the number of days when rainfall was more than 1mm
  
  a <- read.csv("World_Rainfall.csv", header = TRUE)
  a[, 3] <- as.character(levels(a[, 3]))[a[, 3]] 
  ans6 <- max(a$DAYS_1MM)
  t16 <- a[which(a$DAYS_1MM == ans6), 2]
  t26 <- a[which(a$DAYS_1MM == ans6), 3]
  ANSWER6 <- paste(t16,"station located in",t26,"recorded a total of",ans6,
                   "days on which the rainfall was greater than 1mm")
  ANSWER6
}



#ANSWER7

Answer.7 <- function() {
  # This function plots a graph between Temperature and Vapour pressure 
  #
  #Args:
  #   None
  #
  #Retuns:
  #  A graph
  
  a <- read.csv("World_Rainfall.csv", header = TRUE)
  a[, 3] <- as.character(levels(a[, 3]))[a[, 3]] 
  vapor.pressure <- a[, 10]
  temperature <- a[, 9]
  
  
  panel_fn <- function(x, y, ...)
  {
    panel.xyplot(x, y, ...)
    panel.xyplot(x, y, type="smooth", col="red", grid = T)
  }
  x11()
  xyplot(vapor.pressure ~ temperature, xlab = "Temperature", ylab = "Vapour Pressure",
         panel=panel_fn, main = "Temperature v/s Vapour Pressure")
}



#ANSWER8

Answer.8 <- function() {
  # This function plots a graph between Atmospheric Pressure and Elevation 
  #
  #Args:
  #   None
  #
  #Retuns:
  #  A graph
  
  a <- read.csv("World_Rainfall.csv", header = TRUE)
  a[, 3] <- as.character(levels(a[, 3]))[a[, 3]] 
  elevation <- a[, 6]
  pressure <- a[, 7]
  
  panel_fn1 <- function(x, y, ...)
  {
    panel.xyplot(x, y, ...)
    panel.xyplot(x, y, type="r", col="red", grid = T)
  }
  x11()
  xyplot(elevation ~ pressure, xlab = "Pressure", ylab = "Elevation", 
         panel=panel_fn1, main = " Atmospheric Pressure v/s Elevation")
}


#########################################################################################################
 
# Here are the answers to all the 8 questions 

Answer.1()
Answer.2()
Answer.3()
Answer.4()
Answer.5()
Answer.6()
Answer.7()
Answer.8()
