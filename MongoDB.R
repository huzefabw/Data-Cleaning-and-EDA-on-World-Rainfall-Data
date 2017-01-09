########################################################################################################
#                             INSERTING DATA INTO MongoDB AND DATABASE QUERIES
########################################################################################################

a <- read.csv("World_Rainfall.csv", header = TRUE)
a[, 3] <- as.character(levels(a[, 3]))[a[, 3]] 


m <- mongo(collection = "a")

m$drop()
m$insert(a)
  
# Query.1-- In the country of USA which stations recorded the most rainfall which had
#           a total of 11days when 1mm or more rainfall occured in a single day  

Query.1 <- m$find('{"COUNTRY":"USA", "DAYS_1MM":11}', sort='{"TOTAL_RAIN":-1}')


# Query.2-- Which countries recorded Temeratures greater than 35 degree C

Query.2 <- m$distinct("COUNTRY", '{"MEAN_TEMP":{"$gt":35}}')


# Query.3-- Average elevation of all the countries ( higest elevation to lowest)

Query.3<- m$aggregate('[{"$group":{"_id":"$COUNTRY", "Average":{"$avg":"$ELEVATION"}}}]')


# Query.4-- Map REDUCE funciton on the elevation countries 

Query.4 <- m$mapreduce(
  map = "function(){emit(Math.floor(this.ELEVATION/10)*10, 1)}",
  reduce = "function(id, counts){return Array.sum(counts)}"
)

##########################################################################################################


# Query.1-- In the country of USA which stations recorded the most rainfall which had 
# a total of 11days when 1mm or more rainfall occured in a single day  

View(Query.1)


# Query.2-- Which countries recorded Temeratures greater than 35 degree C
Query.2


# Query.3-- Average elevation of all the countries ( higest elevation to lowest)
View(Query.3)


# Query.4-- Map REDUCE funciton on the elevation countries 
View(Query.4)

##########################################################################################################