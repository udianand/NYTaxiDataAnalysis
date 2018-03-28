#Data Analyst Code Challenge


##NOTE: Required Packages: Uncomment to install them.

###package.install("fitdistrplus")
###package.install("logspline")
###package.install("dplyr")
###package.install("randomForest")
###package.install("splitstackshape")
###package.install("ggmap")
###package.install("dplyr")



#Question 1:
#-----------#

##Part(a)
# Data was loaded into R graphically using the Import Dataset

##Part (b)
trainData = green_tripdata_2015_09
dim(trainData) # Number of rows: 149426, Number of columns: 21

##################################################################

#Question 2: 
#-----------#


##Part(a): Plot a histogram of the number of the trip distance ("Trip Distance")

##figure 1##
hist(trainData$Trip_distance, main = "Histogram of Trip Distance", xlab = "Trip Distance in miles", col = "green", border="black"
     ,  xlim=c(0,25), las=1, breaks=1000, prob=TRUE)
curve(dnorm(x, mean=mean(trainData$Trip_distance), sd=sd(trainData$Trip_distance)), add=TRUE)

##figure2##
number_of_trip_distance = table(trainData$Trip_distance)
hist(number_of_trip_distance, main = "Histogram of Number of Trip Distance", xlab = "Number of Trip Distance", col = "green", border="black",
    xlim=c(0,5000) , las=1, breaks=100, prob=TRUE)
curve(dnorm(x, mean=mean(number_of_trip_distance), sd=sd(number_of_trip_distance)), add=TRUE)

##Part(b): 

##Seems to be a Right Skewed Distribution
library(fitdistrplus)
library(logspline)

descdist(as.numeric(trainData$Trip_distance), discrete = FALSE)
descdist(as.numeric(number_of_trip_distance), discrete = FALSE)

##INFERENCE##
#It seems that the data is right skewed. Furthermore, from Figure 1 we can infer that most of the 
#trip distances are concentrated in the 0 to 5 mile range (highly dense). Similarly, for Figure 2, 
#0 -100 range seems to be the most dense. 
#Something, that we can infer is that short travel distances are extremely popular and that 
#as the trip distance increases the frequency of the trip decreases.
#Moreover,  I wanted to find if the distribution follows  a known distribution. 
#However, both the trip distance and number of trip distance do not seem to follow any known distribution.
#This is further confirmed in Question (5)


#############################################################################

#Question 3:
#-----------#

##Part(a): Report mean and median trip distance grouped by hour of day.

trainData$Date_time <- as.POSIXct(trainData$lpep_pickup_datetime, format="%Y-%m-%d %H:%M:%S")
trainData$Hour <- format(trainData$Date_time, '%H')
result = aggregate(Trip_distance ~ Hour,data=trainData, FUN=length)
mean(result$Trip_distance) #62288.58
median(result$Trip_distance) #60496

##Part(b): We'd like to get a rough sense of identifying trips that originate or terminate at one of the NYC 
##area airports. Can you provide a count of how many transactions fit this criteria, the average fair, 
##and any other interesting characteristics of these trips.

#Bounding box of JFK and Laguardia

#Source: Flickr GeoAPI,
##JFK Bounding Box: -73.8352, 40.6195, -73.7401, 40.6659
## Source: https://www.flickr.com/places/info/12520380
library(dplyr)
jfk_pickup = filter(trainData, ( Pickup_latitude >= 40.6195 & Pickup_latitude <= 40.6659) & 
                    (Pickup_longitude >= -73.8352 & Pickup_longitude <= -73.7401))

jfk_dropoff = filter(trainData, ( Dropoff_latitude >= 40.6195 & Dropoff_latitude <= 40.6659) & 
                       (Dropoff_longitude >= -73.8352 & Dropoff_longitude <= -73.7401))

jfk_pickup_average_fare = sum(jfk_pickup$Total_amount)/nrow(jfk_pickup)
jfk_dropoff_average_fare = sum(jfk_dropoff$Total_amount)/nrow(jfk_pickup)

##Laguardia Bounding Box:	-73.8895, 40.7664, -73.8550, 40.7931
##Source: https://www.flickr.com/places/info/12520509

lag_pickup = filter(trainData, ( Pickup_latitude >= 40.7664 & Pickup_latitude <= 40.7931) & 
                      (Pickup_longitude >= -73.8895 & Pickup_longitude <= -73.8550))

lag_dropoff = filter(trainData, ( Dropoff_latitude >= 40.7664 & Dropoff_latitude <= 40.7931) & 
                       (Dropoff_longitude >= -73.8895 & Dropoff_longitude <= -73.8550))

lag_pickup_average_fare = sum(lag_pickup$Total_amount)/nrow(jfk_pickup)
lag_dropoff_average_fare = sum(lag_dropoff$Total_amount)/nrow(jfk_pickup)

airport_name = c("JFK Pickup", "JFK Dropoff", "Lag PickUp", "Lag Dropoff")
airport_stats = c(nrow(jfk_pickup), nrow(jfk_dropoff), nrow(lag_pickup), nrow(lag_dropoff))
airport_average_fare = c(jfk_pickup_average_fare, jfk_dropoff_average_fare, lag_pickup_average_fare, lag_dropoff_average_fare)
airport = data.frame(airport_name, airport_stats, airport_average_fare)

#Plot the average fare
barplot(airport$airport_average, main="Average Fare",names.arg=airport$airport_name, col = "green")

#Plot the number of pickups and dropoff
barplot(airport$airport_stats, main="Number of pickups and dropoff",names.arg=airport$airport_name, col = "green")

#######################################################################################

#Question 4:
#-----------#

##Part(a): Build a derived variable for tip as a percentage of the total fare.

total_cost_except_tip  = trainData$Fare_amount + trainData$Extra + trainData$MTA_tax + trainData$Tolls_amount + trainData$improvement_surcharge
tip_amount = trainData$Total_amount - total_cost_except_tip
trainData$Tip_as_perecentage_of_total_amount = (tip_amount/trainData$Total_amount)*100

##Part(b): Build a predictive model
###Plausible options: GLM, Decision Trees, Random Forest, XGBoost
###Option Chosen: Random Forest

##Check for NA. I found that variable Trip_type had NA. I changed all of them to 1.
##this is because there are only handful of NA's and the number of 1's was significantly higher than 2's
##Number of 1's: 1461506, Number of 2's: 33416

predictData = trainData[1:10000,]

#Check for NA and take action where appropriate
predictData$Trip_type[is.na(predictData$Trip_type)] = 1
predictData$Tip_as_perecentage_of_total_amount[is.na(predictData$Tip_as_perecentage_of_total_amount)] = median(predictData$Tip_as_perecentage_of_total_amount, na.rm = "True")
predictData$Tip_as_perecentage_of_total_amount[is.infinite(predictData$Tip_as_perecentage_of_total_amount)] = 0
predictData$Trip_type[is.na(predictData$Trip_type)] = median(predictData$Trip_type, na.rm = "True")

#Convert to Factors
predictData$Payment_type = as.factor(predictData$Payment_type)
predictData$Passenger_count = as.factor(predictData$Passenger_count)
predictData$Hour = as.numeric(predictData$Hour)

summary(predictData)

#Data Division
data_division = sample(nrow(predictData), floor(nrow(predictData)*0.7))
predict_train_data = predictData[data_division,]
predict_test_data = predictData[-data_division,]

str(predictData)
#Random Forest Model

#RATIONALE BEHIND CHOOSING PREDICTORS
# 1. Instead of going with total payment as a predictor. I skip it and rather include all the predictor variables, 
#    this is to make the model more exhaustive as any effect of other cost varibales will get masked in the
#    pressence of total cost.
# 2. I also included the passenger count, as i think more the people travelling higher is the probability of
#    getting a (higher) tip.
# 3. Pickup/Dropoff latitudes and longitudes as more posh locations might get you more tip.
# 4. Paying by card vs paying by cash. Since payment type is masked as 1 and 2. I wanted to see effect of payment
#    type on the tip

library(randomForest)
set.seed(415)
rf_fit <- randomForest(Tip_as_perecentage_of_total_amount  ~ Fare_amount + Extra + MTA_tax + Tolls_amount + improvement_surcharge + Trip_type + 
                    Pickup_longitude + Hour + Pickup_latitude + Dropoff_longitude + Dropoff_latitude + Passenger_count + Payment_type,
                    data = predict_train_data, importance=TRUE, ntree = 200)

which.min(rf_fit$mse)

varImpPlot(rf_fit)
print(rf_fit)

# predict and evaluate on the test set
pred = predict(rf_fit, newdata = predict_test_data)

rf_rmse = sqrt(mean((pred-predict_test_data$Tip_as_perecentage_of_total_amount)^2))
rf_rmse

rf_mae = mean(abs(pred-predict_test_data$Tip_as_perecentage_of_total_amount)^2)
rf_mae

##NOTE: I WAS NOT ABLE TO RUN WITH SET NTREE ON MY PERSONAL LAPTOP AS IT IS COMPUTATIONALLY VERY INTENSIVE FOR MY PERSONAL
##LAPTOP. AND CURRENTLY DO NOT HAVE A STUDENT ACCOUNT FOR AMAZON CLOUD COMPUTING TO OFFLOAD THE COMPUTATIONS.

#######################################################################################


#Question 5:
#-----------#

#If the data leaps out and screams some question of you that we haven't asked, 
#ask it and answer it! Use this as an opportunity to highlight your special skills and philosophies.

##References:

#1. http://www.milanor.net/blog/maps-in-r-introduction-drawing-the-map-of-europe/
#2. http://minimaxir.com/2015/11/nyc-ggplot2-howto/
#3. https://www.flickr.com/places/info/2459115

#round to four digits to increase the pool of identical coordinates
trainData$Lat = round(trainData$Pickup_latitude, 4)
trainData$Long = round(trainData$Pickup_longitude, 4)

#create a new column which is a concatentation of the latitude and longitude
trainData$Coord <- paste(trainData$Lat, trainData$Long, sep=',')

#calculate the total fair amount, average fair and total number of trips of all available coordinates
total_fare_amount = aggregate(Total_amount ~ Coord, data=trainData, FUN=sum)
total_number_trips = aggregate(Total_amount ~ Coord, data=trainData, FUN=length)
average_fair = aggregate(Total_amount ~ Coord, data = trainData, FUN=function(x) {sum(x)/length(x)})
total_hours = aggregate(Hour ~ Coord, data=trainData, FUN=length)

#create a new data set comprising of coordinates, the total fare amount the total number of trips and
#and the average fair
fareData =  cbind(total_fare_amount, total_number_trips$Total_amount, average_fair$Total_amount, total_hours$Hour)

colnames(fareData)[colnames(fareData) == "total_number_trips$Total_amount"] = "Total_number_of_trips"
colnames(fareData)[colnames(fareData) == "average_fair$Total_amount"] = "Average_fair"
colnames(fareData)[colnames(fareData) == "total_hours$Hour"] = "Hour"

#split the coordinates back to the original latitude and longitude
fareData$Coordinates = fareData$Coord

library(splitstackshape)
fareData = cSplit(fareData, "Coordinates", ",")
names(fareData)[names(fareData) == 'Coordinates_1'] = 'Latitude'
names(fareData)[names(fareData) == 'Coordinates_2'] = 'Longitude'

##plot
library(ggmap)
library(mapproj)
library(dplyr)

#Used the boundary coordinates to bound NYC. Source: [2] and [3]
min_lat <- 40.57
max_lat <- 40.91
min_long <- -74.15
max_long <- -73.70

#Select and plot latitudes and longitudes where total number of trip > 500 and average fair > 300

#max trip count = 3267
#Corresponding entry =
#Coord                Total_amount Total_number_of_trips Average_fair Latitude Longitude
# 40.7213,-73.8443      51280.6         3627              14.13857  40.7213  -73.8443

#max average fair = 490.
#Corresponding entry = 
#  Coord             Total_amount Total_number_of_trips Average_fair Latitude Longitude
#1 40.8124,-73.9211          490           1             490         40.8124  -73.9211
##

#RESULT: As we can see trips with higher average fare have lower frequency and vice versa.
#Something that we infered earlier in question 2

##Based on above result, I plot the lat and long where total number of trips is greater than 100
##and average fair >=14
filterdData = filter(fareData, Average_fair >= 14.13 & Total_number_of_trips > 100)


#Code for heat maps
library(ggplot2)
library(ggmap)
nyc_map <- get_map(location = "New York City", maptype = "roadmap", zoom = 13)
ggmap(nyc_map, extent = "device") + geom_density2d(data = filterdData, 
  aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = filterdData, 
  aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
  bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

#Creating theme for plotting
library(ggplot2)
my_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      panel.background = element_rect(fill = "black", colour = "black", size = 0.5, linetype = "solid"),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

plot <- ggplot(trainData, aes(x=Dropoff_longitude, y=Dropoff_latitude)) +
  geom_point(color="white", size=0.06) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  my_theme()

png("nyc-taxi-3.png", w=600, h=600, res=100)
plot
dev.off()

#Map of NYC plotted using Total number of Trips > 10
filterdData.1 = filter(fareData, Total_number_of_trips > 10)
  plot <- ggplot(filterdData.1, aes(x=Longitude, y=Latitude)) +
    geom_point(size=0.03, color = "green") +
    scale_x_continuous(limits=c(min_long, max_long)) +
    scale_y_continuous(limits=c(min_lat, max_lat)) + 
    my_theme() +
    labs(title = "Map of NYC, Using Location of Total Number of Trips > 10") +
    theme(legend.position="none") +
    coord_equal()
    
  png("nyc-taxi4.png", w=600, h=600, res=100)
  plot
  dev.off()




## Merge with the original training Data to get results about this trip
sampledf = merge(trainData, filterdData, by="Coord")
table(sampledf$P)

#histogram of payment types in this area
hist(sampledf$Payment_type, main = "Popular Payment Types", xlab = "Payment Options", col = "green", border="black"
     ,las=1, prob=TRUE)

#histogram of most popular pickup hour
hist(as.numeric(sampledf$Hour.x), main = "Popular Pickup Hour", xlab = "Hours", col = "green", border="black"
     ,las=1, prob=TRUE)

filterdData$Latitude = round(filterdData$Latitude, digits = 3)
filterdData$Longitude = round(filterdData$Longitude, digits = 3)

  
group_loc =  data.frame(table(filterdData$Latitude, filterdData$Longitude))
popular_loc = filter(x, x$Freq>10)

barplot(popular_loc$Freq, main="10 Most Popular Spots", col = "green")
