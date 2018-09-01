library(sp)

GroceryLatLong <- Grocery %>% select(X,Y) %>%
  dplyr::rename(LONGITUDE=X,LATITUDE=Y)

GroceryLatLong$Grocery_ID <- rownames(GroceryLatLong)

MasterLatLong <- master_data %>% select(LATITUDE,LONGITUDE)

MasterLatLong$House_ID <- rownames(MasterLatLong)


temperaturePoints <- GroceryLatLong                                    
densityPoints <- MasterLatLong

# promote the input lists to SpatialPointsDataFrames

coordinates(temperaturePoints) <- c("LONGITUDE", "LATITUDE")
coordinates(densityPoints) <- c("LONGITUDE", "LATITUDE")             

# Remove temparature points with 'invalid' value of -199

validTempPoints <- temperaturePoints

#  Define these vectors, used in the loop.

closestSiteVec <- vector(mode = "numeric",length = nrow(densityPoints))
minDistVec     <- vector(mode = "numeric",length = nrow(densityPoints))

# Get the vector index of the temperature station closest to each field station.
# Use the spDistsN1 function to compute the distance vector between each
# field station site and all of the temperature stations. Then, find and
# retain the actual temperature, and the index of the closest temperature
# to each transect station.
#
# spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
#
# where:
#         pointList   : List of candidate points.
#         pointToMatch: Single point for which we seek the closest point in pointList.
#         longlat     : TRUE  computes Great Circle distance in km,
#                       FALSE computes Euclidean distance in units of input geographic coordinates
#
# We use Great Circle distance to increase distance calculation accuracy at high latitudes
# See the discussion of distance units in the header portion of this file
#
# minDistVec stores distance from the closest temperature station to each density measurement point.
# closestSiteVec stores the index of the closest temperature station to each density measurement point.
#
for (i in 1 : nrow(densityPoints))
{
  distVec <- spDistsN1(validTempPoints,densityPoints[i,],longlat = TRUE)
  minDistVec[i] <- min(distVec)
  closestSiteVec[i] <- which.min(distVec)
}
#
# Create the Temperature Assignment table: merge the temperature point list with the transect point list
# into a five-column table by merging the temperature point and transect point lists.
#
PointAssignTemps <- as(validTempPoints[closestSiteVec,]$Grocery_ID,"numeric")
FinalTable = data.frame(coordinates(densityPoints),densityPoints$House_ID,
                        closestSiteVec,minDistVec,PointAssignTemps) 

FinalTable <- FinalTable %>%select(densityPoints.House_ID,minDistVec,PointAssignTemps) %>%
  dplyr::rename(House_ID=densityPoints.House_ID,Grocery_ID=PointAssignTemps,GroceryDistnace=minDistVec)



###

master_data$ID <- rownames(master_data)

#Merge back to get 
master_data <- merge(master_data,FinalTable,by.x="ID",by.y="House_ID",all.x=T)

cor(master_data$GroceryDistnace,master_data$PRICE)