
master_data$COUNT_C <- 1

#how many places repeat?
AddressCount <- master_data %>%
  select(FULLADDRESS, COUNT_C) %>%
  group_by(FULLADDRESS) %>%
  summarise(Sum = sum(COUNT_C))

table(AddressCount$Sum)

#Census Block?
CensusCount <- master_data %>%
  select(ASSESSMENT_NBHD, COUNT_C) %>%
  group_by(ASSESSMENT_NBHD) %>%
  summarise(Sum = sum(COUNT_C))

table(CensusCount)

#Price by Year
 MedianPrice_ByYear<- master_data %>%
  select(SALEDATE, PRICE) %>%
  group_by(SALEDATE) %>%
  summarise(Median_Price = median(PRICE))
 
 #Price by Hood
 MedianPrice_ByHood <- master_data %>%
   select(ASSESSMENT_NBHD, PRICE) %>%
   group_by(ASSESSMENT_NBHD) %>%
   summarise(Median_Price = median(PRICE))
 
ggplot(data=MedianPrice_ByYear, aes(x=SALEDATE, y=Median_Price)) +
   geom_bar(stat="identity")
 
 #Price by Ward
 MedianPrice_ByWard <- master_data %>%
   select(WARD, PRICE) %>%
   group_by(WARD) %>%
   summarise(Median_Price = median(PRICE))
 
 p<-ggplot(data=MedianPrice_ByWard, aes(x=SALEDATE, y=Median_Price)) +
   geom_bar(stat="identity")
 
#Map Houses
 MaxLong <- max(master_data$LONGITUDE)
 MinLong <- min(master_data$LONGITUDE)
  MaxLat <- max(master_data$LATITUDE)
 MinLat <- min(master_data$LATITUDE)
 install.packages("rworldmap")
 library(rworldmap)
 newmap <- getMap(resolution = "low")
 plot(newmap, xlim = c(MinLat, MaxLat), ylim = c(MinLong, MaxLong), asp = 1)
 points(master_data$LONGITUDE, master_data$LATITUDE, col = "red", cex = .6)