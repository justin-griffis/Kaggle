library(tidyverse) # metapackage with lots of helpful functions
library(ggplot2)
library(tibble)
library(tidyr)
library(readr)
#install.packages("rgeos")
library(rgeos)

setwd("/Users/justingriffis/Downloads/dc-residential-properties")

#list.files(path = "../input")

#Read in properties data
df <- read.csv("DC_Properties.csv")

#Remove where Price is missing
is.na(df$PRICE) <- df$PRICE < 2000
unknown <- df[is.na(df$PRICE),]

master_data <- df[complete.cases(df$PRICE), ]

# Dropping repeated columns
master_data <- subset(master_data, select=-c(X.1,STYLE,X,Y,GIS_LAST_MOD_DTTM))
# head(master_data)

## Checking for validity of data
is.na(master_data$BATHRM) <- master_data$BATHRM + master_data$HF_BATHRM == 0
is.na(master_data$HF_BATHRM) <- master_data$BATHRM + master_data$HF_BATHRM == 0

master_data$HEAT <- as.character(master_data$HEAT)
is.na(master_data$HEAT) <- master_data$HEAT == 'No Data'

master_data$AC <- as.character(master_data$AC)
is.na(master_data$AC) <- master_data$AC == '0'

is.na(master_data$ROOMS) <- master_data$ROOMS == 0

is.na(master_data$AYB) <- master_data$AYB <= 0
is.na(master_data$EYB) <- master_data$EYB <= 0

master_data$STORIES <- floor(master_data$STORIES)
is.na(master_data$STORIES) <- master_data$STORIES > 26

master_data$SALEDATE <- as.character(master_data$SALEDATE)
master_data$SALEDATE <- substr(master_data$SALEDATE,1,4)
master_data$SALEDATE <- as.numeric(master_data$SALEDATE)

is.na(master_data$GBA) <- master_data$GBA <= 0

master_data$STRUCT <- as.character(master_data$STRUCT)
is.na(master_data$STRUCT) <- master_data$STRUCT == ''

master_data$GRADE <- as.character(master_data$GRADE)
is.na(master_data$GRADE) <- master_data$GRADE == 'No Data'
is.na(master_data$GRADE) <- master_data$GRADE == ''

master_data$CNDTN <- as.character(master_data$CNDTN)
is.na(master_data$CNDTN) <- master_data$CNDTN == ''

master_data$EXTWALL <- as.character(master_data$EXTWALL)
is.na(master_data$EXTWALL) <- master_data$EXTWALL == ''

master_data$ROOF <- as.character(master_data$ROOF)
is.na(master_data$ROOF) <- master_data$ROOFL == ''

master_data$INTWALL <- as.character(master_data$INTWALL)
is.na(master_data$INTWAL) <- master_data$INTWALL == ''

master_data$KITCHENS[master_data$KITCHENS == 44] = 4

is.na(master_data$FIREPLACES) <- master_data$FIREPLACES > 15
is.na(master_data$LANDAREA) <- master_data$LANDAREA <= 0
is.na(master_data$LIVING_GBA) <- master_data$LIVING_GBA <= 0
is.na(master_data$BEDRM) <- master_data$BEDRM == 0

master_data$ASSESSMENT_NBHD <- as.character(master_data$ASSESSMENT_NBHD)
is.na(master_data$ASSESSMENT_NBHD) <- master_data$ASSESSMENT_NBHD == ''

master_data$WARD <- as.character(master_data$WARD)
is.na(master_data$WARD) <- master_data$WARD == ''

master_data$QUADRANT <- as.character(master_data$QUADRANT)
is.na(master_data$QUADRANT) <- master_data$QUADRANT == ''

master_data$AGE_AT_SALE <- master_data$SALEDATE-master_data$AYB

master_data$HEAT <- as.factor(master_data$HEAT)
master_data$AC <- as.factor(master_data$AC)
master_data$STRUCT <- as.factor(master_data$STRUCT)
master_data$GRADE <- as.factor(master_data$GRADE)
master_data$CNDTN <- as.factor(master_data$CNDTN)
master_data$EXTWALL <- as.factor(master_data$EXTWALL)
master_data$ROOF <- as.factor(master_data$ROOF)
master_data$INTWALL <- as.factor(master_data$INTWALL)
master_data$SOURCE <- as.factor(master_data$SOURCE)
master_data$ZIPCODE <- as.factor(master_data$ZIPCODE)
master_data$ASSESSMENT_NBHD <- as.factor(master_data$ASSESSMENT_NBHD)
master_data$WARD <- as.factor(master_data$WARD)
master_data$QUADRANT <- as.factor(master_data$QUADRANT)


## Computing missing values for Independent varibales
x=subset(master_data, select = -PRICE)
y=master_data$PRICE

## Splitting data based on column data type
x_num=data.frame(x[sapply(x, function(number) is.integer(number)|| is.numeric(number))])
x_cat=data.frame(x[sapply(x, function(cat) is.factor(cat))])

## Command to exclude columns based on number of missing values
xnum_na_count <-data.frame(sapply(x_num, function(na) sum(length(which(is.na(na))))))

## Replacing missing values of continous independent variables with median
f=function(x_num)
{
  x_num[is.na(x_num)] =median(x_num, na.rm=TRUE) #convert the item with NA to median value from the column
  x_num #display the column
}
x_fillnum=data.frame(apply(x_num,2,f))
x_fillnum_count <-data.frame(sapply(x_fillnum, function(na) sum(length(which(is.na(na))))))
x_fillnum_count

## Replacing missing values of categorical independent variables with mode
helperFunc <- function(x_cat){
  sample(levels(x_cat), sum(is.na(x_cat)), replace = TRUE,
         prob = as.numeric(table(x_cat))/sum(!is.na(x_cat)))   
}

x_cat[sapply(x_cat, is.na)]  <- unlist(sapply(x_cat, helperFunc))
x_cat_count <-data.frame(sapply(x_cat, function(na) sum(length(which(is.na(na))))))

## Final data
master_data=cbind(master_data$PRICE,x_fillnum,x_cat) 
colnames(master_data)[1] = "PRICE"

##Bring in Grocery Store data
Grocery <- read.csv("Grocery_Store_Locations.csv")

#Create ID
Grocery$ID <- rownames(Grocery)

Grocery <- Grocery %>% select(X,Y,ID)

set1 <- structure(list(

set1sp <- SpatialPoints(set1)
set2sp <- SpatialPoints(set2)
set1$nearest_in_set2 <- apply(gDistance(set1sp, set2sp, byid=TRUE), 1, which.min)


#Initial Regressions
data_reg=master_data
reg_model1=lm(formula = PRICE~BATHRM+HF_BATHRM+HEAT+
                AC+ROOMS+BEDRM+
                QUALIFIED+SALE_NUM+
                FIREPLACES+LANDAREA+
                SOURCE+ZIPCODE+ASSESSMENT_NBHD+
                WARD+QUADRANT+AGE_AT_SALE,data=data_reg)
summary(reg_model1)

#Change to log
reg_model_log_10=lm(formula = log10(PRICE)~BATHRM+HF_BATHRM+HEAT+
                      AC+ROOMS+BEDRM+
                      QUALIFIED+SALE_NUM+
                      FIREPLACES+LANDAREA+
                      SOURCE+ZIPCODE+ASSESSMENT_NBHD+
                      WARD+QUADRANT+AGE_AT_SALE,data=data_reg)
summary(reg_model_log_10)

#Stepwise AIC Var Selection
step <- stepAIC(reg_model_log_10, direction="both")
step$anova # display results

##Remove Outliers to map

quantile(master_data$PRICE, probs=.995)

Smaller <- master_data[which(master_data$PRICE<=6432125),]
ggplot(Smaller, aes(x=PRICE)) + geom_density()


mean.sp <- mean(Smaller$PRICE)
sd.sp <- sd(Smaller$PRICE)
max.sp <- max(Smaller$PRICE)
min.sp <- min(Smaller$PRICE)

Smaller$Count <- 1

Count <- Smaller  %>%
  select(PRICE,Count) %>%
  group_by(PRICE) %>%
  summarize(Count = sum(Count))

#OVerlay normal distrubtion compared to actual distribution

 ggplot(data = data.frame(x = c(min.sp, max.sp)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mean.sp, sd = sd.sp)) + ylab("") +
  scale_y_continuous(breaks = NULL) 


 ggplot(Smaller, aes(x=PRICE)) + geom_density() 

