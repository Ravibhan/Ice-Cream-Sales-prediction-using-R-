# Ice-Cream-Sales-prediction-using-R-
This repository studies and analysis the data of ice cream sales in the various cities of two country to find a way for boosting the sale of ice cream. The main focus of this project is to do in-depth analysis of each variable from the data which affect the sale of the ice cream. The data will be explored and analyzed in the R programming.
R script 
# import the sales data of ice cream of two countries and given the data file name : ice_cream #

ice_cream <- read.csv("https://raw.githubusercontent.com/sedaerdem/Statistics/master/data/icecream.csv")


# Loading  and installing the packages #

# Dplyr : for data insight # 
install.packages("dplyr")
library(dplyr)

# ggplot2 : for visualization of data #
install.packages("ggplot2")
library(ggplot2)

#check the structure  and view the ice_cream data #

str(ice_cream)

# Null value check of ice_cream data #

# checking null value for icecream sale # 

sum(is.na(ice_cream$icecream_sales))

# checking null value for income #

sum(is.na(ice_cream$income))

#checking null value for price # 

sum(is.na(ice_cream$price))

#checking null value for temperature # 

sum(is.na(ice_cream$temperature))

#checking null value for country #

sum(is.na(ice_cream$country))

#checking null value for seasons # 

sum(is.na(ice_cream$seasons))

# checking the class of each variable in the given data before investing and analyzing #
class(ice_cream$ShopID)
class(ice_cream$icecream_sales)
class(ice_cream$income)
class(ice_cream$price)
class(ice_cream$temperature)
class(ice_cream$country)
class(ice_cream$seasons)

# summary of all the variable  provided in the data set #

  summary(ice_cream)
  

# summarizing the variables of ice_cream data set # 

  # summary of descriptive statistics  of sales by country #
  ice_cream %>%
    group_by(country) %>%
    summarise(count=n(),
              mu = mean(icecream_sales),pop_med = median(icecream_sales),
              sigma = sd(icecream_sales), pop_iqr = IQR(icecream_sales),
              pop_min = min(icecream_sales), pop_max = max(icecream_sales),
              pop_q1 = quantile(icecream_sales, 0.25), #first quantile , 25th percentile #
              pop_q3 = quantile(icecream_sales, 0.75)) #third quantile , 75th percentile #

# summary of descriptive statistics of sales by seasons # 
  ice_cream %>%
    group_by(seasons) %>%
    summarise(count=n(),
              mu = mean(icecream_sales),pop_med = median(icecream_sales),
              sigma = sd(icecream_sales), pop_iqr = IQR(icecream_sales),
              pop_min = min(icecream_sales), pop_max = max(icecream_sales),
              pop_q1 = quantile(icecream_sales, 0.25), #first quantile , 25th percentile #
              pop_q3 = quantile(icecream_sales, 0.75)) #third quantile , 75th percentile #
  
  # Creating Histogram for the Ice cream Sales #
  hist(ice_cream$icecream_sales)
  
  # creating scatter plot of variables of Ice_cream data # 
  
  # Analyzing the sales : Density plot of sales
  # Compute the density data
  
  # plot density
  
 density(ice_cream$icecream_sales)
  plot(dens, frame = FALSE, col = "Maroon", main = "Desnity plot of Ice cream Sale")
  
  #box plot of sales per country
  ggplot(data =ice_cream,aes(x= country , y = icecream_sales , color = country)) + geom_boxplot() + xlab("Country") + ylab("Ice cream sales")  + ggtitle("Box plot of Ice cream Sale")
  
  #box plot of sales per season
  ggplot(data =ice_cream,aes(x= seasons , y = icecream_sales , color = seasons)) + geom_boxplot() + xlab("Season") + ylab("Ice cream sales")  + ggtitle("Box plot of Ice cream Sale")
  
  # Analyzing the Income : Density plot of Income  
  ggplot(ice_cream,aes(x=income)) + geom_density()+ xlab("Income") + ylab("Density") + ggtitle("Desnity plot of Income")
  
  #box plot of Income per country
  ggplot(data =ice_cream,aes(x= country , y = income , color = country)) + geom_boxplot() + xlab("Country") + ylab("income")  + ggtitle("Box plot of Income per Country")
  
  #box plot of Income per season
  ggplot(data =ice_cream,aes(x= seasons , y = income , color = seasons)) + geom_boxplot() + xlab("Season") + ylab("income")  + ggtitle("Box plot of Income per Season")

  #Analyzing the price : Density plot of Price  
  ggplot(ice_cream,aes(x=price)) + geom_density()+ xlab("Price") + ylab("Density") + ggtitle("Desnity plot of Price")
  
  #Box plot of price per country 
  ggplot(data =ice_cream,aes(x= country , y = price , color = country)) + geom_boxplot() + xlab("Country") + ylab("Price")  + ggtitle("Box plot of Price per Country")
  
  #analyzing  the Temperature :Box plot of temperature Country wise 
  ggplot(data =ice_cream,aes(x= country , y = temperature , color = country )) + geom_boxplot() + xlab("Country") + ylab("Tempreture")  + ggtitle("Box plot of Tempreture as per Country")
  
# Hypothesis Test #
  ggplot(data = na.omit(ice_cream),
  aes(x=country ,y=icecream_sales, color = country)) +  
    geom_boxplot()+xlab("country")+ 
    ylab("Ice cream sales")+ 
    ggtitle("Box plot for the sale of ice cream")+
    stat_summary(fun= mean , geom = "point" , shape = 2 , size = 4)
  
  # Histogram of ice cream sale of randomly selected 100 observation  # 
  
 Sales <- ice_cream$icecream_sales
 sample_1 <- sample(Sales, 100)
 mean(sample_1)
 glimpse(sample_1)
 hist(sample_1)
 
 #statsr :   for frequents inference and decision making 
 install.packages("statsr")
 library(statsr)
 
 inference(y=icecream_sales, x = country  , data = ice_cream,
 statistic = c("mean"),
 type = c("ht"),
 null = 0,
 alternative = c ("twoside"), method =c("theoretical"), conf_level = 0.96,
 order = c("A", "B"))
 
 
 # correlation matrix  by calculation of coefficient from the combination of numerical variable #
 
 
 #Computation of the correlation coefficient between pairs of our numerical variables, this returns the correlation matrix

ice_cream %>%
  select(icecream_sales, income ,price , temperature) %>%
  cor() %>%
  knitr::kable(digits = 3, caption = "Correlation between tempreature , trice , income and ice cream sale" )

    
  #Visualization of variables along with the combination of another variable#

Visual_1 <- ggplot(ice_cream, aes(x= income , y = icecream_sales)) +
    geom_point ()+
    labs(x = "Income (in £)", y = "Ice cream sales (in £)", title = "Relationship between ice cream sales and income") + 
    geom_smooth(method = "lm", se = FALSE)

Visual_2 <- ggplot(ice_cream, aes(x= price , y = icecream_sales)) +
  geom_point ()+
  labs(x = "price (in £)", y = "Ice cream sales (in £)", title = "Relationship between ice cream sales and price") + 
  geom_smooth(method = "lm", se = FALSE)


Visual_3 <- ggplot(ice_cream, aes(x= temperature , y = icecream_sales)) +
  geom_point ()+
  labs(x = "temperature (in celcius C)", y = "Ice cream sales (in £)", title = "Relationship between ice cream sales and temperature") + 
  geom_smooth(method = "lm", se = FALSE)
library(gridExtra)
grid.arrange(Visual_1,Visual_2,Visual_3)

# visual representation of scatter plot by forming the relationship between temperature and Ice cream sale ,country wise in highlighted in separate color # 
ggplot(data = ice_cream, aes(x= temperature , y = icecream_sales ,  color=country)) + geom_point ()+
  labs(x = "temperature (in celcius C)", y = "Ice cream sales (in £)", title = "Relationship between ice cream sales and country wise temperature ") + 
  geom_smooth(method = "lm", se = FALSE)

# visual representation of scatter plot by forming the relationship between income and Ice cream sale ,country wise in highlighted in separate color # 
ggplot(data = ice_cream, aes(x= income , y = icecream_sales ,  color=country)) + geom_point ()+
  labs(x = "income (in £ )", y = "Ice cream sales (in £)", title = "Relationship between ice cream sales and country wise income ") + 
  geom_smooth(method = "lm", se = FALSE)

# visual representation of scatter plot by forming the relationship between price and Ice cream sale ,country wise in highlighted in separate color # 
ggplot(data = ice_cream, aes(x= price , y = icecream_sales ,  color=country)) + geom_point ()+
  labs(x = "price (in £ )", y = "Ice cream sales (in £)", title = "Relationship between ice cream sales and country wise price ") + 
  geom_smooth(method = "lm", se = FALSE)

#multiple regression model # 
ice_cream <- read.csv("https://raw.githubusercontent.com/sedaerdem/Statistics/master/data/icecream.csv")
Sales_structure <- lm(icecream_sales ~ income + price + temperature + country + seasons, data = ice_cream)
summary(Sales_structure)
  
  #t-critical value # 
  qt(0.025, df=992)

# confidence interval at 90% 
confint(Sales_structure, level = 0.90)

#understanding the Model# 
country_a_20 <- data.frame(income = 20000, price = 3 , temperature = 20,country = "A", seasons = "Winter")
predict(Sales_structure, country_a_20 ,interval = "prediction",level = 0.95)
country_b_30 <- data.frame(income = 30000, price = 3 , temperature = 20,country = "B", seasons = "Winter")
predict(Sales_structure, country_b_30 ,interval = "prediction",level = 0.95)


country_icecream_temperature_1 <-data.frame(income = 30000, price = 3 , temperature = 20,country = "A", seasons = "Winter")
predict(Sales_structure, country_icecream_temperature_1 ,interval = "prediction",level = 0.95)
country_icecream_temperature_2 <-data.frame(income = 30000, price = 3.5 , temperature = 22,country = "A", seasons = "Winter")
predict(Sales_structure, country_icecream_temperature_2 ,interval = "prediction",level = 0.95)

# Condition Testing # 

# Linearity Testing # 

par(mfrow=c(1,2))
plot(Sales_structure$residuals~ice_cream$income)
plot(Sales_structure$residuals~ice_cream$price)
plot(Sales_structure$residuals~ice_cream$temperature)
  
#nearly normal distribution error terms # 
  hist(Sales_structure$residuals)
  qqnorm(Sales_structure$residuals)
  qqline(Sales_structure$residuals)
  
  #constant variability of residuals #
  plot(Sales_structure$residuals-Sales_structure$fitted)
  
  # Residuals as independent #
  plot(Sales_structure$residuals)

  # prediction # 
  Prediction_ice<- data.frame(income=30000, price = 3 , temperature = 23 , country = "A", seasons = "Spring")
  predict(Sales_structure,Prediction_ice, interval  = "prediction" , level = 0.95)

