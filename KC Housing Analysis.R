---
  title: "Housing price clusterrs and prediction"
author: "SM2R2 Consulting Group"
date: "31 December 2018"
output:
  word_document:
  keep_md: yes
---

library(tidyverse)
library(sqldf)
library(gridExtra) #for plotting
library(boot) #For diognastic plots
library(car) # for avplots
library(ggrepel) #For plotting
library(scales) #for changing decimals to be displayed as percentages
library(naniar) #For missing values plot
library(stringr) #For strings
library(timeDate)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(dbscan)
library(data.table)
library(zoo)
library(factoextra)
library(clue)
library(cluster)
library(tsne)
library(fpc)
library(ClustOfVar)
library(PCAmixdata)
library(klaR)
library(ggfortify)
library(maps)
library(ggplot2)
library(stringr)
library(DT)
library(leaflet)
library(corrplot)
library(psych)
library(randomForest)
library(hydroGOF)
library(e1071)
library(gbm)
library(caret)
library(VIM)
#housedf <- read.csv("C:/Users/774712616/Desktop/Data Course Semester 2/final lab/housedf.csv")
housedf <- read.csv("C:/Users/MKAlbini/Desktop/York Data Class/2 trimester/housedf.csv")
zipdemog <- read.csv("C:/Users/MKAlbini/Desktop/York Data Class/2 trimester/zipdemog.csv")
#zipdemog <- read.csv("C:/Users/774712616/Desktop/Data Course Semester 2/final lab/zipdemog.csv")


str(housedf)
colnames(housedf)

# Determine if there are any missing values
aggr_plot <- aggr(housedf, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# no missing values were detected


# fix date field
housedf$date<-as.Date(housedf$date, "%Y%m%dT000000")
str(housedf)

#fix formats
housedf$zipcode <- as.factor(housedf$zipcode)
housedf$waterfront <- as.factor(housedf$waterfront)

# make house with 33 bedrooms into 3. Was probably a typo.
housedf[15871,4] <- 3
summary(housedf)

# use max date from built and reno to make relavant date appear on one column.
#Assumption is that a renovated house has similar value to new house.

housedf <- transform(housedf, built_reno_date = pmax(yr_built, yr_renovated))

#remove extra columns no longer needed
housedf$id <- NULL

#calculate square foot values
housedf$price_per_living_sqft <- housedf$price / housedf$sqft_living
housedf$price_per_lot_sqft <- housedf$price / housedf$sqft_lot
housedf$price_per_above_sqft <- housedf$price / housedf$sqft_above
housedf$FinishedBasement <- ifelse(housedf$sqft_basement>0,"Yes","No")

# determine breaks for binning
library(rpart)
temp1 <- rpart(housedf$price~housedf$price_per_above_sqft)
plot(temp1)
text(temp1)

temp2 <- rpart(housedf$price_per_lot_sqft~housedf$sqft_lot)
plot(temp2)
text(temp2)

library(Hmisc)
housedf <- housedf%>%
  mutate (
    Price_Group = cut(housedf$price_per_above_sqft, breaks = c(0, 225, 370, 469, 627, 773, 5000) , labels = c("0-$225","$226-$370","$371-$469", "$470-$627","$628-$773", "Over $774")),
    Lot_Size_Group = cut2(housedf$sqft_lot,g=5),
    Sqft_above_Group = cut2(housedf$sqft_above,g=5),
    Build_Type_Group = cut(housedf$yr_built,breaks = c(0,1950,1975,2000,2015),labels = c("<1950","1950 to 1975","1975 to 2000","2000 to 2015"))
  )

housedf$GradeGroup <- as.factor(housedf$grade)
housedf$yr_built <- as.Date(housedf$yr_built)

housedf$Home_Age <- Sys.Date()- housedf$yr_built
housedf$Home_Age <- housedf$Home_Age / 365

str(housedf)
summary(housedf)

# Plotting for data analysis
ggplot(data = housedf) + stat_count(mapping = aes(x = Price_Group, fill = Build_Type_Group))

ggplot(data = housedf) + stat_summary(mapping = aes(x = Price_Group, y = price_per_above_sqft),
                                      fun.ymin = min,
                                      fun.ymax = max,
                                      fun.y = median)

ggplot(data = housedf) + stat_summary(mapping = aes(x = Sqft_above_Group, y = price_per_above_sqft),
                                      fun.ymin = min,
                                      fun.ymax = max,
                                      fun.y = median)

ggplot(data = housedf) +
  geom_bar(mapping = aes(x = Lot_Size_Group, fill = Price_Group))

ggplot(data = housedf) +
  geom_bar(mapping = aes(x = Lot_Size_Group, fill = GradeGroup), position = "dodge")

ggplot (data = housedf, mapping = aes(x = Sqft_above_Group, y = price_per_above_sqft)) + geom_boxplot() + labs(title = "Above Ground Price per Square Foot by Total Home Size") + theme_classic()
ggplot (data = housedf, mapping = aes(x = Lot_Size_Group, y = price_per_lot_sqft)) + geom_boxplot() + labs(title = "Square Foot Revenue by Lot Size") + theme_classic()


# Price averages with Zipcodes
housedf %>%
  group_by(zipcode) %>%
  summarise(count = n(), zip_ave = mean(price)) %>%
  mutate(zip_diff = zip_ave - mean(zip_ave)) %>%
  mutate(zip_difference = ifelse(zip_diff > 0, 'Below Average' , 'Above Average')) %>%
  arrange((zip_diff)) %>%
  #filter(count > 20) %>%
  ggplot(aes( x = factor(zipcode, levels =zipcode), y = zip_diff)) + 
  geom_bar(stat = 'identity', aes(fill=zip_difference), width = .6) + 
  coord_flip() + 
  labs(x = 'Zip Code', y = 'Percentage Point Difference', title = 'Price Averages per Zip Code') + 
  theme_bw()

# Break up the zip codes into tiers for models
housedf$tier1_zip <-ifelse(housedf$zipcode %in% c(98039),1,0)
housedf$tier2_zip <-ifelse(housedf$zipcode %in% c(98004,98040,98112),1,0)
housedf$tier3_zip <-ifelse(housedf$zipcode %in% c(98102,98109,98105,98006,98119, 98005, 98033, 98199, 98075),1,0)
housedf$tier4_zip <-ifelse(housedf$zipcode %in% c(98074,98077,98053,9817798008,98052,98122,98115,98116,98007,98027,98029, 98144, 98103,98024,98107,98117,98072),1,0)
housedf$tier5_zip <-ifelse(housedf$zipcode %in% c(98136,98065,98034,98059,98011,98070,98125,98166,98028,98014,98045,98019,98126,98155,98010,98056,98118,98133),1,0)
housedf$tier6_zip <-ifelse(housedf$zipcode %in% c(98038,98146,98108,98058,98092,98106,98022,98042,98178),1,0)
housedf$tier7_zip <- 1 - housedf$tier1_zip - housedf$tier2_zip - housedf$tier3_zip - housedf$tier4_zip - housedf$tier5_zip - housedf$tier6_zip

str(housedf)
# Explore Numeric columns

housedf %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free", ncol = 4) +
  geom_density(fill= 'lightblue') +
  theme_bw()

# Explore non-numeric columns

housedf %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar(fill = 'blue') + 
  theme_bw()

# get numeric values
housedf_num <- housedf[,sapply(housedf, is.numeric)]

str(housedf_num)
#scale the variables
scaled_housedf_num <- as.data.frame(scale(housedf_num))
str(scaled_housedf_num)

d <- dist(scaled_housedf_num,method = "euclidean") #distance matrix
h_clust <- hclust(d, method = "ward.D") #clustering
plot(h_clust) #dendrogram

rect.hclust(h_clust,k=5)

# Determine number of clusters
set.seed(123)
wss <- (nrow(scaled_housedf_num)-1)*sum(apply(scaled_housedf_num,2,var))
for (i in 1:ncol(scaled_housedf_num)) wss[i] <- sum(kmeans(scaled_housedf_num, 
                                                           centers=i)$withinss)
plot(1:ncol(scaled_housedf_num), wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



##### Checking correlation between all variables
CorrelationResults = cor(housedf_num)

corrplot(CorrelationResults)

housedf_num %>%
  gather(key,value,-price) %>%
  ggplot(aes(x=value,y=price)) +
  geom_jitter(color = 'light blue',alpha = .6) +
  geom_smooth(method = 'gam', color= 'dark blue', fill = 'grey', alpha = .2) +
  facet_wrap(~key, scales = 'free') + 
  theme_bw()

# K-Means Cluster Analysis
set.seed(123)
fit <- kmeans(scaled_housedf_num, 5) # 5 cluster solution. 8 or 13 better?
# get cluster means 
clust_means <- round(aggregate(scaled_housedf_num,by=list(fit$cluster),FUN=mean),2)
# append cluster assignment
housedf_num1<- data.frame(scaled_housedf_num, fit$cluster)

fviz_cluster(fit, data <- scaled_housedf_num)

str(clust_means)

housedf_clus <- cbind(housedf, fit$cluster)
colnames(housedf_clus)[35] <- "cluster"

cluster_means_summary <- round(t(housedf_clus %>%
  keep(is.numeric)%>%
  group_by(cluster) %>%
  summarise_all(mean)),3)
  
cluster_means_summary

# The clusters seem to indicate that the location and zipcode tiers play an important factor in the clustering.
#lets get more info specific to the zip code tiers to use for our marketing strategy for each zone.

## Break up the zip codes into tiers for demographics comparison
housedf2 <- read.csv("C:/Users/MKAlbini/Desktop/York Data Class/2 trimester/housedf.csv")

# fix date field
housedf2$date<-as.Date(housedf$date, "%Y%m%dT000000")
str(housedf)

#fix formats
housedf2$zipcode <- as.factor(housedf$zipcode)
housedf2$waterfront <- as.factor(housedf$waterfront)

# make house with 33 bedrooms into 3. Was probably a typo.
housedf2[15871,4] <- 3
summary(housedf2)

# use max date from built and reno to make relavant date appear on one column.
#Assumption is that a renovated house has similar value to new house.

housedf2 <- transform(housedf2, built_reno_date = pmax(yr_built, yr_renovated))

#remove extra columns no longer needed
housedf2$id <- NULL

housedf2$zip_tier <- ifelse(housedf2$zipcode %in% c(98039), "Tier1",
                            ifelse(housedf2$zipcode %in% c(98004,98040,98112),"Tier2",
                                   ifelse(housedf2$zipcode %in% c(98102,98109,98105,98006,98119, 98005, 98033, 98199, 98075),"Tier3",
                                          ifelse(housedf2$zipcode %in% c(98074,98077,98053,98177,98008,98052,98122,98115,98116,98007,98027,98029, 98144, 98103,98024,98107,98117,98072),"Tier4",
                                                 ifelse(housedf2$zipcode %in% c(98136,98065,98034,98059,98011,98070,98125,98166,98028,98014,98045,98019,98126,98155,98010,98056,98118,98133),"Tier5",
                                                        ifelse(housedf2$zipcode %in% c(98038,98146,98108,98058,98092,98106,98022,98042,98178), "Tier6","Tier7"))))))

housedf2 <- as.data.frame(aggregate(housedf2[,2:21], list(housedf2$zip_tier), mean))
housedf2 <- as.data.frame(t(housedf2))
housedf2

# Break up zipcode demographicsfile zipcodes into tiers to see how the price tiers allign to the demographics for the area.
housedf_zips <- as.data.frame(unique(housedf[["zipcode"]]))
colnames(housedf_zips)[1] <- "zipcode"
zipdemog$zipcode <- as.factor(zipdemog$zipcode)
str(zipdemog)
str(housedf_zips)

zipdemog_housedf <- merge(housedf_zips, zipdemog, by.x = "zipcode", by.y = "zipcode")

zipdemog_housedf <- zipdemog_housedf[c(1,4,8,13,14,15,21)] #just took the ones I thought would help for now.

zipdemog_housedf$zip_tier <- ifelse(zipdemog_housedf$zipcode %in% c(98039), "Tier1",
                                    ifelse(zipdemog_housedf$zipcode %in% c(98004,98040,98112),"Tier2",
                                           ifelse(zipdemog_housedf$zipcode %in% c(98102,98109,98105,98006,98119, 98005, 98033, 98199, 98075),"Tier3",
                                                  ifelse(zipdemog_housedf$zipcode %in% c(98074,98077,98053,98177,98008,98052,98122,98115,98116,98007,98027,98029, 98144, 98103,98024,98107,98117,98072),"Tier4",
                                                         ifelse(zipdemog_housedf$zipcode %in% c(98136,98065,98034,98059,98011,98070,98125,98166,98028,98014,98045,98019,98126,98155,98010,98056,98118,98133),"Tier5",
                                                                ifelse(zipdemog_housedf$zipcode %in% c(98038,98146,98108,98058,98092,98106,98022,98042,98178), "Tier6","Tier7"))))))

zip_tiers_means <- as.data.frame(aggregate(zipdemog_housedf[, 2:7], list(zipdemog_housedf$zip_tier), mean))
zip_tiers_means <- as.data.frame(t(zip_tiers_means))

#Use the Compare_housedf table to gain more insights to the zip tiers created to use with clustering data
compare_housedf <- rbind(zip_tiers_means,housedf2)
compare_housedf <- compare_housedf[-c(8,15,23),]
compare_housedf

#PCA 
housedf_clus_num <- housedf_clus[,sapply(housedf_clus, is.numeric)]

prin_comp <- prcomp(housedf_clus_num[2:28], scale = TRUE)
names(prin_comp)
prin_comp$centre
prin_comp$scale
prin_comp$rotation
prin_comp$sdev

dim(prin_comp$x)

prin_comp$sdev^2 / sum(prin_comp$sdev^2)
plot(prin_comp)

par(mfrow=c(2,2))

plot(prin_comp$x[,2], housedf_clus_num$price)
plot(prin_comp$x[,3], housedf_clus_num$price)
plot(prin_comp$x[,4], housedf_clus_num$price)
plot(prin_comp$x[,5], housedf_clus_num$price)
plot(prin_comp$x[,6], housedf_clus_num$price)
plot(prin_comp$x[,7], housedf_clus_num$price)

dim(prin_comp$x)

biplot(prin_comp, scale = 0)

summary(prin_comp)

std_dev <- prin_comp$sdev
pr_var <- std_dev^2
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:15]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",type = "b")

#add a data set with principal components
housedf_princomp <- data.frame(price = housedf_clus_num$price, prin_comp$x)

#we are interested in first 10 PCAs?
housedf_princomp1 <- housedf_princomp[,1:10] 

housedf_princomp_dt = sort(sample(nrow(housedf_princomp1), nrow(housedf_princomp1)*.7))
train<-housedf_princomp1[housedf_princomp_dt,]
test<-housedf_princomp1[-housedf_princomp_dt,]

# define training control
train_control <- trainControl(method="cv", number=5)
# train the model
model <- train(price~., data=train,trControl=train_control, method="xgbLinear")
# summarize results
print(model)
mean(model$results$Rsquared)
mean(model$results$RsquaredSD)
mean(model$results$RMSE)
summary(model)

pricepredicted = predict(model, test)
test_pred <- cbind(test, pricepredicted)
rmse(test_pred$price, test_pred$pricepredicted)
R2(test_pred$price, test_pred$pricepredicted, formula = "corr")

#Lets see if RandomForest is better.
model2 <- train(price~., data=train,trControl=train_control, method="rf")
# summarize results
print(model2)
mean(model2$results$Rsquared)
mean(model2$results$RsquaredSD)
mean(model2$results$RMSE)
summary(model2)

pricepredicted2 = predict(model2, test)
test_pred <- cbind(test, pricepredicted2)
rmse(test_pred$price, test_pred$pricepredicted2)
R2(test_pred$price, test_pred$pricepredicted2, formula = "corr")

# Let's see if GENERALIZED LINEAR REGRESSION is better
set.seed(123)
# define training control & train model
train_control3 <- trainControl(method="cv", number=5)
model3 <- train(price~., data=train,trControl=train_control, method="glm")
# summarize results
print(model3)

pricepredicted3 = predict(model3, test)
test_pred <- cbind(test, pricepredicted3)
rmse(test_pred$price, test_pred$pricepredicted3)
R2(test_pred$price, test_pred$pricepredicted3, formula = "corr")

# train the model by using XGB Tree
model4 <- train(price~., data=train,trControl=train_control, method="xgbTree")
# summarize results
print(model4)
mean(model4$results$Rsquared)
mean(model4$results$RsquaredSD)
mean(model3$results$RMSE)
summary(model4)

pricepredicted4 = predict(model4, test)
test_pred <- cbind(test, pricepredicted4)
rmse(test_pred$price, test_pred$pricepredicted4)
R2(test_pred$price, test_pred$pricepredicted4, formula = "corr")

# XGBLinear is the more accurate for prediction.

#################################################