#################################################################
##############Vehicle Fuel Economy Data Cleaning#################
#################################################################

library(dplyr)
library(olsrr)
 
vehicles_fuel_economy_Edited_for_EVs_8_ <- filter(vehicles_fuel_economy_Edited_for_EVs_8_, fuelType == "Electricity")

vehicles_fuel_economy_Edited_for_EVs_8_ <- vehicles_fuel_economy_Edited_for_EVs_8_[,-10]

vehicles_fuel_economy_Edited_for_EVs_8_$drive <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$drive)
vehicles_fuel_economy_Edited_for_EVs_8_$make <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$make)
vehicles_fuel_economy_Edited_for_EVs_8_$model <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$model)
vehicles_fuel_economy_Edited_for_EVs_8_$`Acceleration 0-60mph (s)` <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$`Acceleration 0-60mph (s)`)
vehicles_fuel_economy_Edited_for_EVs_8_$`Acceleration 0-100 km/h (s)` <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$`Acceleration 0-100 km/h (s)`)
vehicles_fuel_economy_Edited_for_EVs_8_$mpgData <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$mpgData)
vehicles_fuel_economy_Edited_for_EVs_8_$trany <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$trany)
vehicles_fuel_economy_Edited_for_EVs_8_$VClass <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$VClass)
vehicles_fuel_economy_Edited_for_EVs_8_$evMotor <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$evMotor)
vehicles_fuel_economy_Edited_for_EVs_8_$mfrCode <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$mfrCode)
vehicles_fuel_economy_Edited_for_EVs_8_$c240Dscr <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$c240Dscr)
vehicles_fuel_economy_Edited_for_EVs_8_$c240bDscr <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$c240bDscr)
vehicles_fuel_economy_Edited_for_EVs_8_$createdOn <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$createdOn)
vehicles_fuel_economy_Edited_for_EVs_8_$modifiedOn <- as.factor(vehicles_fuel_economy_Edited_for_EVs_8_$modifiedOn)

vehicles_fuel_economy_Edited_for_EVs_8_$drive <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$drive)
vehicles_fuel_economy_Edited_for_EVs_8_$make <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$make)
vehicles_fuel_economy_Edited_for_EVs_8_$model <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$model)
vehicles_fuel_economy_Edited_for_EVs_8_$`Acceleration 0-60mph (s)` <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$`Acceleration 0-60mph (s)`)
vehicles_fuel_economy_Edited_for_EVs_8_$`Acceleration 0-100 km/h (s)` <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$`Acceleration 0-100 km/h (s)`)
vehicles_fuel_economy_Edited_for_EVs_8_$mpgData <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$mpgData)
vehicles_fuel_economy_Edited_for_EVs_8_$trany <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$trany)
vehicles_fuel_economy_Edited_for_EVs_8_$VClass <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$VClass)
vehicles_fuel_economy_Edited_for_EVs_8_$evMotor <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$evMotor)
vehicles_fuel_economy_Edited_for_EVs_8_$mfrCode <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$mfrCode)
vehicles_fuel_economy_Edited_for_EVs_8_$c240Dscr <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$c240Dscr)
vehicles_fuel_economy_Edited_for_EVs_8_$c240bDscr <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$c240bDscr)
vehicles_fuel_economy_Edited_for_EVs_8_$createdOn <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$createdOn)
vehicles_fuel_economy_Edited_for_EVs_8_$modifiedOn <- as.numeric(vehicles_fuel_economy_Edited_for_EVs_8_$modifiedOn)


vehicles_fuel_economy_Edited_for_EVs_8_ <- na.omit(vehicles_fuel_economy_Edited_for_EVs_8_)

maxss <- apply(vehicles_fuel_economy_Edited_for_EVs_8_, 2, max)
minss <- apply(vehicles_fuel_economy_Edited_for_EVs_8_, 2, min)
vehicles_fuel_economy_Edited_for_EVs_8_n <- as.data.frame(scale(vehicles_fuel_economy_Edited_for_EVs_8_, center = minss, scale = maxss - minss))

#################################################
#################################################
############Correlation Matrix###################
#################################################
plot(vehicles_fuel_economy_Edited_for_EVs_8_n[,1:14])
plot(vehicles_fuel_economy_Edited_for_EVs_8_n[,15:28])
plot(vehicles_fuel_economy_Edited_for_EVs_8_n[,29:44])


barrels08 <- lm(PriceMSRP ~ barrels08, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(barrels08)
plot(PriceMSRP ~ barrels08, data = vehicles_fuel_economy_Edited_for_EVs_8_n)

city08 <- lm(PriceMSRP ~ city08, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(city08)

cityE <- lm(PriceMSRP ~ cityE, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(cityE)

comb08 <- lm(PriceMSRP ~ comb08, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(comb08)

combE <- lm(PriceMSRP ~ combE, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(combE)

fuelCost08 <- lm(PriceMSRP ~ fuelCost08, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(fuelCost08)

highway08 <- lm(PriceMSRP ~ highway08, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(highway08)

highwayE <- lm(PriceMSRP ~ highwayE, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(highwayE)


#city08 has strongest correlation with Price MSPR#

dataset2 <- vehicles_fuel_economy_Edited_for_EVs_8_n[c(-1,-4,-5,-6,-9,-10,-11)]

#2nd Correlation Matrix run#

Acceleration_m <- lm(PriceMSRP ~ vehicles_fuel_economy_Edited_for_EVs_8_n$`Acceleration 0-60mph (s)`, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(Acceleration_m)

Acceleration_k <- lm(PriceMSRP ~ vehicles_fuel_economy_Edited_for_EVs_8_n$`Acceleration 0-100 km/h (s)`, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(Acceleration_k)

dataset3 <- vehicles_fuel_economy_Edited_for_EVs_8_n[c(-1,-4,-5,-6,-9,-10,-11,-21)]

#3rd Correlation Matrix run#

range <- lm(PriceMSRP ~ range, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(range)

rangeCity <- lm(PriceMSRP ~ rangeCity, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(rangeCity)

rangeHwy <- lm(PriceMSRP ~ rangeHwy, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(rangeHwy)

dataset4 <- vehicles_fuel_economy_Edited_for_EVs_8_n[c(-1,-4,-5,-6,-9,-10,-11,-21,-30,-31)]

#4th Correlation Matrix run#

UCity <- lm(PriceMSRP ~ UCity, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(UCity)

UHighway <- lm(PriceMSRP ~ UHighway, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(UHighway)

youSaveSpend <- lm(PriceMSRP ~ youSaveSpend, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(youSaveSpend)

dataset5 <- vehicles_fuel_economy_Edited_for_EVs_8_n[c(-1,-4,-5,-6,-9,-10,-11,-21,-30,-31,-35,-37)]

#5th Correlation Matrix run#

plot(dataset5[,1:16])
plot(dataset5[,17:32])
plot(dataset5[c(1,2,3,4,5,11,12,13,14,15,16,17,18,19,20,21)])
plot(dataset5[c(1,2,3,4,5,22,23,24,25,26,27,28,29,30,31,32)])
plot(dataset5[c(6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)])
plot(dataset5[c(6,7,8,9,10,22,23,24,25,26,27,28,29,30,31,32)])
plot(dataset5[c(11,12,13,14,15,16,17,18,19,20,21,22,23,24)])
plot(dataset5[c(11,12,13,14,15,16,25,26,27,28,29,30,31,32)])

id <- lm(PriceMSRP ~ id, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(id)

year <- lm(PriceMSRP ~ year, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(year)

dataset6 <- vehicles_fuel_economy_Edited_for_EVs_8_n[c(-1,-4,-5,-6,-9,-10,-11,-21,-30,-31,-35,-37,-18)]

#6th Correlation Matrix run#

city082 <- lm(PriceMSRP ~ city08, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(city082)

UCity <- lm(PriceMSRP ~ UCity, data = vehicles_fuel_economy_Edited_for_EVs_8_n)
summary(UCity)

dataset7 <- vehicles_fuel_economy_Edited_for_EVs_8_n[c(-1,-4,-5,-6,-9,-10,-11,-21,-30,-31,-35,-37,-18,-3)]


################################################################
#################Outlines in Price##############################
################################################################
##########Remove Outliers##########


data <- vehicles_fuel_economy_Edited_for_EVs_8_ 

boxplot(data)
iqr <- IQR(data$PriceMSRP)
high <- quantile(data$PriceMSRP, 0.75) + 1.5*iqr
low <- quantile(data$PriceMSRP, 0.25) -1.5*iqr
pricedata4 <- subset(pricedata4, pricedata4$MSRP > low & pricedata4$MSRP < high) 


####Linear Relationship to Y#####

plot(data$charge240, data$PriceMSRP)
abline(data$charge240, data$PriceMSRP)
charge240lm <- lm(PriceMSRP ~ charge240, data = data)
summary(charge240lm)

plot(data$drive, data$PriceMSRP)
abline(data$drive, data$PriceMSRP)
drivelm <- lm(PriceMSRP ~ drive, data = data)
summary(drivelm)

plot(data$engId, data$PriceMSRP)
abline(data$engId, data$PriceMSRP)
engIdlm <- lm(PriceMSRP ~ engId, data = data)
summary(engIdlm)

plot(data$hlv, data$PriceMSRP)
abline(data$hlv, data$PriceMSRP)
hlvlm <- lm(PriceMSRP ~ hlv, data = data)
summary(hlvlm)

plot(data$hpv, data$PriceMSRP)
abline(data$hpv, data$PriceMSRP)
hpvlm <- lm(PriceMSRP ~ hpv, data = data)
summary(hpvlm)

plot(data$id, data$PriceMSRP)
abline(data$id, data$PriceMSRP)
idlm <- lm(PriceMSRP ~ id, data = data)
summary(idlm)

plot(data$lv4, data$PriceMSRP)
abline(data$lv4, data$PriceMSRP)
lv4lm <- lm(PriceMSRP ~ lv4, data = data)
summary(lv4lm)

plot(data$make, data$PriceMSRP)
abline(data$make, data$PriceMSRP)
makelm <- lm(PriceMSRP ~ make, data = data)
summary(makelm)

plot(data$model, data$PriceMSRP)
abline(data$model, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ model, data = data)
summary(modellm)

plot(data$`Top Speed (mph)`, data$PriceMSRP)
abline(data$`Top Speed (mph)`, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ `Top Speed (mph)`, data = data)
summary(modellm)

plot(data$`Acceleration 0-60mph (s)`, data$PriceMSRP)
abline(data$`Acceleration 0-60mph (s)`, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ `Acceleration 0-60mph (s)`, data = data)
summary(modellm)

plot(data$`Battery Capacity kWh`, data$PriceMSRP)
abline(data$`Battery Capacity kWh`, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ `Battery Capacity kWh`, data = data)
summary(modellm)

plot(data$Seats, data$PriceMSRP)
abline(data$Seats, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ Seats, data = data)
summary(modellm)

plot(data$`Trunk Volume (cuft)`, data$PriceMSRP)
abline(data$`Trunk Volume (cuft)`, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ `Trunk Volume (cuft)`, data = data)
summary(modellm)

plot(data$`Power (HP)`, data$PriceMSRP)
abline(data$`Power (HP)`, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ `Power (HP)`, data = data)
summary(modellm)

plot(data$mpgData, data$PriceMSRP)
abline(data$mpgData, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ mpgData, data = data)
summary(modellm)

plot(data$pv4, data$PriceMSRP)
abline(data$pv4, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ pv4, data = data)
summary(modellm)

plot(data$rangeHwy, data$PriceMSRP)
abline(data$rangeHwy, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ rangeHwy, data = data)
summary(modellm)
        
plot(data$trany, data$PriceMSRP)
abline(data$trany, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ trany, data = data)
summary(modellm)     
        
plot(data$UCity, data$PriceMSRP)
abline(data$UCity, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ UCity, data = data)
summary(modellm)    

plot(data$VClass, data$PriceMSRP)
abline(data$VClass, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ VClass, data = data)
summary(modellm)

plot(data$evMotor, data$PriceMSRP)
abline(data$evMotor, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ evMotor, data = data)
summary(modellm)

plot(data$mfrCode, data$PriceMSRP)
abline(data$mfrCode, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ mfrCode, data = data)
summary(modellm)

plot(data$c240Dscr, data$PriceMSRP)
abline(data$c240Dscr, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ c240Dscr, data = data)
summary(modellm)

plot(data$charge240b, data$PriceMSRP)
abline(data$charge240b, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ charge240b, data = data)
summary(modellm)

plot(data$c240bDscr, data$PriceMSRP)
abline(data$c240bDscr, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ c240bDscr, data = data)
summary(modellm)

plot(data$createdOn, data$PriceMSRP)
abline(data$createdOn, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ createdOn, data = data)
summary(modellm)

plot(data$modifiedOn, data$PriceMSRP)
abline(data$modifiedOn, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ modifiedOn, data = data)
summary(modellm)

plot(data$`# of Doors`, data$PriceMSRP)
abline(data$`# of Doors`, data$PriceMSRP)
modellm <- lm(PriceMSRP ~ `# of Doors`, data = data)
summary(modellm)

plot(data$year, data$PriceMSRP)

###################################Final set of attributes###########################################################

data2 <- data[c(2, 20, 22, 23, 27, 32, 34)]
data2n <- vehicles_fuel_economy_Edited_for_EVs_8_n[c(2, 20, 22, 23, 27, 32, 34)]

#########################################
##########For Different Clusters#########
#########################################
library(dendextend)
library(factoextra)
library(tidyverse)
library(cluster)

####################################################################
########################Hierarchical Clustering w/o train/test data#####################
####################################################################

library(factoextra)

price_EUC <- dist(data2, method = "euclidean")
price_HC <- hclust(price_EUC, method = "ward.D2")
plot(price_HC, cex = 0.3)
fviz_dend(price_HC, k = 3)

cluster_groups <- cutree(price_HC, k = 3)

#Cluster 1

Cluster_1 <- subset(data2, cluster_groups == 1)
Cluster_1
Cluster_1$PriceMSRP
min(Cluster_1$PriceMSRP)
max(Cluster_1$PriceMSRP)

plot(Cluster_1$PriceMSRP)

EV_Price_lm_c1 <- lm(PriceMSRP ~ ., data = Cluster_1)
summary(EV_Price_lm_c1)

FwdfitPrice_c1 <- ols_step_forward_p(EV_Price_lm_c1, penter=.05)
FwdfitPrice_c1

Final_Pricelm_c1 <- lm(PriceMSRP ~ `Power (HP)`, data = Cluster_1)
summary(Final_Pricelm_c1)

test_c1 <- predict(Final_Pricelm_c1, newdata = Cluster_1)
test_c12 <- sqrt(mean((test_c1 - Cluster_1$PriceMSRP)^2))
test_c12

#Cluster 2

Cluster_2 <- subset(data2, cluster_groups == 2)
Cluster_2
min(Cluster_2$PriceMSRP)
max(Cluster_2$PriceMSRP)
plot(Cluster_2$PriceMSRP)

EV_Price_lm_c2 <- lm(PriceMSRP ~ ., data = Cluster_2)
summary(EV_Price_lm_c2)

FwdfitPrice_c2 <- ols_step_forward_p(EV_Price_lm_c2, penter=.05)
FwdfitPrice_c2

Final_Pricelm_c2 <- lm(PriceMSRP ~ `Acceleration 0-60mph (s)` + UCity + rangeHwy, data = Cluster_2)
summary(Final_Pricelm_c2)

test_c2 <- predict(Final_Pricelm_c2, newdata = Cluster_2)
test_c22 <- sqrt(mean((test_c2 - Cluster_2$PriceMSRP)^2))
test_c22

#Cluster 3

Cluster_3 <- subset(data2, cluster_groups == 3)
Cluster_3
min(Cluster_3$PriceMSRP)
max(Cluster_3$PriceMSRP)
plot(Cluster_3$PriceMSRP)

EV_Price_lm_c3 <- lm(PriceMSRP ~ ., data = Cluster_3)
summary(EV_Price_lm_c3)

FwdfitPrice_c3 <- ols_step_forward_p(EV_Price_lm_c3, penter=.05)
FwdfitPrice_c3

Final_Pricelm_c3 <- lm(PriceMSRP ~ UCity, data = Cluster_3)
summary(Final_Pricelm_c3)

test_c3 <- predict(Final_Pricelm_c3, newdata = Cluster_3)
test_c32 <- sqrt(mean((test_c3 - Cluster_3$PriceMSRP)^2))
test_c32

##########################################
##############PCA#########################
##########################################

library(tidyverse)


price_EUC <- dist(data, method = "euclidean")
price_HC <- hclust(price_EUC, method = "ward.D2")
plot(price_HC, cex = 0.3)
fviz_dend(price_HC, k = 3)

cluster_groups <- cutree(price_HC, k = 3)

#Cluster 1

Cluster_1 <- subset(data, cluster_groups == 1)
Cluster_1
Cluster_1$PriceMSRP
min(Cluster_1$PriceMSRP)
max(Cluster_1$PriceMSRP)

Cluster_1.2 <- Cluster_1[,-23]

EV_Price_PC <- prcomp(Cluster_1.2, scale = TRUE, center = TRUE)
EV_Price_PC
plot(EV_Price_PC)


EV_Price_PC_Cbind <- cbind(Cluster_1,EV_Price_PC$x[,1:5])
head(EV_Price_PC_Cbind)

EV_Price_PC_Cbind$PC1

Final_EV_Price_PC <- lm(PriceMSRP ~ PC1, data = EV_Price_PC_Cbind)
summary(Final_EV_Price_PC)

Final_EV_Price_PC2 <- lm(PriceMSRP ~ PC1 + PC2, data = EV_Price_PC_Cbind)
summary(Final_EV_Price_PC2)

Final_EV_Price_PC3 <- lm(PriceMSRP ~ PC1 + PC2 + PC3, data = EV_Price_PC_Cbind)
summary(Final_EV_Price_PC3)

Final_EV_Price_PC4 <- lm(PriceMSRP ~ PC1 + PC2 + PC3 + PC4, data = EV_Price_PC_Cbind)
summary(Final_EV_Price_PC4)

Final_EV_Price_PC5 <- lm(PriceMSRP ~ PC1 + PC2 + PC3 + PC4 + PC5, data = EV_Price_PC_Cbind)
summary(Final_EV_Price_PC5)


test_pc1 <- predict(Final_EV_Price_PC4, newdata = EV_Price_PC_Cbind)
test_pc12 <- sqrt(mean((test_pc1 - EV_Price_PC_Cbind$PriceMSRP)^2))
test_pc12

(EV_Price_PC_Cbind$PC1)

Final_EV_Price_PC4$coefficients
head(EV_Price_PC_Cbind$PC1)
mean(Cluster_3$PriceMSRP)

#Cluster 2

Cluster_2 <- subset(data, cluster_groups == 2)
Cluster_2
Cluster_2$PriceMSRP
min(Cluster_2$PriceMSRP)
max(Cluster_2$PriceMSRP)

Cluster_2.2 <- Cluster_2[c(-23,-33)]

EV_Price_PC2 <- prcomp(Cluster_2.2, scale = TRUE, center = TRUE)
EV_Price_PC2
plot(EV_Price_PC2)


EV_Price_PC_Cbind2 <- cbind(Cluster_2,EV_Price_PC2$x[,1:6])
head(EV_Price_PC_Cbind2)


Final_EV_Price_PC <- lm(PriceMSRP ~ PC1, data = EV_Price_PC_Cbind2)
summary(Final_EV_Price_PC)

Final_EV_Price_PC2 <- lm(PriceMSRP ~ PC1 + PC2, data = EV_Price_PC_Cbind2)
summary(Final_EV_Price_PC2)

Final_EV_Price_PC3 <- lm(PriceMSRP ~ PC1 + PC2 + PC3, data = EV_Price_PC_Cbind2)
summary(Final_EV_Price_PC3)

Final_EV_Price_PC4 <- lm(PriceMSRP ~ PC1 + PC2 + PC3 + PC4, data = EV_Price_PC_Cbind2)
summary(Final_EV_Price_PC4)

Final_EV_Price_PC5 <- lm(PriceMSRP ~ PC1 + PC2 + PC3 + PC4 + PC5, data = EV_Price_PC_Cbind2)
summary(Final_EV_Price_PC5)

Final_EV_Price_PC6 <- lm(PriceMSRP ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = EV_Price_PC_Cbind2)
summary(Final_EV_Price_PC6)



test_pc1 <- predict(Final_EV_Price_PC5, newdata = EV_Price_PC_Cbind2)
test_pc12 <- sqrt(mean((test_pc1 - EV_Price_PC_Cbind2$PriceMSRP)^2))
test_pc12

Final_EV_Price_PC$model$PC1
head(EV_Price_PC_Cbind2$PC1)

#Cluster 3

Cluster_3 <- subset(data, cluster_groups == 3)
Cluster_3
Cluster_3$PriceMSRP
min(Cluster_3$PriceMSRP)
max(Cluster_3$PriceMSRP)

Cluster_3.2 <- Cluster_3[c(-7, -23, -24)]

EV_Price_PC3 <- prcomp(Cluster_3.2, scale = TRUE, center = TRUE)
EV_Price_PC3
plot(EV_Price_PC3)


EV_Price_PC_Cbind3 <- cbind(Cluster_3,EV_Price_PC3$x[,1:5])
head(EV_Price_PC_Cbind3)


Final_EV_Price_PC <- lm(PriceMSRP ~ PC1, data = EV_Price_PC_Cbind3)
summary(Final_EV_Price_PC)

Final_EV_Price_PC2 <- lm(PriceMSRP ~ PC1 + PC2, data = EV_Price_PC_Cbind3)
summary(Final_EV_Price_PC2)

Final_EV_Price_PC3 <- lm(PriceMSRP ~ PC1 + PC2 + PC3, data = EV_Price_PC_Cbind3)
summary(Final_EV_Price_PC3)

Final_EV_Price_PC4 <- lm(PriceMSRP ~ PC1 + PC2 + PC3 + PC4, data = EV_Price_PC_Cbind3)
summary(Final_EV_Price_PC4)

Final_EV_Price_PC5 <- lm(PriceMSRP ~ PC1 + PC2 + PC3 + PC4 + PC5, data = EV_Price_PC_Cbind3)
summary(Final_EV_Price_PC5)


test_pc1 <- predict(Final_EV_Price_PC4, newdata = EV_Price_PC_Cbind3)
test_pc12 <- sqrt(mean((test_pc1 - EV_Price_PC_Cbind3$PriceMSRP)^2))
test_pc12

Final_EV_Price_PC$model$PC1
head(EV_Price_PC_Cbind2$PC1)




















































########################################################################################
########################################################################################
#################################Question 2 - Price Predicting##########################
########################################################################################
########################################################################################

library(caTools)


############################################################
#######################New Price Data Set###################
############################################################


###########################EV Price#########################

pricedata3 <- pricedata2

pricedata3 <- filter(pricedata3, Engine_Fuel_Type == 'electric')

pricedata3 <- pricedata3[c(-5,-6)]

pricedata3.2 <- na.omit(pricedata3)

####Don't Need for lms####
pricedata3.2$Make <- as.factor(pricedata3.2$Make) 
pricedata3.2$Model <- as.factor(pricedata3.2$Model)
pricedata3.2$Engine_Fuel_Type <- as.factor(pricedata3.2$Engine_Fuel_Type)
pricedata3.2$`Transmission Type` <- as.factor(pricedata3.2$`Transmission Type`)
pricedata3.2$Driven_Wheels <- as.factor(pricedata3.2$Driven_Wheels)
pricedata3.2$`Market Category` <- as.factor(pricedata3.2$`Market Category`)
pricedata3.2$`Vehicle Size` <- as.factor(pricedata3.2$`Vehicle Size`)
pricedata3.2$`Vehicle Style` <- as.factor(pricedata3.2$`Vehicle Style`)

pricedata3.2$Make <- as.numeric(pricedata3.2$Make) 
pricedata3.2$Model <- as.numeric(pricedata3.2$Model)
pricedata3.2$Engine_Fuel_Type <- as.numeric(pricedata3.2$Engine_Fuel_Type)
pricedata3.2$`Transmission Type` <- as.numeric(pricedata3.2$`Transmission Type`)
pricedata3.2$Driven_Wheels <- as.numeric(pricedata3.2$Driven_Wheels)
pricedata3.2$`Market Category` <- as.numeric(pricedata3.2$`Market Category`)
pricedata3.2$`Vehicle Size` <- as.numeric(pricedata3.2$`Vehicle Size`)
pricedata3.2$`Vehicle Style` <- as.numeric(pricedata3.2$`Vehicle Style`)

################################Whole data Set###################################################################

###EVs##

priceev <- pricedata3.2

plot(priceev$Year, priceev$MSRP)

iqr <- IQR(priceev$MSRP)
high <- quantile(priceev$MSRP, 0.75) + 1.5*iqr
low <- quantile(priceev$MSRP, 0.25) -1.5*iqr
priceev <- subset(priceev, priceev$MSRP > low & priceev$MSRP < high) 

evlm <- lm(MSRP ~ Year, data = priceev)
evlm


plot(priceev$Year, priceev$MSRP)
abline(evlm, col = "Red")


###ICE####

pricedata4 <- filter(pricedata2, Engine_Fuel_Type != 'electric')

iqr <- IQR(pricedata4$MSRP)
high <- quantile(pricedata4 $MSRP, 0.75) + 1.5*iqr
low <- quantile(pricedata4 $MSRP, 0.25) -1.5*iqr
pricedata4 <- subset(pricedata4, pricedata4$MSRP > low & pricedata4$MSRP < high) 


Pricedataicelm <- lm(MSRP ~ Year, data = pricedata4)

plot(pricedata4$Year,pricedata4$MSRP)
abline(Pricedataicelm, col = "Red")
Pricedataicelm











