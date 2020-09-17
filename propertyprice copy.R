pp <- read.csv("Property_Price_Train.csv", header = TRUE)
View(pp)
str(pp)
summary(pp)

#EDA
hist(pp$Building_Class)
boxplot(pp$Building_Class)
boxplot(pp$BsmtFinSF2)
boxplot(pp$BsmtUnfSF )
max(pp$BsmtUnfSF)
boxplot(pp$Total_Basement_Area)
hist(pp$Total_Basement_Area)
boxplot(pp$First_Floor_Area)
hist(pp$First_Floor_Area)
hist(pp$LowQualFinSF)
boxplot(pp$Second_Floor_Area)
hist(pp$Second_Floor_Area)
boxplot(pp$Grade_Living_Area)
boxplot(pp$Rooms_Above_Grade)
hist(pp$Garage_Area)
boxplot(pp$Garage_Area)
hist(pp$Open_Lobby_Area)
which(pp$Pool_Area > 0)
boxplot(pp$ Month_Sold)
boxplot(pp$Sale_Type)
boxplot(pp$Sale_Price)
pp[,-1]
pp
pp <- pp[,-1]
View(pp)
pairs(~Sale_Price + Building_Class + Lot_Size, data = pp)

 #treatment of building class
summary(pp$Building_Class)
uv_bc <- 3*quantile(pp$Building_Class, 0.99)
pp$Building_Class[pp$Building_Class > uv_bc] <- uv_bc
summary(pp$Building_Class)
hist(pp$Building_Class)

 #treatmennt of Lot_extent
summary(pp$Lot_Extent)
which(is.na(pp$Lot_Extent))
pp$Lot_Extent[is.na(pp$Lot_Extent)] <- mean(pp$Lot_Extent, na.rm = TRUE)
summary(pp$Lot_Extent)
boxplot(pp$Lot_Extent)
uv_ls <- 3*quantile(pp$Lot_Extent, 0.99)
pp$Lot_Extent[pp$Lot_Extent > uv_ls] <-uv_ls
summary(pp$Lot_Extent)
lv_le = 0.3* quantile(pp$Lot_Extent, 0.01)
pp$Lot_Extent[pp$Lot_Extent < lv_le] <-lv_le
summary(pp$Lot_Extent)
hist(pp$Lot_Extent)
cor(pp$Lot_Extent, pp$Sale_Price)


#treatment of Lot size
summary(pp$Lot_Size)
uv_lsz <- 3*quantile(pp$Lot_Size, 0.99)
pp$Lot_Size[pp$Lot_Size > uv_lsz] <-uv_lsz
summary(pp$Lot_Size)
lv_lsz = 0.3* quantile(pp$Lot_Size, 0.01)
pp$Lot_Size[pp$Lot_Size< lv_lsz] <-lv_lsz
summary(pp$Lot_Size)
boxplot(pp$Lot_Size)
hist(pp$Lot_Size)
uv_lsz <- 2*quantile(pp$Lot_Size, 0.99)
pp$Lot_Size[pp$Lot_Size > uv_lsz] <-uv_lsz
summary(pp$Lot_Size)
hist(pp$Lot_Size)
cor(pp$Sale_Price, pp$Lot_Size)
boxplot(pp$Lot_Size)

#ovarall maerial
summary(pp$Overall_Material)
hist(pp$Overall_Material)
boxplot(pp$Overall_Material)
lv_om = 0.3* quantile(pp$Overall_Material, 0.01)
pp$Overall_Material[pp$Overall_Material< lv_om] <-lv_om
summary(pp$Overall_Material)

#house condition
summary(pp$House_Condition)
boxplot(pp$House_Condition)
lv_hc = 0.3* quantile(pp$House_Condition, 0.01)
pp$House_Condition[pp$House_Condition < lv_hc] <-lv_hc
summary(pp$House_Condition)

#construction year
summary(pp$Construction_Year)
lv_cy = 0.3* quantile(pp$Construction_Year, 0.01)
pp$Construction_Year[pp$Construction_Year < lv_cy] <-lv_cy
summary(pp$Construction_Year)
hist(pp$Construction_Year)

#remodel year
summary(pp$Remodel_Year)
hist(pp$Remodel_Year)
cor(pp$Construction_Year, pp$Remodel_Year)

#roof quality
summary(pp$Roof_Quality)
pp <- pp[,-22]
View(pp)

#replace NA from brick veener type
summary(pp$Brick_Veneer_Type)
which(is.na(pp$Brick_Veneer_Type))


#brick veener area
summary(pp$Brick_Veneer_Area)
mean(pp$Brick_Veneer_Area[is.na(pp$Brick_Veneer_Area)], na.rm = TRUE)
summary(pp$Brick_Veneer_Area)
pp$Brick_Veneer_Area[is.na(pp$Brick_Veneer_Area)] <- mean(pp$Brick_Veneer_Area, na.rm = TRUE)
summary(pp$Brick_Veneer_Area)
boxplot(pp$Brick_Veneer_Area)
uv_bva <- 3*quantile(pp$Brick_Veneer_Area, 0.99)
pp$Brick_Veneer_Area[pp$Brick_Veneer_Area > uv_bva] <- uv_bva
summary(pp$Brick_Veneer_Area)
hist(pp$Brick_Veneer_Area)
lv_bva <- 0.3*quantile(pp$Brick_Veneer_Area, 0.01)
pp$Brick_Veneer_Area[pp$Brick_Veneer_Area < lv_bva] <- lv_bva
summary(pp$Brick_Veneer_Area)
boxplot(pp$Brick_Veneer_Area)
hist(pp$Brick_Veneer_Area)
cor(pp$Brick_Veneer_Area, pp$Sale_Price)

#bsmtfinsf1
summary(pp$BsmtFinSF1)
hist(pp$BsmtFinSF1)
uv_bfs <- 3*quantile(pp$BsmtFinSF1, 0.99)
pp$BsmtFinSF1[pp$BsmtFinSF1 > uv_bfs] <- uv_bfs
summary(pp$BsmtFinSF1)
lv_bfs = 0.3* quantile(pp$BsmtFinSF1, 0.01)
pp$BsmtFinSF1[pp$BsmtFinSF1 < lv_bfs] <- lv_bfs
summary(pp$BsmtFinSF1)

#bsmtfinsf2
summary(pp$BsmtFinSF2)
hist(pp$BsmtFinSF2)
uv_bfs2 <- 3*quantile(pp$BsmtFinSF2, 0.99)
pp$BsmtFinSF2[pp$BsmtFinSF2 > uv_bfs2] <- uv_bfs2
summary(pp$BsmtFinSF2)

#bsmtunfsf
summary(pp$BsmtUnfSF)
hist(pp$BsmtUnfSF)
uv_bus <- 3*quantile(pp$BsmtFinSF2, 0.99)
pp$BsmtUnfSF[pp$BsmtUnfSF > uv_bus] <- uv_bus
summary(pp$BsmtUnfSF)

#total basement area
summary(pp$Total_Basement_Area)
boxplot(pp$Total_Basement_Area)
uv_tba <- 3*quantile(pp$Total_Basement_Area, 0.99)
pp$Total_Basement_Area[pp$Total_Basement_Area > uv_tba] <- uv_tba
summary(pp$Total_Basement_Area)
pairs(~Sale_Price + Total_Basement_Area, data = pp)

#1st floor area
summary(pp$First_Floor_Area)
hist(pp$First_Floor_Area)
boxplot(pp$First_Floor_Area)
uv_ffa <- 3*quantile(pp$First_Floor_Area, 0.99)
pp$First_Floor_Area[pp$First_Floor_Area > uv_ffa] <- uv_ffa
summary(pp$First_Floor_Area)

#2nd floor area
summary(pp$Second_Floor_Area)
pairs(~Sale_Price + Second_Floor_Area, data = pp)
boxplot(pp$Second_Floor_Area)
uv_sfa <- 3*quantile(pp$Second_Floor_Area, 0.99)
pp$Second_Floor_Area[pp$Second_Floor_Area > uv_sfa] <- uv_sfa
summary(pp$Second_Floor_Area)

#low qualitySF
summary(pp$LowQualFinSF)
hist(pp$LowQualFinSF)
boxplot(pp$LowQualFinSF)
cor(pp$Sale_Price, pp$LowQualFinSF)
pp <- pp[,-44]
View(pp)

#grade living area
summary(pp$Grade_Living_Area)
boxplot(pp$Grade_Living_Area)
uv_gla <- 3*quantile(pp$Grade_Living_Area, 0.99)
pp$Grade_Living_Area[pp$Grade_Living_Area > uv_gla] <- uv_gla
summary(pp$Grade_Living_Area)
hist(pp$Grade_Living_Area)
cor(pp$Sale_Price, pp$Grade_Living_Area )

#avg area calulation
pp$totalarea <- (pp$First_Floor_Area +pp$Second_Floor_Area)
pp$totalarea
View(pp) 
pp <- pp[,-42:-43]
View(pp)
pp$Avg_floor_area <- (pp$Grade_Living_Area + pp$totalarea)/2
View(pp)
pp <- pp[,-74]
pp <- pp[,-42]
View(pp)
summary(pp$Avg_floor_area)
hist(pp$Avg_floor_area)
uv_afa <- 2*quantile(pp$Avg_floor_area, 0.99)
pp$Avg_floor_area[pp$Avg_floor_area > uv_afa] <- uv_afa  
summary(pp$Avg_floor_area) 
pp <- pp[,-68]
View(pp)

#ug full bathroom
summary(pp$Underground_Full_Bathroom)
boxplot(pp$Underground_Full_Bathroom)
uv_ufb <- 3*quantile(pp$Underground_Full_Bathroom, 0.99)
pp$Underground_Full_Bathroom[pp$Underground_Full_Bathroom > uv_ufb] <- uv_ufb
summary(pp$Underground_Full_Bathroom)

#ug half bathroom
summary(pp$Underground_Half_Bathroom)
pp <- pp[,-46]
View(pp)

#full bathroom above grade
summary(pp$Full_Bathroom_Above_Grade)
cor(pp$Sale_Price, pp$Full_Bathroom_Above_Grade)
hist(pp$Full_Bathroom_Above_Grade)
boxplot(pp$Full_Bathroom_Above_Grade)


#half bathroom above grade
summary(pp$Half_Bathroom_Above_Grade)
cor(pp$Sale_Price, pp$Half_Bathroom_Above_Grade)
hist(pp$Half_Bathroom_Above_Grade)
boxplot(pp$Half_Bathroom_Above_Grade)
pairs(~ Sale_Price + Half_Bathroom_Above_Grade, data = pp)
uv_hbag <- 2*quantile(pp$Half_Bathroom_Above_Grade, 0.99)
pp$Half_Bathroom_Above_Grade[pp$Half_Bathroom_Above_Grade > uv_hbag] <- uv_hbag
summary(pp$Half_Bathroom_Above_Grade)

#replacing bathroom above grade
pp$Bathroom_Above_Grade <- (pp$Half_Bathroom_Above_Grade + pp$Full_Bathroom_Above_Grade)/2
View(pp)
#removal half and full Bathroom_Above_Grade
pp <- pp[,-46:-47]
View(pp)
summary(pp$Bathroom_Above_Grade)
hist(pp$Bathroom_Above_Grade)
boxplot(pp$Bathroom_Above_Grade)

#bedroom above grade
summary(pp$Bedroom_Above_Grade)
boxplot(pp$Bedroom_Above_Grade)
uv_bag <- 4*quantile(pp$Bedroom_Above_Grade, 0.99)
pp$Bedroom_Above_Grade[pp$Bedroom_Above_Grade > uv_bag] <- uv_bag
summary(pp$Bedroom_Above_Grade)
hist(pp$Bedroom_Above_Grade)


#kitchen above grade
summary(pp$Kitchen_Above_Grade)
boxplot(pp$Kitchen_Above_Grade)
hist(pp$Kitchen_Above_Grade)
pp <- pp[,-47]
View(pp)

#rooms above grade
summary(pp$Rooms_Above_Grade)
hist(pp$Rooms_Above_Grade)
uv_rag <- 4*quantile(pp$Rooms_Above_Grade, 0.99)
pp$Rooms_Above_Grade[pp$Rooms_Above_Grade > uv_rag] <- uv_rag
summary(pp$Rooms_Above_Grade)
boxplot(pp$Rooms_Above_Grade)

#fireplace
summary(pp$Fireplaces)
boxplot(pp$Fireplaces)
hist(pp$Fireplaces)
uv_fp <- 3*quantile(pp$Fireplaces, 0.99)
pp$Fireplaces[pp$Fireplaces > uv_fp] <- uv_fp
summary(pp$Fireplaces)
cor(pp$Sale_Price, pp$Fireplaces)

#garage built year
summary(pp$Garage_Built_Year)
which(is.na(pp$Garage_Built_Year))
pp$Garage_Built_Year[is.na(pp$Garage_Built_Year)] <- mean(pp$Garage_Built_Year,na.rm = TRUE)
summary(pp$Garage_Built_Year)
lv_gby <- 0.3*quantile(pp$Garage_Built_Year, 0.01)
pp$Garage_Built_Year[pp$Garage_Built_Year < lv_gby] <- lv_gby
summary(pp$Garage_Built_Year)
hist(pp$Garage_Built_Year)
boxplot(pp$Garage_Built_Year)
cor(pp$Sale_Price, pp$Garage_Built_Year)

#garage size
summary(pp$Garage_Size)
hist(pp$Garage_Size)
boxplot(pp$Garage_Size)
cor(pp$Sale_Price, pp$Garage_Size)

#garage area 
summary(pp$Garage_Area)
hist(pp$Garage_Area)
cor(pp$Garage_Area, pp$Sale_Price)
lv_ga <- 0.3*quantile(pp$Garage_Area, 0.01)
pp$Garage_Area[pp$Garage_Area < lv_ga] <- lv_ga     
summary(pp$Garage_Area)
hist(pp$Garage_Area)
boxplot(pp$Garage_Area)
uv_ga <- 3*quantile(pp$Garage_Area, 0.99)
pp$Garage_Area[pp$Garage_Area > uv_ga] <- uv_ga
summary(pp$Garage_Area)
cor(pp$Garage_Area, pp$Sale_Price)


#w deck area
summary(pp$W_Deck_Area)
hist(pp$W_Deck_Area)
lv_wda <- 0.3*quantile(pp$W_Deck_Area, 0.01)
pp$W_Deck_Area[pp$W_Deck_Area < lv_wda] <- lv_wda 
summary(pp$W_Deck_Area)
uv_wda <- 3*quantile(pp$W_Deck_Area, 0.99)
pp$W_Deck_Area[pp$W_Deck_Area > uv_wda] <- uv_wda
summary(pp$W_Deck_Area)
hist(pp$W_Deck_Area)
boxplot(pp$W_Deck_Area)
cor(pp$Sale_Price, pp$W_Deck_Area)

#pool area
summary(pp$Pool_Area)
boxplot(pp$Pool_Area)
hist(pp$Pool_Area)
cor(pp$Sale_Price, pp$Pool_Area)
pp <- pp[,-62]
View(pp)

#three session lobby area
summary(pp$Three_Season_Lobby_Area)
boxplot(pp$Three_Season_Lobby_Area)
cor(pp$Sale_Price, pp$Three_Season_Lobby_Area)

#screen lobby area
summary(pp$Screen_Lobby_Area)
cor(pp$Sale_Price, pp$Screen_Lobby_Area)
cor(pp$Three_Season_Lobby_Area, pp$Screen_Lobby_Area)

pp <- pp[,-60:-61]
View(pp)

#open lobby area
summary(pp$Open_Lobby_Area)
lv_ola <- 0.3*quantile(pp$Open_Lobby_Area, 0.01)
pp$Open_Lobby_Area[pp$Open_Lobby_Area < lv_ola] <- lv_ola 
summary(pp$Open_Lobby_Area)
uv_ola <- 3*quantile(pp$Open_Lobby_Area, 0.99)
pp$Open_Lobby_Area[pp$Open_Lobby_Area > uv_ola] <- uv_ola
summary(pp$Open_Lobby_Area)
hist(pp$Open_Lobby_Area)
boxplot(pp$Open_Lobby_Area)
cor(pp$Open_Lobby_Area, pp$Sale_Price)

#enclosed lobby area
summary(pp$Enclosed_Lobby_Area)
lv_ela <- 0.3*quantile(pp$Enclosed_Lobby_Area, 0.01)
pp$Enclosed_Lobby_Area[pp$Enclosed_Lobby_Area< lv_ela] <- lv_ela
summary(pp$Enclosed_Lobby_Area)
uv_ela <- 3*quantile(pp$Enclosed_Lobby_Area, 0.99)
pp$Enclosed_Lobby_Area[pp$Enclosed_Lobby_Area > uv_ela] <- uv_ela
summary(pp$Enclosed_Lobby_Area)
hist(pp$Enclosed_Lobby_Area)
boxplot(pp$Enclosed_Lobby_Area)
cor(pp$Sale_Price, pp$Enclosed_Lobby_Area)

#avg lobby area
pp$Avg_lobby_area <- (pp$Open_Lobby_Area + pp$Enclosed_Lobby_Area)/2
View(pp)
summary(pp$Avg_lobby_area)
hist(pp$Avg_lobby_area)
lv_ala <- 0.3*quantile(pp$Avg_lobby_area, 0.01)
pp$Avg_lobby_area[pp$Avg_lobby_area < lv_ala] <- lv_ala
summary(pp$Avg_lobby_area)
uv_ala <- 3*quantile(pp$Avg_lobby_area, 0.99)
pp$Avg_lobby_area[pp$Avg_lobby_area > uv_ala] <- uv_ala
summary(pp$Avg_lobby_area)
hist(pp$Avg_lobby_area)
cor(pp$Sale_Price, pp$Avg_lobby_area)

#removal of lobby areas
pp <- pp[, -58:-59]
View(pp)

#miscellaneous values
summary(pp$Miscellaneous_Value)
boxplot(pp$Miscellaneous_Value)
cor(pp$Sale_Price, pp$Miscellaneous_Value)
pp <- pp[,-61]
View(pp)

#month sold
summary(pp$Month_Sold)
hist(pp$Month_Sold)
cor(pp$Sale_Price, pp$Month_Sold)
lv_ms <- 0.3*quantile(pp$Month_Sold, 0.01)
pp$Month_Sold[pp$Month_Sold < lv_ms] <- lv_ms
summary(pp$Month_Sold)

#year sold
summary(pp$Year_Sold)
hist(pp$Year_Sold)

#zoning class
summary(pp$Zoning_Class)
cor(pp$Sale_Price, pp$Zoning_Class)
pp <- pp[,-2]
View(pp)

#road type
summary(pp$Road_Type)
pp <- pp[,-4]
View(pp)

#lane type
summary(pp$Lane_Type)
pp <- pp[,-4]
View(pp)

#property shape
summary(pp$Property_Shape)

#land outline
summary(pp$Land_Outline)
pp <- pp[,-5]
View(pp)

#utility type
summary(pp$Utility_Type)
pp <- pp[,-5]

#lot configurationn
summary(pp$Lot_Configuration)

#property slope
summary(pp$Property_Slope)
pp <- pp[,-6]


#neigbhorhood
summary(pp$Neighborhood)

#condition 1
summary(pp$Condition1)
pp <- pp[,-7]

#condition 2
summary(pp$Condition2)
pp <- pp[,-7]

#house type
summary(pp$House_Type)
pp <- pp[,-7]

#house design
summary(pp$House_Design)

#roof design
summary(pp$Roof_Design)
pp <- pp[,-12]

#exterior first
summary(pp$Exterior1st)

#exterior second
summary(pp$Exterior2nd)

#brick veener type
summary(pp$Brick_Veneer_Type)
library(modeest)
pp$Brick_Veneer_Type[is.na(pp$Brick_Veneer_Type)] <- mfv(pp$Brick_Veneer_Type, na_rm = TRUE)
summary(pp$Brick_Veneer_Type)

#exterior material
summary(pp$Exterior_Material)

#exterior condition
summary(pp$Exterior_Condition)
pp <- pp[,-17]

#foundation type
summary(pp$Foundation_Type)

#basement height
summary(pp$Basement_Height)
library(modeest)
pp$Basement_Height[is.na(pp$Basement_Height)] <- mfv(pp$Basement_Height, na_rm = TRUE)
summary(pp$Basement_Height)

#basement condition
summary(pp$Basement_Condition)
pp <- pp[,-19]

#exposure level
summary(pp$Exposure_Level)
pp$Exposure_Level[is.na(pp$Exposure_Level)] <- mfv(pp$Exposure_Level, na_rm = TRUE)
summary(pp$Exposure_Level)

#bsmtfintype 1
summary(pp$BsmtFinType1)
pp$BsmtFinType1[is.na(pp$BsmtFinType1)] <- mfv(pp$BsmtFinType1, na_rm = TRUE)
summary(pp$BsmtFinType1)

#bsmtfintype 2
summary(pp$BsmtFinType2)
pp <- pp[,-22]

#bsmntfinsf2
summary(pp$BsmtFinSF2)
pp <- pp[,-22]

#heatin type
summary(pp$Heating_Type)
pp <- pp[,-24]

#heating quality
summary(pp$Heating_Quality)

#air conditioning
summary(pp$Air_Conditioning)
pp <- pp[,-25]

#electrical system
summary(pp$Electrical_System)
pp <- pp[,-25]

#bathroomcalculation
pp$Bathroom = (pp$Half_Bathroom_Above_Grade + pp$Full_Bathroom_Above_Grade + pp$Underground_Full_Bathroom + pp$Underground_Half_Bathroom)/4
View(pp)               
pp <- pp[,-49]               
pp <- pp[,-25:-28]               
summary(pp$Bathroom)               
boxplot(pp$Bathroom)               
hist(pp$Bathroom)               
cor(pp$Sale_Price, pp$Bathroom)               

#fireplace
summary(pp$Fireplaces)
hist(pp$Fireplaces)               

#fireplace quality
summary(pp$Fireplace_Quality)
pp <- pp[,-27]               

#garage
summary(pp$Garage)
pp$Garage[is.na(pp$Garage)] <- mfv(pp$Garage, na_rm = TRUE)               
summary(pp$Garage)               

#garage finsh year
summary(pp$Garage_Finish_Year)
pp$Garage_Finish_Year[is.na(pp$Garage_Finish_Year)] <- mfv(pp$Garage_Finish_Year, na_rm = TRUE)               
summary(pp$Garage_Finish_Year)               

#garage quality
summary(pp$Garage_Quality)
pp <- pp[,-32]
 
#garage condition

summary(pp$Garage_Condition)
pp <- pp[,-32]

#pavved drive
summary(pp$Pavedd_Drive)
pp <- pp[,-32]

#pool quality
summary(pp$Pool_Quality)
pp <- pp[,-33]

#fence quality
summary(pp$Fence_Quality)
pp <- pp[,-33]

#misellenaous feature
summary(pp$Miscellaneous_Feature)
pp <- pp[,-33]
 
#sale condition
summary(pp$Sale_Condition)
pp <- pp[,-34]

#total area
summary(pp$totalarea)
pp <- pp[,-35]

library(dummies)
pp <- dummy.data.frame(pp)
View(pp)

#garage finish year fin
summary(pp$Garage_Finish_YearFin)
cor(pp$Sale_Price, pp$Garage_Finish_YearFin)

#removal of columns
pp <- pp[,-7]
pp <- pp[,-11]
pp <- pp[,-35]
pp <- pp[,-42]
pp <- pp[,-70]
pp <- pp[,-78]
pp <- pp[,-83]
pp <- pp[,-88]
pp <- pp[,-91]
pp <- pp[,-94]
pp <- pp[,-99]
pp <- pp[,-106]
pp <- pp[,-113]
pp <- pp[,-117]

model1 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model1)

#removal of less probability components
pp <- pp[,-118]
pp <- pp[,-87]
pp <- pp[,-83]
pp <- pp[,-72]
pp <- pp[,-65]
pp <- pp[,-63]
pp <- pp[,-60]
pp <- pp[,-54]
pp <- pp[,-37]
pp <- pp[,-36]

model2 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model2)
 
#backward elimination
pp <- pp[,-80]
pp <- pp[,-52]
pp <- pp[,-46]
pp <- pp[,-44]
pp <- pp[,-39]
pp <- pp[,-17]

model3 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model3)

#backward elimination
pp <- pp[,-96]
pp <- pp[,-94]
pp <- pp[,-89]
pp <- pp[,-75]
pp <- pp[,-49]
pp <- pp[,-44]

model4 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model4)

#backward elimination
pp <- pp[,-93]
pp <- pp[,-85]
pp <- pp[,-79]
pp <- pp[,-71]
pp <- pp[,-61]
pp <- pp[,-53]
pp <- pp[,-47]
pp <- pp[,-44]
pp <- pp[,-35]
pp <- pp[,-7]

model5 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model5)

#backward elimination
pp <- pp[,-85]
pp <- pp[,-80]
pp <- pp[,-58]
pp <- pp[,-52]
pp <- pp[,-47]
pp <- pp[,-46]
pp <- pp[,-40]

model6 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model6)

#backward elimination
pp <- pp[,-45]
model7 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model7)

#backward elimination
pp <- pp[,-55]
model8 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model8)

#backward elimination
pp <- pp[,-29]
model9 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model9)

#backward elimination
pp <- pp[,-40]
model10 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model10)


#backward elimination
pp <- pp[,-53]
model11 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model11)


#backward elimination
pp <- pp[,-43]
model12 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model12)

#backward elimination
pp <- pp[,-4]
model13 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model13)

#backward elimination
pp <- pp[,-73]
model14 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model14)

#backward elimination
pp <- pp[,-41]
model15 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model15)


#backward elimination
pp <- pp[,-64]
model16 <- lm(pp$Sale_Price ~ ., data = pp)
summary(model16)
View(pp)

#spliting the data
library(caTools)
set.seed(45)
split_pp = sample.split(pp$Sale_Price, SplitRatio = 4/5)
tr_set_pp = subset(pp, split_pp == TRUE)
test_set_pp = subset(pp, split_pp == FALSE)
View(tr_set_pp)
View(test_set_pp)
model17 <- lm(Sale_Price ~ ., data = tr_set_pp)
summary(model17)
pred17 <- predict(model17, newdata = test_set_pp)
View(pred17)

MSE_md17 = mean((test_set_pp$Sale_Price - pred17)^2)
MSE_md17 = sqrt(MSE_md17)
MSE_md17

#removal of columns which of no use
pp <- pp [,-10]

#model prediction
tr_set_pp = subset(pp, split_pp == TRUE)
test_set_pp = subset(pp, split_pp == FALSE)
View(tr_set_pp)
View(test_set_pp)
model18 <- lm(Sale_Price ~ ., data = tr_set_pp)
summary(model18)
pred18 <- predict(model18, newdata = test_set_pp)
View(pred18)

MSE_md18 = mean((test_set_pp$Sale_Price - pred18)^2)
MSE_md18 = sqrt(MSE_md18)
MSE_md18

#removal of columns which of no use
pp <- pp [,-60]
pp <- pp[,-55]
pp <- pp[,-5]

#model prediction
tr_set_pp = subset(pp, split_pp == TRUE)
test_set_pp = subset(pp, split_pp == FALSE)
View(tr_set_pp)
View(test_set_pp)
model19 <- lm(Sale_Price ~ ., data = tr_set_pp)
summary(model19)
pred19 <- predict(model19, newdata = test_set_pp)
View(pred19)

MSE_md19 = mean((test_set_pp$Sale_Price - pred19)^2)
MSE_md19 = sqrt(MSE_md19)
MSE_md19

#removal of columns which of no use
pp <- pp[,-67]
pp <- pp[,-56]
pp <- pp[,-43]
pp <- pp[,-30]
pp <- pp[,-20]

#model prediction
tr_set_pp = subset(pp, split_pp == TRUE)
test_set_pp = subset(pp, split_pp == FALSE)
View(tr_set_pp)
View(test_set_pp)
model20 <- lm(Sale_Price ~ ., data = tr_set_pp)
summary(model20)
pred20 <- predict(model20, newdata = test_set_pp)
View(pred20)

MSE_md20 = mean((test_set_pp$Sale_Price - pred20)^2)
MSE_md20 = sqrt(MSE_md20)
MSE_md20