
#Import file

setwd("D:/GLIB_Analytics_PG/Classes/Mar/materials")
library(readxl)
car_data = read_excel("../../April/advanced_stats/Auto.xlsx")

#conducting PCA 
car_pca = prcomp(car_data[,-1],scale. = T)

#View PCA
summary(car_pca)

# train data
car_train = cbind(car_data$mpg,car_pca$x[,1],car_pca$x[,2])
car_train = as.data.frame(car_train)
names(car_train) = c("mpg","PC1","PC2")

#View train data
View(car_train)

#create linear regression model on train data
car_pca_lm = lm(mpg~., data = car_train)

#View lm model
car_pca_lm

#Predict mpg for train data
car_train$predicated_mpg = predict(car_pca_lm,newdata = car_train,type = "response")

#Add residuals to train data
car_train$residuals = car_pca_lm$residuals

#View predicted data
View(car_train)

# creating test data
car_data_test = as.data.frame(car_data)
car_data_test = car_data_test[-c(1:30),]
View(car_data_test)
car_data_test[1,] = c(16,6,250,105,3897,18.5)
car_data_test[2,] = c(15,8,350,145,4440,14)
car_data_test[3,] = c(27,4,151,90,2950,17.3)

# View test data
View(car_data_test)

# conducting PCA 4 test data
car_test_pca = prcomp(car_data_test[,-1],scale. = T)
# View test PCA
car_test_pca

# creating test data based on PCA scores
car_test = cbind(car_data_test$mpg,car_test_pca$x[,1],car_test_pca$x[,2])
car_test = as.data.frame(car_test)
names(car_test) = c("mpg","PC1","PC2")

#View test data
View(car_test)

#Predict values for test data
car_test$predicted_mpg = predict(car_pca_lm,newdata = car_test)

#residuals
car_test$residuals = car_test$mpg - car_test$predicted_mpg

#View predicted test data
View(car_test)
