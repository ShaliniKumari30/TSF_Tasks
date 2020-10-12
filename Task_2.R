# Simple Linear Regression 

# Libraries 
library(tidyverse)
library(caTools)
library(ggplot2)
library(e1071)

# Importing Dataset 
Hours_data <- read.csv("Task_2.csv")                                                                  
Hours_data

# Graphical Analysis 

# Scatter Plot

scatter.smooth(x =Hours_data$Hours , y = Hours_data$Scores , main = "Scores ~ Hours" )

# Box Plot 

par(mfrow = c(1,2))  
outlier_values <- boxplot.stats(Hours_data$Hours)$out
boxplot(Hours_data$Hours , main = "Hours" )
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
outlier_values <- boxplot.stats(Hours_data$Scores)$out
boxplot(Hours_data$Scores , main = "Scores" )
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

# Density Plot 

par(mfrow = c(1,2))  
plot(density(Hours_data$Hours), main="Density Plot: Hours", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Hours_data$Hours), 2)))   
polygon(density(Hours_data$Hours), col="red")
plot(density(Hours_data$Scores), main="Density Plot: Scores", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Hours_data$Scores), 2)))
polygon(density(Hours_data$Scores), col="red")

# Correlation 

cor(Hours_data$Hours , Hours_data$Scores)

# Splitting the dataset into Training set and Test set 

set.seed(123)                                                                                                                                       
split <- sample.split(Hours_data$Scores , SplitRatio = 0.75)    
training_set <- subset(Hours_data , split == TRUE)
training_set
test_set <- subset(Hours_data , split == FALSE)
test_set

# Fitting Simple Linear Regression to a training set 

regressor <- lm(formula = Scores ~ Hours , data = training_set)
regressor

summary(regressor)  

# Predicting the test set results 

y_pred <- predict(regressor , newdata = test_set)
y_pred


# Visualizing the training set results 

ggplot()+
  geom_point(aes(x = training_set$Hours , y = training_set$Scores) , 
             colour = 'red') +
  geom_line(aes(x = training_set$Hours , y =  predict(regressor , newdata = training_set)),
            colour = 'blue') +
  ggtitle('Scores vs Hours(Training set)')+
  xlab('Hours')+
  ylab('Scores')

# Visualizing the test set results 

ggplot()+
  geom_point(aes(x = test_set$Hours , y = test_set$Scores) , 
             colour = 'red') +
  geom_line(aes(x = test_set$Hours , y =  predict(regressor , newdata = test_set)),
            colour = 'blue') +
  ggtitle('Scores vs Hours(Test set)')+
  xlab('Hours')+
  ylab('Scores')

# Predicting the value   

predict(regressor,data.frame(Hours = 9.25 , Scores = " "))  