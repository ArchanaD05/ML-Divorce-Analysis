divorce$Class <- as.factor(divorce$Class)

#Load the ggplot2 library
library(ggplot2)

#Plot the scatterplot matrix for Q1 and Q3 with divorce event
ggplot(divorce, aes(x = Atr1, y = Atr3)) +
  geom_point(aes(fill = Class, shape = Class, color = Class)) +
  xlab("Attribute 1") +
  ylab("Attribute 3") + 
  ggtitle("Attribute 1 and Attribute 3 vs Divorce") +
  theme(plot.title = element_text(hjust = 0.5))


#Plot the scatterplot matrix for Q6 and Q7 with divorce event
ggplot(divorce, aes(x = Atr6, y = Atr7)) +
  geom_point(aes(fill = Class, shape = Class, color = Class)) +
  xlab("Attribute 6") +
  ylab("Attribute 7") + 
  ggtitle("Attribute 6 and Attribute 7 vs Divorce") +
  theme(plot.title = element_text(hjust = 0.5))

#Perform a VIF analysis
#Create the linear model
divorce$Class <- as.numeric(divorce$Class)
linearmodel <- lm(Class ~ .,data = divorce)
summary(linearmodel)
#install.packages("car",dependencies = TRUE)
library(car)
viftable <- vif(linearmodel)
viftable
sorttable <- sort(viftable,decreasing=TRUE)
sorttable
sorttable[sorttable < 8]

library(neuralnet)
set.seed(9)
Divorce_Index <- sample(nrow(divorce), 0.7 * nrow(divorce), replace = FALSE)
Divorce_Train <- divorce[Divorce_Index, ]
Divorce_Test <- divorce[-Divorce_Index, ]

#Create the first neural network
NeuralNetwork1 <- neuralnet(Class ~ Atr6 + Atr7 + Atr42 + Atr43 + Atr45 + Atr46 + Atr47 + Atr48 + Atr52, Divorce_Train,
                 hidden = 4, lifesign = "minimal", linear.output = FALSE, threshold = 0.1)

#Plot the neural network
plot(NeuralNetwork1)

#Test the neural network 
Temp_Test1 <- subset(Divorce_Test, select = c("Atr6","Atr7","Atr42","Atr43","Atr45","Atr46","Atr47","Atr48","Atr52"))

NeuralNetwork1.results <- compute(NeuralNetwork1, Temp_Test1)

results <- data.frame(actual = Divorce_Test$Class, prediction = NeuralNetwork1.results$net.result)

#View the results
results$prediction <- round(results$prediction)
results[1:15, ]

#Convert the data type to factor
Actual <- as.factor(Divorce_Test$Class)
Neural_Result <- as.factor(round(results$prediction))

#Build confusion matrix
PredictTable <- table(Neural_Result, Actual)
PredictTable

#Calculate the accuracy
sum(diag(PredictTable) / sum(PredictTable))

#Create the second neural network
NeuralNetwork2 <- neuralnet(Class ~ Atr7 + Atr43 + Atr46 + Atr48 + Atr52, Divorce_Train,
                 hidden = 4, lifesign = "minimal", linear.output = FALSE, threshold = 0.1)

plot(NeuralNetwork2)

#Test the neural network 
Temp_Test2 <- subset(Divorce_Test, select = c("Atr7","Atr43","Atr46","Atr48","Atr52"))

NeuralNetwork2.results <- compute(NeuralNetwork2, Temp_Test2)

results <- data.frame(actual = Divorce_Test$Class, prediction = NeuralNetwork2.results$net.result)

#View the results
results$prediction <- is.numeric(results$prediction)
results$prediction <- round(results$prediction)
results[1:15, ]

#Convert the data type to factor
Actual <- as.factor(Divorce_Test$Class)
Neural_Result <- as.factor(round(results$prediction))

#Build confusion matrix
PredictTable <- table(Neural_Result, Actual)
PredictTable

#Calculate the accuracy
sum(diag(PredictTable) / sum(PredictTable))

