# Import Data from the working directory into R
german_data <- read.csv("numeric_german.csv", header = TRUE)

# Random Sampling is performed in order to select the data at random

# Index is the actual variable that divides the dataset
sample_size <- 0.765*nrow(german_data)
set.seed(80)
index_value <- sample( seq_len(nrow(german_data)), size = sample_size )

# Dataset is divided into Training Data and Test Data
data_train <- german_data[index_value, ]
data_test <- german_data[-index_value, ]

# Scale the data so that it is of approprite size
max <- apply(german_data , 2 , max)
min <- apply(german_data, 2 , min)
scaled_german_data <- as.data.frame(scale(german_data, center = min, scale = max - min))

# Note- Ensure that packages nnet, neuralnet, ggplot and devtools are already installed

# Load the required libraries
library(nnet)
library(neuralnet)

# Creating training and test set
train_NN <- scaled_german_data[index_value , ]
test_NN <- scaled_german_data[-index_value , ]

# Fit neural network and train 
set.seed(2)
NN = neuralnet(ans~a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x, train_NN, hidden = c(20,20), threshold = 0.001, learningrate.factor = list(minus=0.2, plus = 0.9), stepmax = 1e+06, linear.output = FALSE)

# Import the function from Github to get a legible neural network model
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

# Plot the neural network model
plot.nnet(NN)

# Prediction using the trained neural network
library(neuralnet)
predict_test_NN <- compute(NN, test_NN[,c(1:24)])
predict_test_NN <- (predict_test_NN$net.result * (max(german_data$ans) - min(german_data$ans))) + min(german_data$ans)

# The Predicted Creditabilitxy is plotted against the Actual Creditability
plot(data_test$ans, predict_test_NN, col='red', pch=16, ylab = "Predicted Creditability", xlab = "Real Creditability")

predict_test_NN <- round(predict_test_NN)
gc_gc<-0
gc_bc<-0
bc_gc<-0
bc_bc<-0

# Classifying the predicted data according to the confusion matrix
for (count in c(1:235))
{
  if((predict_test_NN[count]==1)&&(data_test[count,25]==1))
  {gc_gc<-gc_gc+1}
  else if ((predict_test_NN[count]==0)&&(data_test[count,25]==1))
  {gc_bc<-gc_bc+1}
  else if ((predict_test_NN[count]==1)&&(data_test[count,25]==0))
  {bc_gc<-bc_gc+1}
  else
  {bc_bc<-bc_bc+1}
}

# Calculating the precision, recall and F-score
precision <- (gc_gc/(gc_gc+bc_gc))*100
recall <- (gc_gc/(gc_gc+gc_bc))*100
fscore <- ((2*precision*recall)/(precision+recall))*100

#Calculating accuracy
test_accuracy = ((gc_gc+bc_bc)/(gc_gc+bc_bc+bc_gc+gc_bc))*100

# Calculating total and average cost using the confusion matrix
total_cost <- (5*bc_gc)+gc_bc
average_cost <- total_cost/235
