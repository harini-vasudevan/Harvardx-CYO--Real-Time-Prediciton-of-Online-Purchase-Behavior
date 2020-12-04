#Automatically install missing packages 
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
#Libraries we are going to work 
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(plyr)
library(dplyr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(cluster)
###Load the data
#Online Shoppers Purchasing Intention Data set 
#https://archive.ics.uci.edu/ml/machine-learning-databases/00468/online_shoppers_intention.csv
#Downloading the file and saving it to dl
dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00468/online_shoppers_intention.csv", dl)
#Read the csv file 
data <- read.csv(dl)
##Quick preview  of data structure
str(data)
head(data)
summary(data) #summary statistics
##Missing value analysis 
colSums(is.na(data))

###DATA PREPROCESSING
#Preserving the logical class of the variable later we change these into numeric
data <- data %>% mutate(Weekend_01 = Weekend , Revenue_01 = Revenue)
#Arranging months by order
data$Month <- factor(data$Month, levels = c("Feb", "Mar", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered = TRUE)
#Convert Weekend and Revenue variable from 'FALSE' and 'TRUE' with '1' or '0'
data$Revenue <- gsub(FALSE, 0, data$Revenue)
data$Revenue <- gsub(TRUE, 1, data$Revenue)
data$Weekend <- gsub(TRUE, 1, data$Weekend)
data$Weekend <- gsub(FALSE, 0, data$Weekend)
#Converting variables to factors with ordinal variables
data$OperatingSystems <- factor(data$OperatingSystems)
data$Browser <- factor(data$Browser)
data$Region <- factor(data$Region)
data$TrafficType <- factor(data$TrafficType)
data$VisitorType <- factor(data$VisitorType)
data$Revenue<- factor(data$Revenue)
data$Weekend<- factor(data$Weekend)

###Exploratory Data Analysis
summary(data[,c(1:10)])  #summary statistics
#Distribution of Revenue
table(data$Revenue)
#Distribution of Weekend
table(data$Weekend)
#Distribution of Visitor Type
table(data$VisitorType)
#Distribution of Traffic Type
table(data$TrafficType)
#Distribution of Region
table(data$Region)
#Distribution of Browser
table(data$Browser)
#Distribution of Operating Systems
table(data$OperatingSystems)
#Distribution of month
table(data$Month)
#Summary of Administrative
summary(data$Administrative)
#summary of Administrative_Duration
summary(data$Administrative_Duration)
#summary of Informational
summary(data$Informational)
#summary of Informational_Duration 
summary(data$Informational_Duration)
#summary of Product Related
summary(data$ProductRelated)
#summary of Product Related_Duration
summary(data$ProductRelated_Duration)

##correlation analysis
correlation <- cor(data[,c(1:10)])
corrplot(correlation, diag = TRUE)

##Relationship between Bounce Rates and Exit Rates
data %>% ggplot(aes(x = BounceRates, y = ExitRates)) +
  geom_point(aes(color = Revenue)) + 
  geom_smooth(se = TRUE, alpha = 0.5) + 
  ggtitle("Relationship between Exit Rates and Bounce Rates") + 
  xlab("Bounce Rates") + ylab("Exit Rates") + 
  geom_text(aes(x = 0.15, y = 0.05, label = "Correlation is 0.913"))

##Revenue based on visitor type and Exit Rates 
visitor_Exit <- data %>% ggplot(aes(VisitorType,ExitRates,color= Revenue)) + 
  geom_point()+ 
  ggtitle("Revenue based on Visitor type  and Exit Rates") + 
  xlab("Visitor Type") + 
  ylab("Exit Rates") +
  theme(legend.position = "bottom") 

##Revenue based on visitor type and page values
visitor_page <- data %>% ggplot(aes(x = VisitorType, y = PageValues,color= Revenue)) + 
  geom_point()+ 
  ggtitle("Revenue based on Visitor type and Page values") + 
  xlab("Visitor type") + 
  ylab("Page values") +
  theme(legend.position = "bottom")

grid.arrange(visitor_Exit ,visitor_page, nrow = 1)

##Revenue based on visitor type and weekends
visitor_type <- data %>% ggplot(aes(x = Revenue)) + 
  geom_bar(aes(fill = VisitorType)) + 
  ggtitle("Revenue based on visitor type") + 
  xlab("Revenue status") + 
  ylab("Visitors") +
  theme(legend.position = "bottom") 

Weekends <- data %>% ggplot(aes(x = Revenue)) + 
  geom_bar(aes(fill = Weekend)) + 
  ggtitle("Revenue based on weekend status") + 
  xlab("Revenue status") + ylab("Visitors") +
  theme(legend.position = "bottom")

grid.arrange(visitor_type ,Weekends, nrow = 1)

##Seasonality Revenue Growth
#Estimate Trend for revenue based on months
Revenue_Month <- data.frame(table(data$Month, data$Revenue))
names(Revenue_Month) <- c("Months", "Revenue", "Frequency")
Revenue_Month %>% ggplot(aes(x = Months, y = Frequency)) + 
  geom_line(aes(color = Revenue, group = Revenue), lwd = 1) +
  geom_point(aes(color = Revenue, group = Revenue, size = 0.2), show.legend = FALSE) + 
  theme_light() + scale_y_continuous() + 
  ggtitle("Trend line for revenue based on months") + 
  xlab("Months") + ylab("Visitors") 

#Estimate the Trend for visitor Type based on months
Visitor_Month <- data.frame(table(data$VisitorType, data$Month))
names(Visitor_Month) <- c("VisitorType", "Month", "Frequency")
Visitor_Month %>% ggplot(aes(x = Month, y = Frequency)) + 
  geom_line(aes(color = VisitorType, group = VisitorType), lwd = 1) + 
  geom_point(aes(color = VisitorType, group = VisitorType, size = 0.1), show.legend = FALSE) +
  theme_light() + scale_y_continuous() + ggtitle("Trend line for visitor type based on months") + 
  xlab("Months") + ylab("Visitors")

##Relationship between Operating Systems and Revenue 
os_Revenue <- data.frame(table(data$OperatingSystems, data$Revenue))
str(os_Revenue)
names(os_Revenue) <- c("OS", "Revenue", "Frequency")
Revenue_0 <- os_Revenue %>% filter(Revenue == 0)
Revenue_0$p <- (Revenue_0$Frequency / sum(Revenue_0$Frequency)) * 100
Revenue_1 <- os_Revenue %>% filter(Revenue == 1)
Revenue_1$p <- (Revenue_1$Frequency / sum(Revenue_1$Frequency)) * 100

#Plotting the Relationship between Operating Systems and Revenue 
Revenue_0 <- Revenue_0 %>% ggplot(aes(x = reorder(OS, -p), y = p)) + 
  geom_bar(stat = "identity", mapping = aes(fill = OS)) +
  scale_y_continuous() +
  xlab("Operating System Types") + 
  ylab("Total visitors in percentage") + 
  theme(legend.position = "bottom") + 
  ggtitle("Relationship between OS and Revenue") + 
  labs(subtitle = "Revenue = 0")
Revenue_1 <- Revenue_1 %>% ggplot(aes(x = reorder(OS, -p), y = p)) + 
  geom_bar(stat = "identity", mapping = aes(fill = OS)) + 
  scale_y_continuous() + 
  xlab("Operating System Types") + 
  ylab("Total visitors in percentage") + 
  theme(legend.position = "bottom") + 
  ggtitle("Relationship between OS and Revenue") + 
  labs(subtitle = "Revenue = 1")
grid.arrange(Revenue_0,Revenue_1, nrow = 1)

##Relationship between Browser and Revenue
Browser_Revenue <- data.frame(table(data$Browser, data$Revenue))
str(Browser_Revenue)
names(Browser_Revenue) <- c("Browser", "Revenue", "Frequency")
Revenue_0B <- Browser_Revenue %>% filter(Revenue == 0)
Revenue_0B$p <- (Revenue_0B$Frequency / sum(Revenue_0B$Frequency)) * 100
Revenue_1B  <- Browser_Revenue %>% filter(Revenue == 1)
Revenue_1B$p <- (Revenue_1B$Frequency / sum(Revenue_1B$Frequency)) * 100

##Plotting the Relationship between Browser and Revenue 
Revenue_0B <-Revenue_0B %>% ggplot(aes(x = reorder(Browser, -p), y = p)) + 
  geom_bar(stat = "identity", mapping = aes(fill = Browser)) +
  scale_y_continuous() +
  xlab("Browser Types") + 
  ylab("Total visitors in percentage") + 
  theme(legend.position = "bottom") + 
  ggtitle("Relationship between Browser and Revenue") + 
  labs(subtitle = "Revenue = 0")
Revenue_1B <-Revenue_1B %>% ggplot(aes(x = reorder(Browser, -p), y = p)) + 
  geom_bar(stat = "identity", mapping = aes(fill = Browser)) +
  scale_y_continuous() +
  xlab("Browser Types") + 
  ylab("Total visitors in percentage") + 
  theme(legend.position = "bottom") + 
  ggtitle("Relationship between Browser and Revenue") + 
  labs(subtitle = "Revenue = 1")
grid.arrange(Revenue_0B,Revenue_1B, nrow = 1)

##Relationship between Region and Revenue
Region_Revenue<- data.frame(table(data$Region, data$Revenue))
str(Region_Revenue)
names(Region_Revenue) <- c("Region", "Revenue", "Frequency")
Revenue_0R <- Region_Revenue %>% filter(Revenue == 0)
Revenue_0R$p <- (Revenue_0R$Frequency / sum(Revenue_0R$Frequency)) * 100
Revenue_1R  <- Region_Revenue %>% filter(Revenue == 1)
Revenue_1R$p <- (Revenue_1R$Frequency / sum(Revenue_1R$Frequency)) * 100

#Plotting the Relationship between Region and Revenue 
Revenue_0R <- Revenue_0R %>% ggplot(aes(x = reorder(Region, -p), y = p)) + 
  geom_bar(stat = "identity", mapping = aes(fill = Region)) +
  scale_y_continuous() + 
  xlab("Region") + ylab("Total visitors (in percentage)") + 
  theme(legend.position = "bottom") + 
  ggtitle("Relationship between Region and Revenue") + 
  labs(subtitle = "Revenue = 0")
Revenue_1R <- Revenue_1R %>% ggplot(aes(x = reorder(Region, -p), y = p)) + 
  geom_bar(stat = "identity", mapping = aes(fill = Region)) +
  scale_y_continuous() + 
  xlab("Region") + ylab("Total visitors (in percentage)") + 
  theme(legend.position = "bottom") + 
  ggtitle("Relationship between Region and Revenue") + 
  labs(subtitle = "Revenue = 1")
grid.arrange(Revenue_0R, Revenue_1R , nrow = 1)

##Relationship between Traffic and Revenue
Traffic_Revenue<- data.frame(table(data$TrafficType, data$Revenue))
str(Traffic_Revenue)
names(Traffic_Revenue) <- c("TrafficType", "Revenue", "Frequency")
Revenue_0T  <- Traffic_Revenue %>% filter(Revenue == 0)
Revenue_0T$p <- (Revenue_0T$Frequency / sum(Revenue_0T$Frequency)) * 100
Revenue_1T <- Traffic_Revenue%>% filter(Revenue == 1)
Revenue_1T$p <- (Revenue_1T$Frequency / sum(Revenue_1T$Frequency)) * 100

#Plotting the Relationship between Traffic and Revenue 
Revenue_0T <- Revenue_0T %>% ggplot(aes(x = reorder(TrafficType, -p), y = p)) + 
  geom_bar(stat = "identity", mapping = aes(fill = TrafficType)) + 
  scale_y_continuous() + 
  xlab("Traffic") + ylab("Total visitors (in percentage)") + 
  theme(legend.position = "bottom") + 
  ggtitle("Relationship between Traffic and Revenue") + labs(subtitle = "Revenue = 0")
Revenue_1T <- Revenue_1T %>% ggplot(aes(x = reorder(TrafficType, -p), y = p)) + 
  geom_bar(stat = "identity", mapping = aes(fill = TrafficType)) + 
  scale_y_continuous() + 
  xlab("Traffic") + ylab("Total visitors (in percentage)") + 
  theme(legend.position = "bottom") + 
  ggtitle("Relationship between Traffic and Revenue") + labs(subtitle = "Revenue = 1")
grid.arrange(Revenue_0T,Revenue_1T, nrow = 1)

### Model Preparation

#Convert variables into factors with ordinal variables
#Convert ordinal variables into numeric for models

data$Month <- factor(data$Month, order = TRUE, levels =c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'))
data$Month_numeric <- mapvalues(data$Month, from = c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'), to = c(1,2,3,4,5,6,7,8,9,10))
data$VisitorType <- factor(data$VisitorType, order = TRUE, levels = c('Returning_Visitor', 'Other', 'New_Visitor'))
data$VisitorType_Numeric <-mapvalues(data$VisitorType, from = c("Returning_Visitor", "Other", "New_Visitor"), to = c(1,2,3))
data$OperatingSystems <- factor(data$OperatingSystems, order = TRUE, levels = c(6,3,7,1,5,2,4,8))
data$Browser <- factor(data$Browser, order = TRUE, levels = c(9,3,6,7,1,2,8,11,4,5,10,13,12))
data$Region <- factor(data$Region, order = TRUE, levels = c(8,6,3,4,7,1,5,2,9))
data$TrafficType <- factor(data$TrafficType, order = TRUE, levels = c(12,15,17,18,13,19,3,9,1,6,4,14,11,10,5,2,20,8,7,16))
data$Weekend <- as.numeric(factor(data$Weekend))
# Scaling the data
scaling <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Copy the data 
scaling_data<- data
str(scaling_data)
#Scaling all the numeric variables in data
scaling_data$Administrative <- scaling(data$Administrative)
scaling_data$Administrative_Duration <- scaling(data$Administrative_Duration)
scaling_data$Informational<- scaling(data$Informational)
scaling_data$Informational_Duration<- scaling(data$Informational_Duration)
scaling_data$ProductRelated <- scaling(data$ProductRelated)
scaling_data$ProductRelated_Duration <- scaling(data$ProductRelated_Duration)
scaling_data$BounceRates <- scaling(data$BounceRates)
scaling_data$ExitRates <- scaling(data$ExitRates)
scaling_data$PageValues <- scaling(data$PageValues)
scaling_data$SpecialDay <- scaling(data$SpecialDay)

#To remove the unwanted columns for clustering models
scaling_data_cluster <- scaling_data [-c(11,16,18,19,20)]

#Splitting the data
#Splitting the data into 70:30 ratio
model_data <- data[-c(17,18,21,22)] # model_data for classification models
set.seed(777, sample.kind="Rounding")# if using R 3.5 or earlier, use `set.seed(1)`
#Data Partition
test_index <- createDataPartition(model_data$Revenue, p = 0.7, list=FALSE)
#Training set
train_data <- model_data[test_index,]
#Test set
test_data <- model_data[-test_index,]

### Model Creation
## Clustering models
str(scaling_data_cluster)
summary(scaling_data_cluster)
##Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- scaling_data_cluster
wss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#The plot above represents the variance within the clusters. 
#This bend indicates that additional clusters beyond the fourth have little value. 

# k-means clustering algorithm
#the maximum number of iterations allowed =100,number of clusters =2
# you can also check for centers = 4 too
k_means <- kmeans(scaling_data_cluster, centers = 2)
#The cluster assignments are in the cluster component
groups <- k_means$cluster
str(k_means)
#size of cluster
k_means$size
#Means
k_means$centers
#sum of squares
k_means$betweenss / k_means$totss
#confusion matrix
confusion_matrix <- table(k_means$cluster, scaling_data$Revenue)
#predictive power of the model
presicion_kmeans<- confusion_matrix [1,1]/(sum(confusion_matrix [1,]))
presicion_kmeans
recall_kmeans<- confusion_matrix [1,1]/(sum(confusion_matrix [,1]))
recall_kmeans
#F1 score
F1<- 2*presicion_kmeans*recall_kmeans/(presicion_kmeans+recall_kmeans)
F1

# To verify if the clusters were extracted correctly, pca performed
pca <- prcomp(scaling_data_cluster[c(1:10)], scale. = TRUE)
summary(pca)  #summary statistics of pca
pca_data <- as.data.frame(pca$x)

#Plotting 10 Principal Components 
plot(pca_data , main = "Principal Components")

#pc1 and pc2 wrt Revenue
data.frame(pc_1 = pca$x[,1], pc_2 = pca$x[,2], 
           Revenue = scaling_data$Revenue) %>%
  ggplot(aes(pc_1, pc_2, color = Revenue)) +
  geom_point()

#Comparison of the clusters
pca_data %>% ggplot(aes(PC1,PC2, color = k_means$cluster))+  geom_point() +ggtitle("PC1 vs PC2, K-Means Clusters")

#plot pca importance
plot(summary(pca)$importance[3,])
summary(pca)$importance[3,]

## K_means for centers = 4
k_means_4 <- kmeans(scaling_data_cluster, centers = 4)
#The cluster assignments are in the cluster component
groups_4 <- k_means_4$cluster
str(k_means_4)
#size of cluster
k_means_4$size
#Means
k_means_4$centers
#sum of squares
k_means_4$betweenss / k_means_4$totss
#confusion matrix
confusion_matrix_4 <- table(k_means_4$cluster, scaling_data$Revenue)
#predictive power of the model
presicion_kmeans_4<- confusion_matrix_4 [1,1]/(sum(confusion_matrix_4[ 1,]))
presicion_kmeans_4
recall_kmeans_4<- confusion_matrix_4[1,1]/(sum(confusion_matrix_4[,1]))
recall_kmeans_4
#F1 score
F1_4<- 2*presicion_kmeans_4*recall_kmeans_4/(presicion_kmeans_4+recall_kmeans_4)
F1_4

#clustering techniques did not observe any significant performance improvement. The plot has large amount of overlap of data.
#F1 score does not improve

##Decision Tree model
str(model_data)
set.seed(1,sample.kind="Rounding")
# Fit classification tree model
fit_dt <- rpart(Revenue_01 ~ . , data = train_data, method="class")
# Predict the test data
predict_dt<- predict(fit_dt,test_data[-18],type = "class") #remove revenue column in test for prediciton
#Accuracy
mean(predict_dt==test_data$Revenue_01)
#Decision tree
rpart.plot(fit_dt,box.palette = "RdYlGn", shadow.col = "darkgray")
#Variable importance of decision Tree
data.frame(fit_dt$variable.importance)

#Predictive power of the decision tree model
confusion_matrix_dt<- table(predict_dt,test_data$Revenue_01) 
confusion_matrix_dt
#Precision
presicion_dt<- confusion_matrix_dt[1,1]/(sum(confusion_matrix_dt[1,]))
presicion_dt
#Recall
recall_dt<- confusion_matrix_dt[1,1]/(sum(confusion_matrix_dt[,1]))
recall_dt
#F1 score
F1_dt<- 2*presicion_dt*recall_dt/(presicion_dt+recall_dt)
F1_dt

