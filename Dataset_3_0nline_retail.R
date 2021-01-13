#Import any necessary packages
library(readr)
library(ggplot2)
library(readxl)    #For reading excel files   
library(dplyr)     #For data wrangling
library(lubridate) #For handling dates
library(ggplot2)   #For plotting of results

#Read in the data
retail <- read_xlsx("C:\\Users\\kupekarraj\\Desktop\\DMML project\\Online Retail.xlsx")
#Convert to dataframe
retail = data.frame(retail)
#Preview the data
head(retail)
dim(retail)
# look for missing values using the DataExplorer package
sum(is.na(retail$InvoiceNo))
sum(is.na(retail$StockCode))
sum(is.na(retail$Description))
sum(is.na(retail$Quantity))
sum(is.na(retail$UnitPrice))
sum(is.na(retail$CustomerID))
sum(is.na(retail$Country))
sum(is.na(retail$InvoiceDate))
sum(is.na(retail))
#alldata <- na.omit(retail)
sum(is.na(retail))
dim(retail)
View(retail)


#no of false transaction on purchased item using customer id 
sum(is.na(retail$CustomerID))
#Answer: 135,080 transactions
#We will exclude these transactions, as we are only interested in monitoring user behaviour
retail <- retail %>%
  filter(!is.na(CustomerID))
#Ensure columns are of correct types
str(alldata)
#Count number of unique items purchased by each customer
uniqueitems <- retail %>%
  group_by(CustomerID) %>%
  summarise(unique_items_customer = n_distinct(StockCode))
uniqueitems

#Group the data at an invoice level
invoicelvl <- retail %>%
  group_by(CustomerID, InvoiceNo) %>%
  summarise(unique_items_inv = n(),
            quantity_purchased = sum(Quantity),
            total_price = sum(Quantity*UnitPrice),
            invoice_date = first(InvoiceDate)) %>%
  arrange(CustomerID,invoice_date)
#Join the two arrays together
combineddata <- left_join(invoicelvl, uniqueitems, by='CustomerID')

#Group the data at a customer level
customerlvl <- combineddata %>%
  group_by(CustomerID) %>%
  summarise(no_orders = n(),
            unique_items_purchased = min(unique_items_customer),
            quantity_items_purchased = sum(quantity_purchased),
            average_quantity_per_order = mean(quantity_purchased),
            total_money_spent = sum(total_price),
            average_spent_per_order = mean(total_price))
summary(customerlvl)
# the max value in total money spent if we apply k mean method baisness we will create as i will cluster the customers based on total money spent which not in our case
# first we will scale the data 
#Scale the Data
km_data <- scale(customerlvl[2:7])

summary(km_data)
#next step is to check multicollinearity
#Print correlation matrix
print(cor(km_data))
# the cor between total money spent and quantity item purchased shows high correlation
# we will exclude the total money spent as quantity item purchased column seems to be more significant for forming clustering of customers
#varImp(km_data)
#Remove the 5th column & scale
km_data_reduced <- scale(customerlvl[c(2:5,7)])

summary(km_data_reduced)
km_data.pca <- prcomp(km_data_reduced, center = T,scale = T)
#Summary of Principal Components
summary(km_data.pca)

#Proportion of variance explained:
var <- km_data.pca$sdev^2
pve <- var/sum(var)
#Scree Plot of Cumulative Variance Explained:
g <- qplot(x = 1:5, y = cumsum(pve), geom = 'line', xlab="Number of Principal Components", ylab='Proportion of Variance', main="Scree Plot")

g + scale_x_continuous(breaks = seq(0, 5, by = 1))
#Remove randomness from iterations by setting a seed
set.seed(123)


#Determine the maximum number of clusters
#k_max = 10 means we will test all possible numbers of clusters from 1 to 10 to see which performs the best.
k_max <- 10

#Calculate the total within sum of squares for each of 1:k_max
wcss <- sapply(1:k_max, function(k){kmeans(km_data_reduced, k)$tot.withinss})
#Vizualize the results
g <- qplot(x = 1:k_max, y = wcss, geom = 'line', xlab="Number of Clusters", ylab='WCSS', main="Number of Clusters")
g + scale_x_continuous(breaks = seq(0, 10, by = 1))
# k-means clustering
km <-kmeans(km_data_reduced, centers = 5, nstart=20)

#Check total within sum of squares with a variety of nstart and iter.max values
#to ensure algorithm convergence
km$tot.withinss
#What are the characteristics of the 5 groups?
km$centers
#How many users belong to each group?
table(km$cluster)
  ## plot the dataset with clusters
#par(pty="m")
#pairs(km_data, col = km$cluster, main="K-Means with 5 Clusters", lower.panel=NULL,
 #     xaxt="n",yaxt="n", oma=c(0,0,5,0))


km_data_reduced$c
data.orig = t(apply(km$centers, 1, function(r)r*attr(km_data_reduced,'scaled:scale') + attr(km_data_reduced, 'scaled:center')))
print(data.orig)
library(ggpubr)
library(factoextra)

fviz_cluster(km_data_reduced, data = alldata,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())
library(cluster)
clusplot(km_data_reduced,km$cluster , color=TRUE, shade=TRUE,
         labels=2, lines=0)

dim(km)
fviz_cluster(km,km_data_reduced)
fviz_cluster(km, geom = "point",  data = km_data_reduced,) + ggtitle("k = 5")

km$cluster
