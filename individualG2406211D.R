teens <- read.csv("snsdata.csv")
#take a quick look at the specifics of the data
getwd()
str(teens)
table(teens$gender)
table(teens$gender, useNA = "ifany")
summary(teens$age)
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)
summary(teens$age)
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")
mean(teens$age)
mean(teens$age, na.rm = TRUE)
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)
ave_age <- ave(teens$age, teens$gradyear, FUN =
                 function(x) mean(x, na.rm = TRUE))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)
library(stats)
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))
set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)
teen_clusters$size
teen_clusters$centers
teens$cluster <- teen_clusters$cluster
teens[1:5, c("cluster", "gender", "age", "friends")]
aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)








#########################################################
library(tidyverse)
library(cluster)
library(factoextra)
library(Rmixmod)
library(mlr)
library(gridExtra)
library(plotrix)
library(NbClust)
library(plot3D)
install.packages("NbClust")
install.packages("plot3D")
mall_customers <- read.csv("C:/Users/94000/Desktop/analytic software/archive/Mall_Customers.csv")


str(mall_customers)
names(mall_customers)
head(mall_customers)
summary(mall_customers$Age)
sd(mall_customers$Age)
summary(mall_customers$Annual.Income..k..)
sd(mall_customers$Annual.Income..k..)
summary(mall_customers$Spending.Score..1.100.)
sd(mall_customers$Spending.Score..1.100.)
# Create a 3D scatter plot of Age, Annual Income, and Spending Score
scatter3D(x = mall_customers$Age, 
          y = mall_customers$Annual.Income..k.., 
          z = mall_customers$Spending.Score..1.100.,
          pch = 18, colvar = mall_customers$Spending.Score..1.100.,
          col = jet.col(100), clab = "Spending Score",
          xlab = "Age", ylab = "Annual Income (k$)", zlab = "Spending Score")




gender_counts <- table(mall_customers$Gender)

gender_counts
barplot(gender_counts, 
        main = "Gender Distribution",
        ylab = "Count",
        xlab = "Gender",
        col = c("#FF9999", "#66B2FF"),
        legend.text = rownames(gender_counts),
        args.legend = list(x = "topright"))

gender_percentages <- round(gender_counts / sum(gender_counts) * 100)
gender_labels <- paste(names(gender_counts), "\n", gender_percentages, "%", sep = "")
gender_percentages
pie3D(gender_counts, 
      labels = gender_labels,
      main = "Gender Ratio",
      col = c("#FF9999", "#66B2FF"),
      explode = 0.1)


par(mfrow = c(2, 2))

hist(mall_customers$Age, 
     col = "#66B2FF", 
     main = "Age Distribution",
     xlab = "Age",
     ylab = "Frequency",
     border = "white")

hist(mall_customers$Annual.Income..k.., 
     col = "#FF9999", 
     main = "Annual Income Distribution",
     xlab = "Annual Income (k$)",
     ylab = "Frequency",
     border = "white")

plot(density(mall_customers$Annual.Income..k..), 
     col = "black", 
     main = "Annual Income Density",
     xlab = "Annual Income (k$)",
     ylab = "Density")
polygon(density(mall_customers$Annual.Income..k..), 
        col = "#FFCC99",
        border = "black")

hist(mall_customers$Spending.Score..1.100., 
     main = "Spending Score Distribution",
     xlab = "Spending Score",
     ylab = "Frequency",
     col = "#99CC99",
     border = "white")

par(mfrow = c(1, 1))

# Perform Principal Component Analysis (PCA) on columns 3 to 5 of mall_customers data
pca_result <- prcomp(mall_customers[, 3:5], scale = TRUE)
summary(pca_result)
pca_result$rotation[, 1:2]


set.seed(2024)
wss <- function(k) {
  kmeans(mall_customers[, 3:5], k, iter.max = 100, nstart = 100, algorithm = "Lloyd")$tot.withinss
}
k_values <- 1:10
wss_values <- map_dbl(k_values, wss)

wss_values

ggplot(data.frame(k = k_values, wss = wss_values), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of clusters (k)",
       y = "Total within-cluster sum of squares",
       title = "Elbow Method for Optimal k") +
  theme_minimal()


silhouette_scores <- function(k) {
  km <- kmeans(mall_customers[, 3:5], k, iter.max = 100, nstart = 50, algorithm = "Lloyd")
  ss <- silhouette(km$cluster, dist(mall_customers[, 3:5], "euclidean"))
  mean(ss[, 3])
}

silhouette_scores

silhouette_values <- sapply(2:7, silhouette_scores)

silhouette_values

ggplot(data.frame(k = 2:7, score = silhouette_values), aes(x = k, y = score)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of clusters (k)",
       y = "Average Silhouette Score",
       title = "Silhouette Analysis for Optimal k") +
  theme_minimal()


gap_stat <- clusGap(mall_customers[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat) +
  labs(subtitle = "Gap Statistic Method") +
  theme_minimal()

# Perform k-means clustering with 6 clusters on columns 3 to 5 of mall_customers data
final_kmeans <- kmeans(mall_customers[, 3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")

final_kmeans
# Add the cluster assignments to the mall_customers data as a factor variable
mall_customers$Cluster <- as.factor(final_kmeans$cluster)

mall_customers$Cluster

ggplot(mall_customers, aes(x = Annual.Income..k.., y = Spending.Score..1.100., color = Cluster)) + 
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Mall Customer Segments",
       subtitle = "Annual Income vs Spending Score",
       x = "Annual Income (k$)",
       y = "Spending Score") +
  theme_minimal()

ggplot(mall_customers, aes(x = Age, y = Spending.Score..1.100., color = Cluster)) + 
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Mall Customer Segments",
       subtitle = "Age vs Spending Score",
       x = "Age",
       y = "Spending Score") +
  theme_minimal()

ggplot(mall_customers, aes(x = Age, y = Annual.Income..k.., color = Cluster)) + 
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Mall Customer Segments",
       subtitle = "Age vs Annual Income",
       x = "Age",
       y = "Annual Income (k$)") +
  theme_minimal()




pca_data <- as.data.frame(pca_result$x[, 1:2])
pca_data$Cluster <- mall_customers$Cluster
# Create a scatter plot of Annual Income vs Spending Score, colored by cluster
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Mall Customer Segments - PCA",
       x = "First Principal Component",
       y = "Second Principal Component") +
  theme_minimal()
fviz_cluster(final_kmeans,data=mall_customers[,3:5])
aggregate(mall_customers[,3:5],by=list(cluster=final_kmeans$cluster),mean)


