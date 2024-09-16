这个数据集是为了学习客户细分（市场篮子分析）概念而创建的。数据集包含了超市购物中心会员卡收集的基本客户信息，包括：

客户 ID: 唯一标识每个客户的编号。
年龄: 客户的年龄。
性别: 客户的性别，分为男性和女性。
年收入（千美元）: 客户的年收入。
消费评分（1-100）: 商家根据客户行为和购买数据等因素赋予客户的评分，用于衡量客户的消费能力和潜力。
数据集用途:

这个数据集可以用于学习无监督机器学习技术，特别是 K-means 聚类算法，并将其应用于客户细分分析。通过将客户划分为不同的群体，商家可以更好地理解客户特征，并根据不同群体的特点制定更有针对性的营销策略，例如：

目标客户: 识别容易转化的客户群体，并将信息传递给营销团队，以便制定相应的策略。
客户忠诚度: 维护现有客户的满意度，并提高他们的消费意愿。
个性化推荐: 根据不同客户群体的特征，提供个性化的产品推荐和优惠活动。

 str(mall_customers)
'data.frame':	200 obs. of  5 variables:
 $ CustomerID            : int  1 2 3 4 5 6 7 8 9 10 ...
 $ Gender                : chr  "Male" "Male" "Female" "Female" ...
 $ Age                   : int  19 21 20 23 31 22 35 23 64 30 ...
 $ Annual.Income..k..    : int  15 15 16 16 17 17 18 18 19 19 ...
 $ Spending.Score..1.100.: int  39 81 6 77 40 76 6 94 3 72 ...
> names(mall_customers)
[1] "CustomerID"             "Gender"                 "Age"                    "Annual.Income..k.."    
[5] "Spending.Score..1.100."
> head(mall_customers)
  CustomerID Gender Age Annual.Income..k.. Spending.Score..1.100.
1          1   Male  19                 15                     39
2          2   Male  21                 15                     81
3          3 Female  20                 16                      6
4          4 Female  23                 16                     77
5          5 Female  31                 17                     40
6          6 Female  22                 17                     76
> summary(mall_customers$Age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  18.00   28.75   36.00   38.85   49.00   70.00 
> sd(mall_customers$Age)
[1] 13.96901
> summary(mall_customers$Annual.Income..k..)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.00   41.50   61.50   60.56   78.00  137.00 
> sd(mall_customers$Annual.Income..k..)
[1] 26.26472
> summary(mall_customers$Spending.Score..1.100.)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   34.75   50.00   50.20   73.00   99.00 
> sd(mall_customers$Spending.Score..1.100.)
[1] 25.82352
> gender_counts <- table(mall_customers$Gender)
> gender_counts

Female   Male 
   112     88 
> pca_result <- prcomp(mall_customers[, 3:5], scale = TRUE)
> summary(pca_result)
Importance of components:
                          PC1    PC2    PC3
Standard deviation     1.1524 0.9996 0.8202
Proportion of Variance 0.4427 0.3331 0.2243
Cumulative Proportion  0.4427 0.7758 1.0000
> pca_result$rotation[, 1:2]
                               PC1         PC2
Age                     0.70638235 -0.03014116
Annual.Income..k..     -0.04802398 -0.99883160
Spending.Score..1.100. -0.70619946  0.03777499
> set.seed(2024)
> wss <- function(k) {
+   kmeans(mall_customers[, 3:5], k, iter.max = 100, nstart = 100, algorithm = "Lloyd")$tot.withinss
+ }
> k_values <- 1:10
> wss_values <- map_dbl(k_values, wss)
> wss_values
 [1] 308812.78 212840.17 143342.75 104366.15  75350.78  58300.44  51084.51  44309.84  40875.02  37175.40
> silhouette_scores <- function(k) {
+   km <- kmeans(mall_customers[, 3:5], k, iter.max = 100, nstart = 50, algorithm = "Lloyd")
+   ss <- silhouette(km$cluster, dist(mall_customers[, 3:5], "euclidean"))
+   mean(ss[, 3])
+ }
> silhouette_scores
function(k) {
  km <- kmeans(mall_customers[, 3:5], k, iter.max = 100, nstart = 50, algorithm = "Lloyd")
  ss <- silhouette(km$cluster, dist(mall_customers[, 3:5], "euclidean"))
  mean(ss[, 3])
}
> silhouette_values <- sapply(2:7, silhouette_scores)
> silhouette_values
[1] 0.2931661 0.3839350 0.4054630 0.4442860 0.4523444 0.4412552
> gap_stat <- clusGap(mall_customers[,3:5], FUN = kmeans, nstart = 25,
+                     K.max = 10, B = 50)
Clustering k = 1,2,..., K.max (= 10): .. done
Bootstrapping, b = 1,2,..., B (= 50)  [one "." per sample]:
.................................................. 50 
> print(gap_stat, method = "firstmax")
Clustering Gap statistic ["clusGap"] from call:
clusGap(x = mall_customers[, 3:5], FUNcluster = kmeans, K.max = 10,     B = 50, nstart = 25)
B=50 simulated reference sets, k = 1..10; spaceH0="scaledPCA"
 --> Number of clusters (method 'firstmax'): 1
          logW   E.logW       gap     SE.sim
 [1,] 7.829990 8.257105 0.4271147 0.01969881
 [2,] 7.625794 8.018642 0.3928485 0.01769773
 [3,] 7.417921 7.825589 0.4076680 0.01724758
 [4,] 7.256540 7.677794 0.4212541 0.01841949
 [5,] 7.104745 7.596225 0.4914803 0.01706567
 [6,] 6.965334 7.525676 0.5603412 0.01672589
 [7,] 6.903828 7.464702 0.5608737 0.01668516
 [8,] 6.847482 7.409614 0.5621320 0.01745838
 [9,] 6.806305 7.360984 0.5546786 0.01790466
[10,] 6.757558 7.316995 0.5594375 0.01875953
> fviz_gap_stat(gap_stat) +
+   labs(subtitle = "Gap Statistic Method") +
+   theme_minimal()
> final_kmeans <- kmeans(mall_customers[, 3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")
> final_kmeans
K-means clustering with 6 clusters of sizes 22, 44, 38, 35, 22, 39

Cluster means:
       Age Annual.Income..k.. Spending.Score..1.100.
1 25.27273           25.72727               79.36364
2 56.34091           53.70455               49.38636
3 27.00000           56.65789               49.13158
4 41.68571           88.22857               17.28571
5 44.31818           25.77273               20.27273
6 32.69231           86.53846               82.12821

Clustering vector:
  [1] 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 2 1 5 3 5 1 2 3 3 3 2 3 3 2 2 2
 [57] 2 2 3 2 2 3 2 2 2 3 2 2 3 3 2 2 2 2 2 3 2 3 3 2 2 3 2 2 3 2 2 3 3 2 2 3 2 3 3 3 2 3 2 3 3 2 2 3 2 3 2 2 2 2 2 3
[113] 3 3 3 3 2 2 2 2 3 3 3 6 3 6 4 6 4 6 4 6 3 6 4 6 4 6 4 6 4 6 3 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6
[169] 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6

Within cluster sum of squares by cluster:
[1]  4099.818  7607.477  7742.895 16690.857  8189.000 13972.359
 (between_SS / total_SS =  81.1 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"        
[8] "iter"         "ifault"      
> mall_customers$Cluster <- as.factor(final_kmeans$cluster)
> mall_customers$Cluster
  [1] 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 5 1 2 1 5 3 5 1 2 3 3 3 2 3 3 2 2 2
 [57] 2 2 3 2 2 3 2 2 2 3 2 2 3 3 2 2 2 2 2 3 2 3 3 2 2 3 2 2 3 2 2 3 3 2 2 3 2 3 3 3 2 3 2 3 3 2 2 3 2 3 2 2 2 2 2 3
[113] 3 3 3 3 2 2 2 2 3 3 3 6 3 6 4 6 4 6 4 6 3 6 4 6 4 6 4 6 4 6 3 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6
[169] 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6 4 6
Levels: 1 2 3 4 5 6
> pca_data <- as.data.frame(pca_result$x[, 1:2])
> pca_data$Cluster <- mall_customers$Cluster
> fviz_cluster(final_kmeans,data=mall_customers[,3:5])
> aggregate(mall_customers[,3:5],by=list(cluster=final_kmeans$cluster),mean)
  cluster      Age Annual.Income..k.. Spending.Score..1.100.
1       1 25.27273           25.72727               79.36364
2       2 56.34091           53.70455               49.38636
3       3 27.00000           56.65789               49.13158
4       4 41.68571           88.22857               17.28571
5       5 44.31818           25.77273               20.27273
6       6 32.69231           86.53846               82.12821
