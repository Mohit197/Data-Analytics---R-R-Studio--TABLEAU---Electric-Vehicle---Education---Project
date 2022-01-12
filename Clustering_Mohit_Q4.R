#Upload and Read Breast Cancer data:
library(readxl)
library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)

library(rpart)
library(rpart.plot)
library(party)
library(partykit)
library(corrr)
library(corrplot)
library(tidyverse)
library(modelr)

library(readxl)
EV_Range_Data= read_excel("C:\\Users\\mohit\\Dropbox\\PhD Rutgers University\\Fall 2021 Course Material\\Data Analytics Engineering Systems\\Project\\Question 4\\EV_range_v2.xlsx", sheet = "EV_Range") 
#Upload the data

#Check data file
head(EV_Range_Data)
str(EV_Range_Data)

library(dendextend)
library(factoextra)
library(cluster)

#K-means 
EV_df <- data.frame(EV_Range_Data)
head(EV_df)
str(EV_df)
EV_df$Model_Year <- as.numeric(EV_df$Model_Year)          # First column is a number
EV_df$Make <- as.numeric(as.factor(EV_df$Make))     # Second column is a character
EV_df$Model <- as.numeric(as.factor(EV_df$Model))    # Third column is an integer
EV_df$class <- as.numeric(as.factor(EV_df$class))
#EV_df$fuel_type <- as.numeric(as.factor(EV_df$fuel_type)) 
EV_df$Range <- as.numeric(EV_df$Range) 

EV_df

EV_df_scale <- scale(EV_df) # Scaling the data
EV_df_scale


head(EV_df_scale, n = 3)
library(factoextra)


kosDist1 = dist(EV_df_scale, method="euclidean")
kosClust1 = hclust(kosDist1, method="ward.D")

#Plot the dendrogram
plot(kosClust1)

#Compute k-means with k = 4
set.seed(1000)
#KmeansCluster = kmeans(EV_df_scale, centers=6)
KmeansCluster <- kmeans(EV_df_scale, 5, nstart = 100)
summary(KmeansCluster)
KmeansCluster

#set.seed(123)
#km.res <- kmeans(na.omit(EV_df), 4, nstart = 25)
#summary(km.res)

# Fancy kmeans
# plot the clusters
fviz_cluster(KmeansCluster, data = EV_df_scale, geom = c("point"),ellipse.type = "euclid")


#Another method - scaled data
# load required packages
library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(EV_df_scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle


# Silhouette method
fviz_nbclust(EV_df_scale, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")


# Gap statistic
set.seed(42)
fviz_nbclust(EV_df_scale, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500 # reduce it for lower computation time (but less precise results)
) +
  labs(subtitle = "Gap statistic method")


library(parameters)

n_clust <- n_clusters(EV_df_scale,
                      package = c("easystats", "NbClust", "mclust"),
                      standardize = FALSE)
n_clust



library(cluster)
set.seed(42)
km_res <- kmeans(EV_df_scale, centers = 2, nstart = 100)

sil <- silhouette(km_res$cluster, dist(EV_df_scale))
fviz_silhouette(sil)

#plot results of final k-means model
fviz_cluster(km_res, data = EV_df_scale)


library(factoextra)

fviz_cluster(km_res, EV_df_scale, ellipse.type = "norm")


fviz_cluster(km_res, data = EV_df_scale,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

table(km_res$cluster)


# Dimension reduction using PCA
res.pca <- prcomp(EV_df_scale,  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(km_res$cluster)
# Add Species groups from the original data sett
ind.coord$Range <- EV_df[['Range']]
# Data inspection
head(ind.coord)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

Dim.11 = eigenvalue[1] + eigenvalue[2] + eigenvalue[3]


#the below code is for the KNN clustering plot
#The 'class' model is the legend for the KNN model
#Need to fix how to add 'class'legends on the side of the plot

plot1 = ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = EV_df[['class']], size = 1.5,  legend ="right", ggtheme = theme_bw(),
  xlab = paste0("Dim 4 (", variance.percent[1] + variance.percent[2] + variance.percent[3] + variance.percent[4] , "% )" ),
  ylab = paste0("Dim 5 (", variance.percent[5], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4) + geom_point(shape = EV_df[['class']])


#add a legend
leg = plot1.ax.legend(EV_df[['class']], title="label", fancybox=True)

#Executive -CAR, CAR - SUV and Compact Car from Chevrolet form one cluster (red-cluster 1)
#thest graph tells us we have two distinct classes of vehicles for consumers to choose from
# 



