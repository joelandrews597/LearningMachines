pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)

library(factoextra)
library(NbClust)


par(mfrow=c(1,3))
### fviz_nbclust(cust_data_f, kmeans, method = c("silhouette", "wss", "gap_stat"))

# Elbow method 
fviz_nbclust(cust_data_f, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method 
fviz_nbclust(cust_data_f, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(cust_data_f, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")



#NbClust(data = NULL, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = NULL)

NbClust(data = cust_data_f, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")
