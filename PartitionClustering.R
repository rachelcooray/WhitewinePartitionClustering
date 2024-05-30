# Partition Clustering

# Import libraries
library(readxl)
library(ggplot2) 
library(NbClust)
library(cluster)
library(factoextra)

# Load dataset
wine_data <- read_excel("Whitewine_v6.xlsx")
wine_data

# View data
head(wine_data)

# Dimensions and summary of data set
dim(wine_data)
summary(wine_data)

# Convert data to data frame
wine_dataframe <- as.data.frame(wine_data)
wine_dataframe

# Pre-processing
# Missing values
missing_values <- colSums(is.na(wine_dataframe))
missing_values

# wine_dataframe <- na.omit(wine_dataframe)

# Subtask 01
# Task A - Scaling and Outlier removal
# Visualize outliers
boxplot(wine_dataframe)

# Make a copy of the data frame before cleaning
cleaned_wine_dataframe <- wine_dataframe

# Function to handle outliers
handle_outliers <- function(dataFrame, threshold = 1.5) {
  for (i in 1:nrow(dataFrame)) {
    row <- dataFrame[i, ]
    for (column in names(dataFrame)) {
      
      # Calculate Q1, Q3, and IQR for each column
      Q1 <- quantile(dataFrame[[column]], 0.25, na.rm = TRUE)
      Q3 <- quantile(dataFrame[[column]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - threshold * IQR
      upper_bound <- Q3 + threshold * IQR
      
      if (row[[column]] < lower_bound | row[[column]] > upper_bound) {
        
        # Replace the outlier with mean of the column
        row[[column]] <- mean(dataFrame[[column]], na.rm = TRUE)
      }
    }
    dataFrame[i, ] <- row 
  }
  return(dataFrame)
}

# Removing outliers
cleaned_wine_dataframe <- handle_outliers(wine_dataframe)

# Box plot after outliers are removed
boxplot(cleaned_wine_dataframe) 

# Dimensions of data frame
dim(cleaned_wine_dataframe)

# Scale the data
scaled_data <- scale(cleaned_wine_dataframe)[, 1:11]
scaled_data

# Convert scaled data back to data frame
scaled_dataframe <- as.data.frame(scaled_data)

# 12th column (quality) from cleaned_wine_dataframe
quality_column <- cleaned_wine_dataframe$quality

# Add the quality column to the scaled data frame
scaled_dataframe <- cbind(scaled_dataframe, quality = quality_column)

# Check the structure of scaled_dataframe
str(scaled_dataframe)

# View the scaled data
head(scaled_dataframe)

# Dimensions of data frame
dim(scaled_dataframe)

# Task B - Determining Number of clusters
set.seed(26)

# NbClust method with Euclidean distance
cluster_no_euclidean <- NbClust(scaled_dataframe, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans" , index = "all")

# NbClust method with Manhattan distance
cluster_no_manhattan <- NbClust(scaled_dataframe, distance = "manhattan", min.nc = 2, max.nc = 10, method = "kmeans" , index = "all")

# Elbow method
cluster_elbow <- fviz_nbclust(scaled_dataframe, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")
cluster_elbow

# Silhouette method
cluster_silhouette <- fviz_nbclust(scaled_dataframe, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette Method")
cluster_silhouette

# Gap Statistics
cluster_gap_stat <- fviz_nbclust(scaled_dataframe, kmeans, method = 'gap_stat', k.max = 10)+
  labs(subtitle = "Gap Statistics")
cluster_gap_stat

# Task C - k-means Clustering 

# Performing k-means with K=2
most_favoured_k <- 2
kmeans_cluster <- kmeans(scaled_dataframe, most_favoured_k)

# Show kmeans output
print(kmeans_cluster)

# Cluster assignment and cluster centers
cluster_assignments <- kmeans_cluster$cluster
print("Cluster Assignments:")
print(cluster_assignments)

cluster_centers <- kmeans_cluster$centers
print("Cluster Centers:")
print(cluster_centers)

# Calculation of WSS, BSS and TSS
within_cluster_sums_of_squares <- kmeans_cluster$withinss
cat("Within-cluster sums of squares (WSS):", within_cluster_sums_of_squares, "\n")

between_cluster_sums_of_squares <- kmeans_cluster$betweenss
cat("Between-cluster sums of squares (BSS):", between_cluster_sums_of_squares, "\n")

total_sum_of_squares <- kmeans_cluster$totss
cat("Total sums of squares (TSS):", total_sum_of_squares, "\n")

BSS_over_TSS_ratio <- between_cluster_sums_of_squares/total_sum_of_squares
cat("Ratio of BSS over TSS:", BSS_over_TSS_ratio, "\n")

# Cluster plot 
cluster_plot <- fviz_cluster(kmeans_cluster, scaled_dataframe, geom = "point")
cluster_plot

# Task D - Silhouette Plot

# Calculate silhouette widths
silhouette_width <- silhouette(kmeans_cluster$cluster, dist(scaled_dataframe))
silhouette_width

# Plot silhouette plot
silhouette_plot <- fviz_silhouette(silhouette_width)
silhouette_plot

# Average Silhouette width
average_silhouette_width <- round(mean(silhouette_width[, 3]),2)
cat("Average Silhouette Width:", average_silhouette_width)


# Subtask 02
# Task E - PCA

# Perform PCA on scaled data
pca_model <- prcomp(scaled_dataframe, scale. = TRUE)
pca_model

summary(pca_model)

# Scree plot
scree_plot <- fviz_eig(pca_model, addlabels = TRUE)
scree_plot

# Get eigenvalues and eigenvectors

pca_eigen <- eigen(cor(scaled_dataframe))
pca_eigen_values <- pca_eigen$values
pca_eigen_values

pca_eigen_vectors <- pca_eigen$vectors
pca_eigen_vectors

# Calculate explained variance ratio per component
explained_variance_ratio <- pca_eigen_values / sum(pca_eigen_values)

# Cumulative explained variance ratio
cumulative_explained_variance <- cumsum(explained_variance_ratio)

# Print explained variance ratio
print("Explained variance ratio per component:")
print(explained_variance_ratio)

# Number of principal components needed to have at least 85% cumulative explained variance
num_components <- which(cumulative_explained_variance >= 0.85)[1]
num_components

# Get the required number of principal components from the PCA model
transformed_data <- data.frame(pca_model$x[, 1:num_components])
transformed_data
dim(transformed_data)
head(transformed_data)

# Task F - Finding k for PCA
set.seed(26)

# NbClust method with Euclidean distance
cluster_no_euclidean_pca <- NbClust(transformed_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans" , index = "all")

# NbClust method with Manhattan distance
cluster_no_manhattan_pca <- NbClust(transformed_data, distance = "manhattan", min.nc = 2, max.nc = 10, method = "kmeans" , index = "all")

# Elbow method
cluster_elbow_pca <- fviz_nbclust(transformed_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")
cluster_elbow_pca

# Silhouette method
cluster_silhouette_pca <- fviz_nbclust(transformed_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette Method")
cluster_silhouette_pca

# Gap Statistics
cluster_gap_stat_pca <- fviz_nbclust(transformed_data, kmeans, method = 'gap_stat', k.max = 10)+
  labs(subtitle = "Gap Statistics")
cluster_gap_stat_pca

# Task F - k-means Clustering for PCA

# Performing k-means with K=2
most_favoured_k <- 2
kmeans_cluster_pca <- kmeans(transformed_data, most_favoured_k)

# Show kmeans output
print(kmeans_cluster_pca)

# Cluster assignment and cluster centers
cluster_assignments_pca <- kmeans_cluster_pca$cluster
print("Cluster Assignments:")
print(cluster_assignments_pca)

cluster_centers_pca <- kmeans_cluster_pca$centers
print("Cluster Centers:")
print(cluster_centers_pca)

# Calculation of WSS, BSS and TSS
within_cluster_sums_of_squares_pca <- kmeans_cluster_pca$withinss
cat("Within-cluster sums of squares (WSS):", within_cluster_sums_of_squares_pca, "\n")

between_cluster_sums_of_squares_pca <- kmeans_cluster_pca$betweenss
cat("Between-cluster sums of squares (BSS):", between_cluster_sums_of_squares_pca, "\n")

total_sum_of_squares_pca <- kmeans_cluster_pca$totss
cat("Total sums of squares (TSS):", total_sum_of_squares_pca, "\n")

BSS_over_TSS_ratio_pca <- between_cluster_sums_of_squares_pca/total_sum_of_squares_pca
cat("Ratio of BSS over TSS:", BSS_over_TSS_ratio_pca, "\n")

# Cluster plot 
cluster_plot_pca <- fviz_cluster(kmeans_cluster_pca, transformed_data, geom = "point")
cluster_plot_pca

# Task H - Silhouette Plot

# Calculate silhouette widths
silhouette_width_pca <- silhouette(kmeans_cluster_pca$cluster, dist(transformed_data))

# Plot silhouette plot
silhouette_plot_pca <- fviz_silhouette(silhouette_width_pca)
silhouette_plot_pca

# Average Silhouette width
average_silhouette_width_pca <- round(mean(silhouette_width_pca[, 3]),2)
cat("Average Silhouette Width:", average_silhouette_width_pca)

# Task I - Calinski Harabasz score

library(clusterCrit)

transformed_data <- as.matrix(transformed_data)
calinski_harabasz_index <- intCriteria(transformed_data, kmeans_cluster_pca$cluster, 'Calinski_Harabasz')
calinski_harabasz_index

# Print Calinski-Harabasz Index
cat("Calinski-Harabasz Index:", calinski_harabasz_index$calinski_harabasz, "\n")


# REFERENCES
# https://stackoverflow.com/questions/56128651/calinski-harabasz-calculation-slower-in-r-clustercrit-than-python-sklearn