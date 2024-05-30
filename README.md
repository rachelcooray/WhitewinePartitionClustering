# White Wine Quality Clustering Project

## Overview
This project performs clustering on a white wine quality dataset using various clustering methods and pre-processing techniques. The goal is to find meaningful clusters within the dataset that can help in understanding the characteristics of different wine qualities.

## Dataset
The dataset used in this project is Whitewine_v6.xlsx, which contains various chemical properties of white wine and their respective quality ratings.

## Libraries Used
readxl: For reading Excel files.
ggplot2: For data visualization.
NbClust: For determining the optimal number of clusters.
cluster: For clustering algorithms and silhouette analysis.
factoextra: For visualizing clustering results.
clusterCrit: For calculating clustering indices like Calinski-Harabasz.

## Steps in the Project
### Load the Dataset

Read the dataset from an Excel file.
View and summarize the data.

### Pre-processing

Handle missing values.
Visualize and remove outliers using a custom function.
Scale the data.

### Clustering Analysis

Determine the optimal number of clusters using various methods: NbClust with Euclidean and Manhattan distances, Elbow method, Silhouette method, and Gap Statistics.
Perform k-means clustering with the most favored number of clusters.
Analyze and visualize clustering results.

### Principal Component Analysis (PCA)

Perform PCA on the scaled data.
Determine the number of principal components needed to explain at least 85% of the variance.
Re-run clustering analysis on the PCA-transformed data.

### Cluster Evaluation

Evaluate clustering performance using silhouette plots and Calinski-Harabasz index.
