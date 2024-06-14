library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)

###CSV/Dataframe is called Hitters_Savant

MLB_Name <- Hitters_Savant$Name 

MLB_PCA <- Hitters_Savant |> 
  select(-Name)

rownames(MLB_PCA) <- MLB_Name

MLB_PCA <- prcomp(MLB_PCA, center = TRUE, scale = TRUE)

get_eigenvalue(MLB_PCA)

fviz_eig(MLB_PCA, addlabels = TRUE) +
  xlab("Principal Component") +
  ylab("% of Variance Explained") +
  labs(title = "**PCA Analysis: Scree Plot**")

pc1 <- fviz_contrib(MLB_PCA, choice = "var", axes = 1)
pc2 <- fviz_contrib(MLB_PCA, choice = "var", axes = 2)
pc3 <- fviz_contrib(MLB_PCA, choice = "var", axes = 3)

plot_grid(pc1, pc2, pc3)

k <- 3

pca_scores <- MLB_PCA$x

set.seed(1928)
MLB_kmeans <- kmeans(pca_scores, centers = k)
MLB_kmeans$cluster

cluster_assignment <- MLB_kmeans$cluster

Hitters_Savant$cluster <- cluster_assignment

kmean_dataviz <- Hitters_Savant

kmean_dataviz <- kmean_dataviz %>%
  mutate(cluster = case_when(
    cluster == 1 ~ "Cluster 1",
    cluster == 2 ~ "Cluster 2",
    cluster == 3 ~ "Cluster 3"))

kmean_data_long <- kmean_dataviz %>%
  gather("Variable", "Value", -Name, -cluster)

ggplot(kmean_data_long, aes(x = Variable, y = Value, color = cluster)) +
  geom_point(size = 3) +
  facet_wrap(~ cluster) +
  scale_color_brewer(palette = "Set1") +
  gghighlight(use_direct_label = FALSE) +
  theme(axis.text = element_text(angle = 90, size = 8),
        strip.text = element_text(face = "bold"),
        legend.position = "none")

Cluster_1 <- Hitters_Savant |> 
  filter(cluster == 1)

Cluster_2 <- Hitters_Savant |> 
  filter(cluster == 2)

Cluster_3 <- Hitters_Savant |> 
  filter(cluster == 3)

Hitters_Savant$cluster <- as.factor(cluster_assignment)

fviz_cluster(MLB_kmeans, data = pca_scores,
             geom = "point", ellipse.type = "convex",
             ggtheme = theme_minimal())

pca_scores_df <- as.data.frame(pca_scores)
pca_scores_df$cluster <- as.factor(cluster_assignment)
pca_scores_df$Name <- rownames(pca_scores_df)

ggplot(pca_scores_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2) +
  geom_text(aes(label = Name), size = 2, vjust = 1, hjust = 1) +
  stat_ellipse(type = "convex", linetype = 2) +
  labs(title = "Cluster plot",
       x = paste("Dim1 (", round(MLB_PCA$sdev[1]^2 / sum(MLB_PCA$sdev^2) * 100, 1), "%)", sep = ""),
       y = paste("Dim2 (", round(MLB_PCA$sdev[2]^2 / sum(MLB_PCA$sdev^2) * 100, 1), "%)", sep = "")) +
  theme_minimal()
