library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

image_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/images/"

df = as.data.frame(soybean_processor_countries_data[,-1])
rownames(df) = soybean_processor_countries_data$iso3_code

# What is the total amount of production and trade for raw and processed goods?
colSums(df)

distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Decide clusters

#df2 = df
#for(i in 1:dim(df2)[1]){
#  for(j in 1:dim(df2)[2]){
#    if(df2[i,j] != 0){
#      df2[i,j] = log(df2[i,j])
#    }
#  }
#}


k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

fviz_cluster(k2, data = df)

# Calculate the gap statistic
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 20, B = 50)

fviz_gap_stat(gap_stat)
k2 <- kmeans(df, centers = 12, nstart = 25)

str(k2)
fviz_cluster(k2, data = df)

df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         iso3_code = row.names(soybean_processor_countries_data)) %>%
  ggplot(aes(raw_production, processed_exports, color = factor(cluster), label = iso3_code)) +
  geom_text()


# PCA clustering

library(factoextra)

soybean_processor_countries_data = df[!rownames(df) %in% c("USA", "BRA", "ARG", "CHN"),]
colnames(soybean_processor_countries_data)

columns_selected = c("processed_net_exports", "raw_net_exports", "raw_production_per_capita")

df = as.data.frame(soybean_processor_countries_data[,-1] %>% 
                     select(columns_selected))
rownames(df) = soybean_processor_countries_data$iso3_code

res.pca <- prcomp(df, scale = TRUE)
#res.pca <- prcomp(df_filtered, scale = TRUE)

fviz_eig(res.pca)

# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)

ggsave(paste0(image_path, "soybean_countries_pca_plot.png"))


# Graph of variables. Positive correlated variables point to the same side of the plot.
# Negative correlated variables point to opposite sides of the graph.

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "PCA Biplot - correlation of soybean production and trade variables",  
             repel = TRUE     # Avoid text overlapping
)

ggsave(paste0(image_path, "soybean_varibles_pca_plot.png"))

# Graph the biplot
fviz_pca_biplot(res.pca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

ggsave(paste0(image_path, "soybean_pca_bi_plot.png"))


# According to the PCA biplot processed imports and raw imports are correlated. Which says that if you import
# a lot of raw soybean, you are more likely to also import processed soybean. Thinking about it it makes sense
# since the demand for the end products of soybean (which often is feed or maybe soybean oil) you might also need
# to import processed materials since you might not have processers to that can take care of all the imported
# raw materials

# On the other hand the imports are not correlated at all with production, which means that if you have high
# soybean production, you don't import raw or processed soybeans. But if you have a low production you do import
# more raw and processed soybeans.

# The exports of processed and raw soybeans are correlated with raw production. This makes sense since 


# Check without the outliers

df_filtered = df[!rownames(df) %in% c("USA", "BRA", "ARG", "CHN"),]
res.pca <- prcomp(df_filtered, scale = TRUE)
fviz_eig(res.pca)

# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)

## Clustering on some different variables ----

soybean_processor_countries_manipulated

df = as.data.frame(soybean_processor_countries_manipulated[,-1])
rownames(df) = soybean_processor_countries_manipulated$iso3_code

library(factoextra)
res.pca <- prcomp(df, scale = TRUE)
fviz_eig(res.pca)

# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)

# Graph of variables. Positive correlated variables point to the same side of the plot.
# Negative correlated variables point to opposite sides of the graph.

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "PCA Biplot - correlation of soybean production and trade variables",  
             repel = TRUE     # Avoid text overlapping
)

## Look at more detailed trade


df = as.data.frame(soybean_commodities_summarised[,-1])
rownames(df) = soybean_commodities_summarised$iso3_code

# Try by removing the most influental ones
df_filtered = df[!rownames(df) %in% c("USA", "BRA", "ARG", "CHN", "IND"),]

res.pca <- prcomp(df, scale = TRUE)
#res.pca <- prcomp(df_filtered, scale = TRUE)
fviz_eig(res.pca)

# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)

# Graph of variables. Positive correlated variables point to the same side of the plot.
# Negative correlated variables point to opposite sides of the graph.

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "PCA Biplot - correlation of soybean production and trade variables",  
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

## PCA clustering on manipulated raw vs processing data

df = as.data.frame(soybean_processor_countries_manipulated[,-1])
rownames(df) = soybean_processor_countries_manipulated$iso3_code

# Try by removing the most influental ones
df_filtered = df[!rownames(df) %in% c("USA", "BRA", "ARG", "CHN", "IND"),]

res.pca <- prcomp(df, scale = TRUE)
#res.pca <- prcomp(df_filtered, scale = TRUE)
fviz_eig(res.pca)

# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)

# Graph of variables. Positive correlated variables point to the same side of the plot.
# Negative correlated variables point to opposite sides of the graph.

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "PCA Biplot - correlation of soybean production and trade variables",  
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

