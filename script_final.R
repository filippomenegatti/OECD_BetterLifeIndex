library(esquisse)
library(factoextra)
library(NbClust)
library(readr)
library(FactoMineR)
library(ggplot2)
library(ggcorrplot)
library(ggplot2)

OECD <- read_csv("OECDBLI2017cleanedcsv.csv")
names <- OECD$Country
OECD$Country <- NULL
rownames(OECD) <- names
colnames(OECD) <- gsub(" ", "_", colnames(OECD))
colnames(OECD) <- c("no_basic_facilities_pct" ,
                    "Housing_expenditure_pct",
                    "Rooms_per_person_rat",
                    "net_disp_income_usd",
                    "net_finan_wealth_usd",
                    "Labour_mkt_insecurity_pct",
                    "Employment_rate_pct",
                    "LT_unemployment_pct",
                    "Personal_earnings_usd",
                    "Quality_sup_net_pct",
                    "Educational_attainment_pct",
                    "Student_skills_avg_score",
                    "Years_in_education_yrs",
                    "Air_pollution_ugm3",
                    "Water_quality_pct",
                    "SH_engag_develop_regulat_avg_score",
                    "Voter_turnout_pct",
                    "Life_expectancy_yrs",
                    "Self-reported_health_pct",
                    "Life_satisfaction_avg_score",
                    "Feel_safe_walk_alone_night_pct",
                    "Homicide_rate_rat",
                    "long_hours_work_pct",
                    "leisure_time_hrs")

scaled_OECD <- scale(OECD) #scale the variables of the model

ggcorrplot(cor(OECD), lab = FALSE, 
           title = 'Pearson Correlation Matrix', 
           ggtheme = 'theme_void') 

pca <- princomp(scaled_OECD)

print(matrix(OECD['South Africa' ,]))

fviz_eig(pca)

comp <- data.frame(pca$scores[,1:2])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

fviz_pca_ind(pca,
             col.ind = "contrib", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


# Eigenvalues
eig.val <- get_eigenvalue(pca)
eig.val

# Results for Variables
res.var <- get_pca_var(pca)
res.var$coord
res.var$contrib
res.var$cos2

# Results for individuals
res.ind <- get_pca_ind(pca)
res.ind$coord
res.ind$contrib
res.ind$cos2

fviz_nbclust(scaled_OECD, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(scaled_OECD, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

set.seed(33)
fviz_nbclust(scaled_OECD, kmeans, method = "gap_stat")+
  labs(subtitle = "Gap statistic method")

sugg_clust <- NbClust(data = scaled_OECD, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = 'kmeans')
sugg_clust

KM <- kmeans(scaled_OECD, centers = 3, iter.max = 10, nstart = 25)

fviz_cluster(KM, data = scaled_OECD,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

res.pca <- PCA(OECD, ncp = 2, graph = FALSE, scale.unit = TRUE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE, nb.clust = -1, method = 'average')

Cluster_1 <- res.hcpc$desc.var$quanti$`1`[, 'Mean in category']
Cluster_2 <- res.hcpc$desc.var$quanti$`2`[, 'Mean in category']
Cluster_3 <- res.hcpc$desc.var$quanti$`3`[, 'Mean in category']
Cluster_4 <- res.hcpc$desc.var$quanti$`4`[, 'Mean in category']

Cluster_1 <- data.frame(Cluster_1)
Cluster_2 <- data.frame(Cluster_2)
Cluster_3 <- data.frame(Cluster_3)
Cluster_4 <- data.frame(Cluster_4)

attach(res.hcpc$data.clust)

clusters <- res.hcpc$data.clust[order(clust) ,]

clust_data = cbind(rownames(clusters), clusters$clust)

fviz_dend(res.hcpc,
          cex = 0.9,
          palette = "jco",
          rect = TRUE, rect_fill = TRUE,
          rect_border = "jco",
          labels_track_height = 0.8)

fviz_cluster(res.hcpc,
             repel = TRUE,
             cex = 0.6,
             show.clust.cent = TRUE,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "Factor map")


