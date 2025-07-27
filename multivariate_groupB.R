# ─── 0) PACKAGES & DATA PREP ─────────────────────────────────────────────────────
# install.packages(c("tidyverse","vegan","factoextra","randomForest"))
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("ggtree")

library(tidyverse)
library(vegan)       # decostand(), rda(), adonis2()
library(factoextra)  # fviz_pca_biplot(), fviz_dend()
library(randomForest)
library(ggtree)      # for nicer dendrogram plotting (optional)

# assume your data is in a data.frame called `df`
# and has at least:
#   CountryResponsible | PlasticBags | PlasticBottles | ... | LocalWaste

# Replace the NA values for 0. --> Probably the default behaviour?
df <- GroupB_data_validated %>% 
  select(CountryResponsible, PlasticBags:LocalWaste) %>% 
  #drop_na()
  mutate_all(funs(replace_na(.,0)))

# isolate metadata + community matrix
meta <- df %>% select(CountryResponsible)
comm <- df %>% select(-CountryResponsible)

# Hellinger‐transform community data (recommended for PCA/cluster/PERMANOVA)
comm_hel <- decostand(comm, method = "hellinger")

# ─── 1) PCA (and optional RDA constrained by Country) ───────────────────────────
# 1a) Unconstrained PCA on Hellinger data
pca1 <- rda(comm_hel)  

summary(pca1)

plot(pca1)

# (re)compute PCA with prcomp
pca_prcomp <- prcomp(comm_hel, scale. = TRUE)

summary(pca_prcomp)

# 1b) Quick biplot via factoextra
fviz_pca_biplot(pca_prcomp,
                geom.ind    = "point",            # show sites as points
                col.ind     = meta$CountryResponsible %>% str_to_title(), 
                palette.ind = "jco",              # color‐blind‐friendly palette
                addEllipses = FALSE,               # 95% CI per country
                label      = "var",               # show arrows for variables
                repel      = TRUE
) +
  labs(title = "PCA of Litter Composition (Hellinger‐transformed)",
       subtitle = "Points = samples colored by Country; arrows = litter types") +
  theme_minimal(base_size = 14)

# Bar‐plot of top 10 contributors to PC1
fviz_contrib(pca_prcomp, choice = "var", axes = 1, top = 10) +
  ggtitle("Top 10 variables contributing to PC1")

# Bar‐plot of top 10 contributors to PC2
fviz_contrib(pca_prcomp, choice = "var", axes = 2, top = 10) +
  ggtitle("Top 10 variables contributing to PC2")


# Get a table of contributions
var_contrib <- get_pca_var(pca_prcomp)$contrib  # a matrix

# Turn into a tibble for easy viewing
contrib_tbl <- as_tibble(var_contrib, rownames = "variable")

# See the top 5 contributors to PC1 and PC2
contrib_tbl %>%
  arrange(desc(Dim.1)) %>%
  slice(1:5)

contrib_tbl %>%
  arrange(desc(Dim.2)) %>%
  slice(1:5)

# # 1c) (Optional) RDA constrained by Country
# rda1 <- rda(comm_hel ~ CountryResponsible, data = meta)
# summary(rda1)
# # biplot:
# plot(rda1, scaling=2, main="RDA: composition ~ Country")

# ─── 2) CLUSTER ANALYSIS & DENDROGRAM ───────────────────────────────────────────
# 2a) Compute distance and hierarchical clustering (Ward’s D2)
dist_mat <- vegdist(comm_hel, method = "euclid")
hc       <- hclust(dist_mat, method = "ward.D2")

# 2b) Plot dendrogram with country labels colored
# first assign each sample a country label
labels_df <- meta %>% 
  mutate(sample = row_number(),
         Country = str_to_title(CountryResponsible))

# assume `hc` is your hclust, and `labels_df$Country` is a character vector
fviz_dend(
  hc,
  # 1) show the tip labels
  show_labels = TRUE,
  # 2) replace the default labels with your country names
  labels      = as.factor(labels_df$Country),
  # 3) colour each label by country
  label_cols  = as.factor(labels_df$Country),
  type        = "rectangle"
) +
  labs(title = "Hierarchical Clustering of Samples by Litter Composition") +
  theme_minimal(base_size = 12) +
  coord_flip()



library(RColorBrewer)

# 1) Generate a palette with one distinct colour per country
countries   <- unique(labels_df$Country)
n_countries <- length(countries)

# Choose a qualitative palette that has at least some colours
max_pal_size <- brewer.pal.info["Paired", "maxcolors"]  # 12

if (n_countries <= max_pal_size) {
  pal <- brewer.pal(n_countries, "Paired")
} else {
  # interpolate from the max colours
  pal <- colorRampPalette(brewer.pal(max_pal_size, "Paired"))(n_countries)
}

# Now pal is length == n_countries, so naming works:
country_cols <- setNames(pal, countries)

# 1) overwrite the labels slot of the hclust
hc$labels <- labels_df$Country

# 2) plot without ever passing labels=… to fviz_dend()
fviz_dend(
  hc,
  show_labels         = TRUE,
  label_cols          = country_cols[hc$labels],  # a character vector of hex‐colours
  type                = "rectangle",
  labels_track_height = 0.3
) +
  labs(title = "Hierarchical Clustering of Samples\nby Litter Composition") +
  theme_minimal(base_size = 12) # +
  #coord_flip()


# Only using significant 5 groups
fviz_dend(
  hc,
  k            = 5,        # cut into 5 clusters
  rect         = TRUE,     # draw rectangles around them
  show_labels  = FALSE,    # no individual tip labels
  k_colors     = "jco",    # a palette for the 5 clusters
  type         = "rectangle"
) +
  labs(title = "Top‑level Clusters of Samples") +
  theme_minimal(base_size = 12) #+
  # coord_flip()




# ─── 3) MULTIVARIATE TESTS ───────────────────────────────────────────────────────
# 3a) PERMANOVA: test if composition differs by Country
set.seed(42)
perm1 <- adonis2(dist_mat ~ CountryResponsible, data = meta, 
                 permutations = 999, by = "margin")
print(perm1)

# 3b) Multivariate Random Forest:  
#     predict Country from composition → variable importance
rf <- randomForest(
  x      = comm_hel,
  y      = as.factor(meta$CountryResponsible),
  ntree  = 500,
  importance = TRUE
)

print(rf)
# An overall OOB error ≈ 65 % tells you that the RF is only slightly better than random guessing across 12 countries (random would be ~91 % error).

# plot variable importance
# A large MeanDecreaseAccuracy means that, when you break the relationship between that predictor and the response, the model’s accuracy suffers a lot → it’s an important predictor for correct country assignments. MDA is generally the more reliable gauge of predictive importance, since it directly measures impact on OOB‐accuracy.
# A large MeanDecreaseGini means that X frequently appears in high‑up the tree splits and yields big purity gains → it’s structurally important to partitioning the data. MDG is faster to compute and tells you which features the trees “prefer” splitting on, but can overweight high–cardinality or continuous variables.
varImpPlot(rf, main="Random Forest: Litter‐type Importance for Country Discrimination")

importance(rf)
