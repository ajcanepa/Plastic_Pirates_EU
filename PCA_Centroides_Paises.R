# PCA_ALL_Waste -----------------------------------------------------------
# Todos los puntos con borde negro y relleno por páis.
x11()
fviz_pca_biplot(
  pca_prcomp,
  geom.ind     = "point",
  fill.ind     = meta$CountryResponsible %>% str_to_title(),  # ← usar fill
  palette      = "Set3",
  addEllipses  = FALSE,
  ellipse.type = "confidence",
  label        = "var",
  repel        = TRUE,
  pointsize    = 4,
  pointshape   = 21,      # ← permite fill + borde
  col.ind      = "black", # ← borde negro
  alpha.ind    = 0.8
) +
  labs(title = "PCA of Litter Composition (Hellinger‐transformed)",
       subtitle = "Points = samples colored by Country; arrows = litter types",
       fill = "Country") +   # ← cambiar a fill
  theme_minimal(base_size = 14) +
  guides(color = "none")     # aseguramos que no aparezca leyenda de color


# PCA_CENTROIDES_COLORES --------------------------------------------------

library(dplyr)
library(stringr)
library(ggplot2)
library(factoextra)
library(RColorBrewer)


# Calcular centroides manualmente
scores <- as.data.frame(pca_prcomp$x)
scores$Country <- meta$CountryResponsible %>% str_to_title()

centroids <- scores %>%
  group_by(Country) %>%
  summarise(PC1 = mean(PC1),
            PC2 = mean(PC2))

# Definir niveles y paleta una sola vez

meta$Country <- factor(str_to_title(meta$CountryResponsible))

# Definir niveles únicos y ordenados
countries <- levels(meta$Country)

# Crear paleta manual coherente
my_palette <- setNames(
  brewer.pal(length(countries), "Set3"),
  countries
)

# PCA con puntos pequeños (color)
p <- fviz_pca_biplot(
  pca_prcomp,
  geom.ind   = "point",
  col.ind    = meta$Country,
  addEllipses = FALSE,
  label      = "var",
  repel      = TRUE,
  pointsize  = 2.5,
  pointshape = 16,
  alpha.ind  = 0.6
)


# Unificar escalas (CLAVE 🔥)
x11()
p +
  geom_point(data = centroids,
             aes(x = PC1, y = PC2, fill = Country),
             shape = 21,
             size = 5,
             color = "black",
             stroke = 1.2) +
  
  scale_color_manual(values = my_palette, drop = FALSE) +
  scale_fill_manual(values = my_palette, drop = FALSE) +
  
  labs(title = "PCA of Litter Composition (Hellinger‐transformed)",
       subtitle = "Small points = samples; Large points = country centroids",
       color = "Country",
       fill = "Country") +
  
  theme_minimal(base_size = 14)


# PCA_Sigle_UsePlastic ----------------------------------------------------
library(dplyr)
library(stringr)
library(ggplot2)
library(factoextra)
library(RColorBrewer)

# PCA
pca_prcomp_SUP <- prcomp(comm_hel_SUP, scale. = TRUE)

# -----------------------------
# 1️⃣ Preparar factor y paleta común
# -----------------------------
meta_SUP$Country <- factor(str_to_title(meta_SUP$CountryResponsible))
countries_sup <- levels(meta_SUP$Country)

my_palette_sup <- setNames(
  brewer.pal(length(countries_sup), "Set3"),
  countries_sup
)

# -----------------------------
# 2️⃣ Crear gráfico base (p_sup) con puntos pequeños
# -----------------------------
p_sup <- fviz_pca_biplot(
  pca_prcomp_SUP,
  geom.ind    = "point",
  col.ind     = meta_SUP$Country,
  addEllipses = FALSE,
  label       = "var",
  repel       = TRUE,
  pointsize   = 2.5,
  pointshape  = 16,
  alpha.ind   = 0.6
)

# -----------------------------
# 3️⃣ Calcular centroides manualmente
# -----------------------------
scores_sup <- as.data.frame(pca_prcomp_SUP$x)
scores_sup$Country <- meta_SUP$Country

centroids_sup <- scores_sup %>%
  group_by(Country) %>%
  summarise(
    PC1 = mean(PC1),
    PC2 = mean(PC2),
    .groups = "drop"
  )

# -----------------------------
# 4️⃣ Añadir centroides y unificar escalas
# -----------------------------
SUP_PCA_Litter_Country2 <-
  p_sup +
  geom_point(data = centroids_sup,
             aes(x = PC1, y = PC2, fill = Country),
             shape = 21,
             size = 5,
             color = "black",
             stroke = 1.2) +
  scale_color_manual(values = my_palette_sup, drop = FALSE) +
  scale_fill_manual(values = my_palette_sup, drop = FALSE) +
  labs(title = "PCA of Litter Composition (Hellinger‐transformed)",
       subtitle = "Points = samples colored by Country; arrows = Single use plastic types",
       color = "Country",
       fill  = "Country") +
  theme_minimal(base_size = 14)

SUP_PCA_Litter_Country2

