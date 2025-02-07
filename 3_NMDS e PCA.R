# Dieta Jaguaribe # 
# Escrito por Vitoria Lima

# 3 NMDS e PCA

# Limpar o ambiente
rm(list=ls())

# Pasta de trabalho
setwd("C:/Users/vitor/OneDrive/Papers/Lima et al/2024/JAG Diet")

# Load the previous script
source("Scripts/2_Amplitude de nicho.R")

#### Avaliar a significância da composição da dieta
# Selecionar variáveis numéricas
diet_jag_nmds <- select(resultado_IAi_especies, -species)

# Visualizar a distância de Bray-Curtis através de NMDS
nmds_result <- metaMDS(diet_jag_nmds, distance = "bray", k = 3)

## Preparar a plotagem 
# Mapear cores 
#species_colors <- as.factor(resultado_IAi_especies$species)
resultado_IAi_especies$species_label <- species_labels[resultado_IAi_especies$species]

unique_species <- length(unique(resultado_IAi_especies$species_label))
color_species <- colorRampPalette(brewer.pal(12, "Paired"))(unique_species)

## Plots 
# NMDS Bray-Curtis - similaridade da composição da dieta 
# Converte os resultados do NMDS para um data frame
nmds_data <- as.data.frame(nmds_result$points)

# Adiciona a coluna de espécies
nmds_data$species <- resultado_IAi_especies$species
nmds_data$species_label <- resultado_IAi_especies$species_label

# Cria o gráfico usando ggplot2 e ggrepel para evitar sobreposição dos rótulos
ggplot(nmds_data, aes(x = MDS1, y = MDS2, color = species_label)) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = species_label), size = 4.5, max.overlaps = Inf) +
  labs(title = "NMDS of Jaguaribe Diet Composition") +
  scale_color_manual(values = color_species) +
  theme_minimal() +
  theme(legend.position = "NULL",
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# PCA
pca_result <- prcomp(diet_jag_nmds, center = TRUE, scale. = TRUE)

pca_df <- data.frame(
  PCA1 = pca_result$x[, 1],
  PCA2 = pca_result$x[, 2],
  species = nmds_data$species, 
  species_label = resultado_IAi_especies$species_label)

# Visualizar resultados da PCA - Recursos
pca_data <- data.frame(pca_result$x)
pca_loadings <- data.frame(pca_result$rotation)

# Selecione os dois primeiros componentes principais
pc1 <- "PC1"
pc2 <- "PC2"

# Aumentando a escala das setas
scale_factor <- 3
pca_loadings_scaled <- pca_loadings * scale_factor

# Criar nova coluna com nomes dos recursos
pca_loadings$recursos <- rownames(pca_loadings)

# Ajustar rótulos dos recursos
pca_loadings$recursos <- c("Algae", 
                           "Higher Plants", 
                           "Insects",  
                           "Decapoda", 
                           "Fish", 
                           "Microinvertebrates", 
                           "Mollusc", 
                           "Terrestrial Invertebrates", 
                           "Detritus")


# Plot - Biplot Consumo Geral
ggplot() +
  geom_jitter(data = pca_data, aes_string(x = pc1, y = pc2), color = "cadetblue", size = 4, alpha = 0.5, width = 0.2, height = 0.2) +
  geom_segment(data = pca_loadings, 
               aes(x = 0, y = 0, xend = pca_loadings_scaled[, pc1], yend = pca_loadings_scaled[, pc2]),
               arrow = arrow(length = unit(0.5, "cm")), color = "red", size = 1, alpha = 0.5) + 
  geom_text_repel(data = pca_loadings, 
                  aes(x = pca_loadings_scaled[, pc1], y = pca_loadings_scaled[, pc2], 
                      label = recursos),
                  size = 6, color = "red", 
                  hjust = 1.1, vjust = 1.1, 
                  nudge_x = 0.1 * sign(pca_loadings_scaled[, pc1]), 
                  nudge_y = 0.1 * sign(pca_loadings_scaled[, pc2])) + 
  labs(title = "PCA Biplot",
       x = paste0("Principal Component 1 (", round(100 * summary(pca_result)$importance[2, 1], 1), "%)"),
       y = paste0("Principal Component 2 (", round(100 * summary(pca_result)$importance[2, 2], 1), "%)")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


# Plot PCA com ggplot2 - Consumo Geral
ggplot(pca_df, aes(x = PCA1, y = PCA2)) +
  geom_point(size = 4.5, color = color_species, alpha = 0.5) +
  geom_text_repel(aes(label = species_label), color = "black", size = 4.5, max.overlaps = Inf) +
  labs(title = "PCA of Jaguaribe Diet Composition",
       x = paste0("Principal Component 1 (", round(100 * summary(pca_result)$importance[2, 1], 1), "%)"),
       y = paste0("Principal Component 2 (", round(100 * summary(pca_result)$importance[2, 2], 1), "%)")) +
  scale_color_manual(values = color_species) +
  theme_minimal() +
  theme(legend.position = "NULL",
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1) )
