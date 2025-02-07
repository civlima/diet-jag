# Dieta Jaguaribe # 
# Escrito por Vitoria Lima

# 4 PCoA
# Limpar o ambiente
rm(list=ls())

# Pasta de trabalho
setwd("C:/Users/vitor/OneDrive/Papers/Lima et al/2024/JAG Diet")

# Load the previous script
source("Scripts/3_NMDS e PCA.R")

# PCoA com Bray-Curtis
dist_bc <- vegdist(diet_jag_nmds, method = "bray")
pcoa_result <- cmdscale(dist_bc, k = 3, eig = TRUE)

# Criar um data frame a partir dos resultados do PCoA
pcoa_df <- data.frame(
  PCoA1 = pcoa_result$points[, 1],
  PCoA2 = pcoa_result$points[, 2],
  species = nmds_data$species, 
  species_label = resultado_IAi_especies$species_label)

# Calculando as correlações entre as variáveis e as coordenadas principais
correlations <- cor(diet_jag_nmds, pcoa_result$points)

# Transformar as correlações em um data frame
loadings_df <- as.data.frame(correlations)
loadings_df$Variable <- rownames(loadings_df)

# Ordenar variáveis e ajustar os nomes das colunas
loadings_df <- data.frame(
  Variable = rownames(correlations),
  PCoA1 = correlations[, 1],
  PCoA2 = correlations[, 2]
)

# Biplot PCoA - Estação
fit <- envfit(pcoa_result, diet_jag_nmds, perm = 1000)

# Extrair as coordenadas dos vetores (loadings equivalentes)
pcoa_loadings <- as.data.frame(scores(fit, "vectors"))

# Multiplicar por um fator de escala
scale_factor <- 3
pcoa_loadings_scaled <- pcoa_loadings * scale_factor

# Adicione rótulos (nomes dos recursos) para as setas
pcoa_loadings$recursos <- rownames(pcoa_loadings)

# Ajustar rótulos dos recursos
pcoa_loadings$recursos <- c("Algae", 
                            "Higher Plants", 
                            "Insects",  
                            "Decapoda", 
                            "Fish", 
                            "Microinvertebrates", 
                            "Mollusc", 
                            "Terrestrial Invertebrates", 
                            "Detritus")

# PCoA Biplot
ggplot() +
  #geom_jitter(data = pcoa_df, aes(x = PCoA1, y = PCoA2), color = "cadetblue", size = 4, alpha = 0.5, width = 0.2, height = 0.2) +
  geom_segment(data = pcoa_loadings, 
               aes(x = 0, y = 0, xend = pcoa_loadings_scaled[, "Dim1"], yend = pcoa_loadings_scaled[, "Dim2"]),
               arrow = arrow(length = unit(0.3, "cm")), color = "red", size = 1, alpha = 0.5) + 
  geom_text_repel(data = pcoa_loadings, 
                  aes(x = pcoa_loadings_scaled[, "Dim1"], y = pcoa_loadings_scaled[, "Dim2"], 
                      label = recursos),
                  size = 5.5, color = "red", 
                  hjust = 1.1, vjust = 1.1, 
                  nudge_x = 0.1 * sign(pcoa_loadings_scaled[, "Dim1"]), 
                  nudge_y = 0.1 * sign(pcoa_loadings_scaled[, "Dim2"])) + 
  labs(title = "PCoA Biplot",
       x = "PCoA 1",
       y = "PCoA 2") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Inverter a direção das dimensões para alinhar com o plot do PCoA
ggplot() +
  geom_segment(data = pcoa_loadings, 
               aes(x = 0, y = 0, 
                   xend = -pcoa_loadings_scaled[, "Dim1"],  # Invertendo Dim1
                   yend = pcoa_loadings_scaled[, "Dim2"]),
               arrow = arrow(length = unit(0.3, "cm")), color = "red", size = 1, alpha = 0.5) +  # Mantendo as setas vermelhas
  geom_text_repel(data = pcoa_loadings, 
                  aes(x = -pcoa_loadings_scaled[, "Dim1"],  # Invertendo Dim1
                      y = pcoa_loadings_scaled[, "Dim2"], 
                      label = recursos),
                  size = 5.5, color = "red",  # Mantendo o texto vermelho
                  hjust = 1.1, vjust = 1.1, 
                  nudge_x = -0.1 * sign(pcoa_loadings_scaled[, "Dim1"]),  # Ajustando nudge para refletir o eixo invertido
                  nudge_y = 0.1 * sign(pcoa_loadings_scaled[, "Dim2"])) + 
  labs(title = "PCoA Biplot",
       x = "PCoA 1",
       y = "PCoA 2") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(legend.position = "none")  # Removendo a legenda



# Plot PCoA - Geral
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2)) +
  geom_point(size = 4.5, color = color_species, alpha = 0.5) +
  geom_text_repel(aes(label = species_label), color = "black", size = 4.5, max.overlaps = Inf) +
  labs(title = "PCoA of Jaguaribe Diet Composition", x = "PCoA 1", y = "PCoA 2") +
  scale_color_manual(values = color_species) +
  theme_minimal() +
  theme(legend.position = "NULL",
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1))


## PCoA 
# Plotagem transposição - PCoA
dist_bc <- vegdist(select(resultado_transp, -species, -transposition))
pcoa_result <- cmdscale(dist_bc, k= 3, eig = TRUE)

# Selecionar variáveis numéricas para transposição
diet_jag_nmds_transp <- select(resultado_transp, -species, -transposition)

# Calculando as correlações entre as variáveis e as coordenadas principais
correlations <- cor(diet_jag_nmds_transp, pcoa_result$points)

# Transformar as correlações em um data frame
loadings_df <- as.data.frame(correlations)
loadings_df$Variable <- rownames(loadings_df)

# Ordenar variáveis e ajustar os nomes das colunas
loadings_df <- data.frame(
  Variable = rownames(correlations),
  PCoA1 = correlations[, 1],
  PCoA2 = correlations[, 2]
)


pcoa_df_transp <- data.frame(
  PCoA1 = pcoa_result$points[, 1],
  PCoA2 = pcoa_result$points[, 2],
  Labels = resultado_transp$species,
  Transposition = resultado_transp$transposition)

# Ajustar nomes das variáveis para plotagem
pcoa_df_transp <- pcoa_df_transp %>%
  mutate(Labels = recode(Labels, !!!species_labels))

pcoa_df_transp$Transposition <-  str_to_title(pcoa_df_transp$Transposition) 
pcoa_df_transp$Transposition <- factor(pcoa_df_transp$Transposition, levels = c("Pre", "Post"))

# Organizar cores por espécie
unique_species <- length(unique(pcoa_df_transp$Labels))
color_species <- colorRampPalette(brewer.pal(12, "Paired"))(unique_species)

# Plot PCoA - Transposition
ggplot(pcoa_df_transp, aes(x = PCoA1, y = PCoA2, color = Labels)) +
  geom_point(size = 5, alpha = 0.5) +
  geom_text_repel(aes(label = Labels), color = "black", size = 5, max.overlaps = Inf, show.legend = FALSE) +
  labs(title = "PCoA - Transposition", x = "PCoA 1", y = "PCoA 2") +
  scale_color_manual(values = color_species) +
  #  coord_fixed(xlim = x_limits, ylim = y_limits) +
  theme_minimal() +
  theme(
    legend.position = "NULL",
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.line.x = element_blank(), 
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.text = element_text(size = 15)
  ) +
  facet_wrap(~ Transposition)


# Biplot PCoA - Transposição
fit <- envfit(pcoa_result, diet_jag_nmds_transp, perm = 1000)

# Extrair as coordenadas dos vetores (loadings equivalentes)
pcoa_loadings <- as.data.frame(scores(fit, "vectors"))

# Multiplicar por um fator de escala
scale_factor <- 1
pcoa_loadings_scaled <- pcoa_loadings * scale_factor

# Adicione rótulos (nomes dos recursos) para as setas
pcoa_loadings$recursos <- rownames(pcoa_loadings)

# Ajustar rótulos dos recursos
pcoa_loadings$recursos <- c("Algae", 
                            "Higher Plants", 
                            "Insects",  
                            "Decapoda", 
                            "Fish", 
                            "Microinvertebrates", 
                            "Mollusc", 
                            "Terrestrial Invertebrates", 
                            "Detritus")

# Plotagem Biplot
ggplot() +
  geom_jitter(data = pcoa_df_transp, aes(x = PCoA1, y = PCoA2), color = "cadetblue", size = 4, alpha = 0.5, width = 0.2, height = 0.2) +
  geom_segment(data = pcoa_loadings, 
               aes(x = 0, y = 0, xend = pcoa_loadings_scaled[, "Dim1"], yend = pcoa_loadings_scaled[, "Dim2"]),
               arrow = arrow(length = unit(0.3, "cm")), color = "red", size = 1, alpha = 0.5) + 
  geom_text_repel(data = pcoa_loadings, 
                  aes(x = pcoa_loadings_scaled[, "Dim1"], y = pcoa_loadings_scaled[, "Dim2"], 
                      label = recursos),
                  size = 5.5, color = "red", 
                  hjust = 1.1, vjust = 1.1, 
                  nudge_x = 0.1 * sign(pcoa_loadings_scaled[, "Dim1"]), 
                  nudge_y = 0.1 * sign(pcoa_loadings_scaled[, "Dim2"])) + 
  labs(title = "PCoA Biplot",
       x = "PCoA 1",
       y = "PCoA 2") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


# Plotagem por estação
dist_bc <- vegdist(select(resultado_season, -species, -season), method = "bray")
pcoa_result <- cmdscale(dist_bc, k = 3, eig = TRUE)

# Preparar plotagem por estação
diet_jag_nmds_season <- select(resultado_season, -species, -season)

# Calculando as correlações entre as variáveis e as coordenadas principais
correlations <- cor(diet_jag_nmds_season, pcoa_result$points)

# Transformar as correlações em um data frame
loadings_df <- as.data.frame(correlations)
loadings_df$Variable <- rownames(loadings_df)

# Ordenar variáveis e ajustar os nomes das colunas
loadings_df <- data.frame(
  Variable = rownames(correlations),
  PCoA1 = correlations[, 1],
  PCoA2 = correlations[, 2]
)


pcoa_df_season <- data.frame(
  PCoA1 = pcoa_result$points[, 1],
  PCoA2 = pcoa_result$points[, 2],
  Labels = resultado_season$species,
  Season = resultado_season$season)

# Ajustar nomes das variáveis para plotagem
pcoa_df_season <- pcoa_df_season %>%
  mutate(Labels = recode(Labels, !!!species_labels))

pcoa_df_season$Season <- str_to_title(pcoa_df_season$Season) 

# Organizar cores por espécie
unique_species <- length(unique(pcoa_df_season$Labels))
color_species <- colorRampPalette(brewer.pal(12, "Paired"))(unique_species)

# Definindo limites dos gráficos
x_limits <- range(c(pcoa_df_season$PCoA1, pcoa_df_transp$PCoA1))
y_limits <- range(c(pcoa_df_season$PCoA2, pcoa_df_transp$PCoA2))

# Espelhando os eixos para padronizar os plots
pcoa_df_season$PCoA1 <- -pcoa_df_season$PCoA1
pcoa_df_season$PCoA2 <- -pcoa_df_season$PCoA2 

# Plot PCoA - Season
ggplot(pcoa_df_season, aes(x = PCoA1, y = PCoA2, color = Labels)) +
  geom_point(size = 5, alpha = 0.5) +
  geom_text_repel(aes(label = Labels), color = "black", size = 5, max.overlaps = Inf, show.legend = FALSE) +
  labs(title = "PCoA - Season", x = "PCoA 1", y = "PCoA 2") +
  scale_color_manual(values = color_species) +
#  coord_fixed(xlim = x_limits, ylim = y_limits) +
  theme_minimal() +
  theme(
    legend.position = "NULL",
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.line.x = element_blank(), 
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.text = element_text(size = 15)
  ) +
  facet_wrap(~ Season)


# Biplot PCoA - Estação
fit <- envfit(pcoa_result, diet_jag_nmds_season, perm = 1000)

# Extrair as coordenadas dos vetores (loadings equivalentes)
pcoa_loadings <- as.data.frame(scores(fit, "vectors"))

# Multiplicar por um fator de escala
scale_factor <- 3
pcoa_loadings_scaled <- pcoa_loadings * scale_factor

# Adicione rótulos (nomes dos recursos) para as setas
pcoa_loadings$recursos <- rownames(pcoa_loadings)

# Ajustar rótulos dos recursos
pcoa_loadings$recursos <- c("Algae", 
                            "Higher Plants", 
                            "Insects",  
                            "Decapoda", 
                            "Fish", 
                            "Microinvertebrates", 
                            "Mollusc", 
                            "Terrestrial Invertebrates", 
                            "Detritus")

# PCoA Biplot Estação
ggplot() +
  #geom_jitter(data = pcoa_df_season, aes(x = PCoA1, y = PCoA2), color = "cadetblue", size = 4, alpha = 0.5, width = 0.2, height = 0.2) +
  geom_segment(data = pcoa_loadings, 
               aes(x = 0, y = 0, xend = pcoa_loadings_scaled[, "Dim1"], yend = pcoa_loadings_scaled[, "Dim2"]),
               arrow = arrow(length = unit(0.3, "cm")), color = "red", size = 1, alpha = 0.5) + 
  geom_text_repel(data = pcoa_loadings, 
                  aes(x = pcoa_loadings_scaled[, "Dim1"], y = pcoa_loadings_scaled[, "Dim2"], 
                      label = recursos),
                  size = 5.5, color = "red", 
                  hjust = 1.1, vjust = 1.1, 
                  nudge_x = 0.1 * sign(pcoa_loadings_scaled[, "Dim1"]), 
                  nudge_y = 0.1 * sign(pcoa_loadings_scaled[, "Dim2"])) + 
  labs(title = "",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## Plotagem por porções
# PCoA - Porções
dist_bc <- vegdist(select(resultado_portion,-species, -portion), method = "bray")
pcoa_result <- cmdscale(dist_bc, k = 2, eig = TRUE)

# Selecionar variáveis numéricas para porção
diet_jag_nmds_portion <- select(resultado_portion, -species, -portion)

# Calculando as correlações entre as variáveis e as coordenadas principais
correlations <- cor(diet_jag_nmds_portion, pcoa_result$points)

# Transformar as correlações em um data frame
loadings_df <- as.data.frame(correlations)
loadings_df$Variable <- rownames(loadings_df)

# Ordenar variáveis e ajustar os nomes das colunas
loadings_df <- data.frame(
  Variable = rownames(correlations),
  PCoA1 = correlations[, 1],
  PCoA2 = correlations[, 2]
)


pcoa_df_port <- data.frame(
  PCoA1 = pcoa_result$points[, 1],
  PCoA2 = pcoa_result$points[, 2],
  Labels = factor(resultado_portion$species),
  Portion = factor(resultado_portion$portion)
)

# Ajustar nomes das variáveis para plotagem
pcoa_df_port <- pcoa_df_port %>%
  mutate(Labels = recode(Labels, !!!species_labels))

pcoa_df_port$Portion <- str_to_title(pcoa_df_port$Portion)
pcoa_df_port$Portion <- factor(pcoa_df_port$Portion, levels = c("Upper", "Middle", "Lower"))

# Plot PCoA
ggplot(pcoa_df_port, aes(x = PCoA1, y = PCoA2, color = Labels)) +
  geom_point(size = 5, alpha = 0.5) +
  geom_text_repel(aes(label = Labels), color = "black", size = 5, max.overlaps = Inf, show.legend = FALSE) +
  labs(title = "PCoA - Portion", x = "PCoA 1", y = "PCoA 2", size = 4) +
  scale_color_manual(values = color_species) +
  theme_minimal() +
  theme(
    legend.position = "NULL",
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.line.x = element_blank(), 
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.text = element_text(size = 15)
  ) +
  facet_wrap(~ Portion)

# Biplot PCoA - Portion
fit <- envfit(pcoa_result, diet_jag_nmds_portion, perm = 1000)

# Extrair as coordenadas dos vetores (loadings equivalentes)
pcoa_loadings <- as.data.frame(scores(fit, "vectors"))

# Multiplicar por um fator de escala
scale_factor <- 3
pcoa_loadings_scaled <- pcoa_loadings * scale_factor

# Adicione rótulos (nomes dos recursos) para as setas
pcoa_loadings$recursos <- rownames(pcoa_loadings)

# Ajustar rótulos dos recursos
pcoa_loadings$recursos <- c("Algae", 
                            "Higher Plants", 
                            "Insects",  
                            "Decapoda", 
                            "Fish", 
                            "Microinvertebrates", 
                            "Mollusc", 
                            "Terrestrial Invertebrates", 
                            "Detritus")

# PCoA Biplot - ERRADO
ggplot() +
  geom_jitter(data = pcoa_df, aes(x = PCoA1, y = PCoA2), color = "cadetblue", size = 4, alpha = 0.5, width = 0.2, height = 0.2) +
  geom_segment(data = pcoa_loadings, 
               aes(x = 0, y = 0, xend = - pcoa_loadings_scaled[, "Dim1"], yend = pcoa_loadings_scaled[, "Dim2"]),
               arrow = arrow(length = unit(0.3, "cm")), color = "red", size = 1, alpha = 0.5) + 
  geom_text_repel(data = pcoa_loadings, 
                  aes(x = pcoa_loadings_scaled[, "Dim1"], y = pcoa_loadings_scaled[, "Dim2"], 
                      label = recursos),
                  size = 5.5, color = "red", 
                  hjust = 1.1, vjust = 1.1, 
                  nudge_x = 0.1 * sign(pcoa_loadings_scaled[, "Dim1"]), 
                  nudge_y = 0.1 * sign(pcoa_loadings_scaled[, "Dim2"])) + 
  labs(title = "PCoA Biplot - Portion",
       x = "PCoA 1",
       y = "PCoA 2") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

## PCoA 
# Plotagem transposição por estação
# PRE
dist_bc <- vegdist(select(resultado_season_pre, -species, -season))
pcoa_result <- cmdscale(dist_bc, k= 3, eig = TRUE)

# Selecionar variáveis numéricas para transposição
diet_jag_nmds_pre <- select(resultado_season_pre, -species, -season)

# Calculando as correlações entre as variáveis e as coordenadas principais
correlations <- cor(diet_jag_nmds_pre, pcoa_result$points)

# Transformar as correlações em um data frame
loadings_df <- as.data.frame(correlations)
loadings_df$Variable <- rownames(loadings_df)

# Ordenar variáveis e ajustar os nomes das colunas
loadings_df <- data.frame(
  Variable = rownames(correlations),
  PCoA1 = correlations[, 1],
  PCoA2 = correlations[, 2]
)

pcoa_df_pre <- data.frame(
  PCoA1 = pcoa_result$points[, 1],
  PCoA2 = pcoa_result$points[, 2],
  Labels = resultado_season_pre$species,
  Season = resultado_season_pre$season)

# Ajustar nomes das variáveis para plotagem
pcoa_df_pre <- pcoa_df_pre %>%
  mutate(Labels = recode(Labels, !!!species_labels))

pcoa_df_pre$Season <-  str_to_title(pcoa_df_pre$Season) 
pcoa_df_pre$Season <- factor(pcoa_df_pre$Season, levels = c("Dry", "Wet"))

# Organizar cores por espécie
unique_species <- length(unique(pcoa_df_pre$Labels))
color_species <- colorRampPalette(brewer.pal(12, "Paired"))(unique_species)

# Plot PCoA - Transposition by season
ggplot(pcoa_df_pre, aes(x = PCoA1, y = PCoA2, color = Labels)) +
  geom_point(size = 5, alpha = 0.5) +
  geom_text_repel(aes(label = Labels), color = "black", size = 5, max.overlaps = Inf, show.legend = FALSE) +
  labs(title = "Pre-Transposition (PCoA)", x = "PCoA 1", y = "PCoA 2") +
  scale_color_manual(values = color_species) +
  #  coord_fixed(xlim = x_limits, ylim = y_limits) +
  theme_minimal() +
  theme(
    legend.position = "NULL",
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.line.x = element_blank(), 
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.text = element_text(size = 15)
  ) +
  facet_wrap(~ Season)

# Criar biplot Pre-transposition
# Biplot PCoA - Season
fit <- envfit(pcoa_result, diet_jag_nmds_pre, perm = 1000)

# Extrair as coordenadas dos vetores (loadings equivalentes)
pcoa_loadings <- as.data.frame(scores(fit, "vectors"))

# Multiplicar por um fator de escala
scale_factor <- 3
pcoa_loadings_scaled <- pcoa_loadings * scale_factor

# Ajustar nomes de colunas
colnames(pcoa_loadings) <- c("PCoA1", "PCoA2")
colnames(pcoa_loadings_scaled) <- c("PCoA1", "PCoA2")

# Adicione rótulos (nomes dos recursos) para as setas
pcoa_loadings$recursos <- rownames(pcoa_loadings)

# Ajustar rótulos dos recursos
pcoa_loadings$recursos <- c("Algae", 
                            "Higher Plants", 
                            "Insects",  
                            "Decapoda", 
                            "Fish", 
                            "Microinvertebrates", 
                            "Mollusc", 
                            "Terrestrial Invertebrates", 
                            "Detritus")

# PCoA Biplot Pre-transposition Season
ggplot() +
#  geom_jitter(data = pcoa_df_pre, aes(x = PCoA1, y = PCoA2), 
#              color = "cadetblue", size = 4, alpha = 0.5, 
#              width = 0.2, height = 0.2) +
  geom_segment(data = pcoa_loadings, 
               aes(x = 0, y = 0, xend = PCoA1, yend = PCoA2),
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "red", size = 1, alpha = 0.5) + 
  geom_text_repel(data = pcoa_loadings, 
                  aes(x = PCoA1, y = PCoA2, label = recursos),
                  size = 5.5, color = "red", 
                  hjust = 1.1, vjust = 1.1, 
                  nudge_x = 0.1 * sign(pcoa_loadings$PCoA1), 
                  nudge_y = 0.1 * sign(pcoa_loadings$PCoA2)) + 
  
  labs(title = "PCoA Biplot Pre-transposition (Season)",
       x = "PCoA 1",
       y = "PCoA 2") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# POST
dist_bc <- vegdist(select(resultado_season_pos, -species, -season))
pcoa_result <- cmdscale(dist_bc, k= 3, eig = TRUE)

# Selecionar variáveis numéricas para transposição
diet_jag_nmds_pos <- select(resultado_season_pos, -species, -season)

# Calculando as correlações entre as variáveis e as coordenadas principais
correlations <- cor(diet_jag_nmds_pos, pcoa_result$points)

# Transformar as correlações em um data frame
loadings_df <- as.data.frame(correlations)
loadings_df$Variable <- rownames(loadings_df)

# Ordenar variáveis e ajustar os nomes das colunas
loadings_df <- data.frame(
  Variable = rownames(correlations),
  PCoA1 = correlations[, 1],
  PCoA2 = correlations[, 2]
)

pcoa_df_pos <- data.frame(
  PCoA1 = pcoa_result$points[, 1],
  PCoA2 = pcoa_result$points[, 2],
  Labels = resultado_season_pos$species,
  Season = resultado_season_pos$season)

# Ajustar nomes das variáveis para plotagem
pcoa_df_pos <- pcoa_df_pos %>%
  mutate(Labels = recode(Labels, !!!species_labels))

pcoa_df_pos$Season <-  str_to_title(pcoa_df_pos$Season) 
pcoa_df_pos$Season <- factor(pcoa_df_pos$Season, levels = c("Dry", "Wet"))

# Organizar cores por espécie
unique_species <- length(unique(pcoa_df_pos$Labels))
color_species <- colorRampPalette(brewer.pal(12, "Paired"))(unique_species)

# Plot PCoA - Transposition by season
ggplot(pcoa_df_pos, aes(x = PCoA1, y = PCoA2, color = Labels)) +
  geom_point(size = 5, alpha = 0.5) +
  geom_text_repel(aes(label = Labels), color = "black", size = 5, max.overlaps = Inf, show.legend = FALSE) +
  labs(title = "Post-Transposition (PCoA)", x = "PCoA 1", y = "PCoA 2") +
  scale_color_manual(values = color_species) +
  #  coord_fixed(xlim = x_limits, ylim = y_limits) +
  theme_minimal() +
  theme(
    legend.position = "NULL",
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.line.x = element_blank(), 
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.text = element_text(size = 15)
  ) +
  facet_wrap(~ Season)

# Criar biplot Pos-transposition
# Biplot PCoA - Season
fit <- envfit(pcoa_result, diet_jag_nmds_pos, perm = 1000)

# Extrair as coordenadas dos vetores (loadings equivalentes)
pcoa_loadings <- as.data.frame(scores(fit, "vectors"))

# Multiplicar por um fator de escala
scale_factor <- 3
pcoa_loadings_scaled <- pcoa_loadings * scale_factor

# Ajustar nomes de colunas
colnames(pcoa_loadings) <- c("PCoA1", "PCoA2")
colnames(pcoa_loadings_scaled) <- c("PCoA1", "PCoA2")

# Adicione rótulos (nomes dos recursos) para as setas
pcoa_loadings$recursos <- rownames(pcoa_loadings)

# Ajustar rótulos dos recursos
pcoa_loadings$recursos <- c("Algae", 
                            "Higher Plants", 
                            "Insects",  
                            "Decapoda", 
                            "Fish", 
                            "Microinvertebrates", 
                            "Mollusc", 
                            "Terrestrial Invertebrates", 
                            "Detritus")

# PCoA Biplot Pos-transposition Season
ggplot() +
  #  geom_jitter(data = pcoa_df_pre, aes(x = PCoA1, y = PCoA2), 
  #              color = "cadetblue", size = 4, alpha = 0.5, 
  #              width = 0.2, height = 0.2) +
  geom_segment(data = pcoa_loadings, 
               aes(x = 0, y = 0, xend = PCoA1, yend = PCoA2),
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "red", size = 1, alpha = 0.5) + 
  geom_text_repel(data = pcoa_loadings, 
                  aes(x = PCoA1, y = PCoA2, label = recursos),
                  size = 5.5, color = "red", 
                  hjust = 1.1, vjust = 1.1, 
                  nudge_x = 0.1 * sign(pcoa_loadings$PCoA1), 
                  nudge_y = 0.1 * sign(pcoa_loadings$PCoA2)) + 
  
  labs(title = "PCoA Biplot Post-transposition (Season)",
       x = "PCoA 1",
       y = "PCoA 2") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Plotagem transposição por porção do rio 
# PRE
dist_bc <- vegdist(select(resultado_portion_pre, -species, -portion))
pcoa_result <- cmdscale(dist_bc, k= 3, eig = TRUE)

# Selecionar variáveis numéricas para transposição
diet_jag_nmds_pre <- select(resultado_portion_pre, -species, -portion)

# Calculando as correlações entre as variáveis e as coordenadas principais
correlations <- cor(diet_jag_nmds_pre, pcoa_result$points)

# Transformar as correlações em um data frame
loadings_df <- as.data.frame(correlations)
loadings_df$Variable <- rownames(loadings_df)

# Ordenar variáveis e ajustar os nomes das colunas
loadings_df <- data.frame(
  Variable = rownames(correlations),
  PCoA1 = correlations[, 1],
  PCoA2 = correlations[, 2]
)


pcoa_df_pre <- data.frame(
  PCoA1 = pcoa_result$points[, 1],
  PCoA2 = pcoa_result$points[, 2],
  Labels = resultado_portion_pre$species,
  Portion = resultado_portion_pre$portion)

# Ajustar nomes das variáveis para plotagem
pcoa_df_pre <- pcoa_df_pre %>%
  mutate(Labels = recode(Labels, !!!species_labels))

pcoa_df_pre$Portion <-  str_to_title(pcoa_df_pre$Portion) 
pcoa_df_pre$Portion <- factor(pcoa_df_pre$Portion, levels = c("Upper", "Middle", "Lower"))

# Organizar cores por espécie
unique_species <- length(unique(pcoa_df_pre$Labels))
color_species <- colorRampPalette(brewer.pal(12, "Paired"))(unique_species)

# Plot PCoA - Transposition by portion
ggplot(pcoa_df_pre, aes(x = PCoA1, y = PCoA2, color = Labels)) +
  geom_point(size = 5, alpha = 0.5) +
  geom_text_repel(aes(label = Labels), color = "black", size = 5, max.overlaps = Inf, show.legend = FALSE) +
  labs(title = "Pre-Transposition (PCoA)", x = "PCoA 1", y = "PCoA 2") +
  scale_color_manual(values = color_species) +
  #  coord_fixed(xlim = x_limits, ylim = y_limits) +
  theme_minimal() +
  theme(
    legend.position = "NULL",
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.line.x = element_blank(), 
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.text = element_text(size = 15)
  ) +
  facet_wrap(~ Portion)

# Criar biplot Pre-transposition
# Biplot PCoA - Potion
fit <- envfit(pcoa_result, diet_jag_nmds_pre, perm = 1000)

# Extrair as coordenadas dos vetores (loadings equivalentes)
pcoa_loadings <- as.data.frame(scores(fit, "vectors"))

# Multiplicar por um fator de escala
scale_factor <- 3
pcoa_loadings_scaled <- pcoa_loadings * scale_factor

# Ajustar nomes de colunas
colnames(pcoa_loadings) <- c("PCoA1", "PCoA2")
colnames(pcoa_loadings_scaled) <- c("PCoA1", "PCoA2")

# Adicione rótulos (nomes dos recursos) para as setas
pcoa_loadings$recursos <- rownames(pcoa_loadings)

# Ajustar rótulos dos recursos
pcoa_loadings$recursos <- c("Algae", 
                            "Higher Plants", 
                            "Insects",  
                            "Decapoda", 
                            "Fish", 
                            "Microinvertebrates", 
                            "Mollusc", 
                            "Terrestrial Invertebrates", 
                            "Detritus")

# PCoA Biplot Pre-transposition Portion
ggplot() +
  #  geom_jitter(data = pcoa_df_pre, aes(x = PCoA1, y = PCoA2), 
  #              color = "cadetblue", size = 4, alpha = 0.5, 
  #              width = 0.2, height = 0.2) +
  geom_segment(data = pcoa_loadings, 
               aes(x = 0, y = 0, xend = PCoA1, yend = PCoA2),
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "red", size = 1, alpha = 0.5) + 
  geom_text_repel(data = pcoa_loadings, 
                  aes(x = PCoA1, y = PCoA2, label = recursos),
                  size = 5.5, color = "red", 
                  hjust = 1.1, vjust = 1.1, 
                  nudge_x = 0.1 * sign(pcoa_loadings$PCoA1), 
                  nudge_y = 0.1 * sign(pcoa_loadings$PCoA2)) + 
  
  labs(title = "PCoA Biplot Pre-transposition (Portion)",
       x = "PCoA 1",
       y = "PCoA 2") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Criar biplot Post-transposition
# Biplot PCoA - Portion
dist_bc <- vegdist(select(resultado_portion_pos, -species, -portion))
pcoa_result <- cmdscale(dist_bc, k= 3, eig = TRUE)

# Selecionar variáveis numéricas para transposição
diet_jag_nmds_pos <- select(resultado_portion_pos, -species, -portion)

# Calculando as correlações entre as variáveis e as coordenadas principais
correlations <- cor(diet_jag_nmds_pos, pcoa_result$points)

# Transformar as correlações em um data frame
loadings_df <- as.data.frame(correlations)
loadings_df$Variable <- rownames(loadings_df)

# Ordenar variáveis e ajustar os nomes das colunas
loadings_df <- data.frame(
  Variable = rownames(correlations),
  PCoA1 = correlations[, 1],
  PCoA2 = correlations[, 2]
)

pcoa_df_pos <- data.frame(
  PCoA1 = pcoa_result$points[, 1],
  PCoA2 = pcoa_result$points[, 2],
  Labels = resultado_portion_pos$species,
  Portion = resultado_portion_pos$portion)

# Ajustar nomes das variáveis para plotagem
pcoa_df_pos <- pcoa_df_pos %>%
  mutate(Labels = recode(Labels, !!!species_labels))

pcoa_df_pos$Portion <-  str_to_title(pcoa_df_pos$Portion) 
pcoa_df_pos$Portion <- factor(pcoa_df_pos$Portion, levels = c("Upper", "Middle", "Lower"))

# Organizar cores por espécie
unique_species <- length(unique(pcoa_df_pos$Labels))
color_species <- colorRampPalette(brewer.pal(12, "Paired"))(unique_species)

# Plot PCoA - Transposition by portion
ggplot(pcoa_df_pos, aes(x = PCoA1, y = PCoA2, color = Labels)) +
  geom_point(size = 5, alpha = 0.5) +
  geom_text_repel(aes(label = Labels), color = "black", size = 5, max.overlaps = Inf, show.legend = FALSE) +
  labs(title = "Post-Transposition (PCoA)", x = "PCoA 1", y = "PCoA 2") +
  scale_color_manual(values = color_species) +
  #  coord_fixed(xlim = x_limits, ylim = y_limits) +
  theme_minimal() +
  theme(
    legend.position = "NULL",
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.line.x = element_blank(), 
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.text = element_text(size = 15)
  ) +
  facet_wrap(~ Portion)

# Criar biplot Pos-transposition
# Biplot PCoA - Season
fit <- envfit(pcoa_result, diet_jag_nmds_pos, perm = 1000)

# Extrair as coordenadas dos vetores (loadings equivalentes)
pcoa_loadings <- as.data.frame(scores(fit, "vectors"))

# Multiplicar por um fator de escala
scale_factor <- 3
pcoa_loadings_scaled <- pcoa_loadings * scale_factor

# Ajustar nomes de colunas
colnames(pcoa_loadings) <- c("PCoA1", "PCoA2")
colnames(pcoa_loadings_scaled) <- c("PCoA1", "PCoA2")

# Adicione rótulos (nomes dos recursos) para as setas
pcoa_loadings$recursos <- rownames(pcoa_loadings)

# Ajustar rótulos dos recursos
pcoa_loadings$recursos <- c("Algae", 
                            "Higher Plants", 
                            "Insects",  
                            "Decapoda", 
                            "Fish", 
                            "Microinvertebrates", 
                            "Mollusc", 
                            "Terrestrial Invertebrates", 
                            "Detritus")

# PCoA Biplot Pos-transposition Portion
ggplot() +
  #  geom_jitter(data = pcoa_df_pre, aes(x = PCoA1, y = PCoA2), 
  #              color = "cadetblue", size = 4, alpha = 0.5, 
  #              width = 0.2, height = 0.2) +
  geom_segment(data = pcoa_loadings, 
               aes(x = 0, y = 0, xend = PCoA1, yend = PCoA2),
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "red", size = 1, alpha = 0.5) + 
  geom_text_repel(data = pcoa_loadings, 
                  aes(x = PCoA1, y = PCoA2, label = recursos),
                  size = 5.5, color = "red", 
                  hjust = 1.1, vjust = 1.1, 
                  nudge_x = 0.1 * sign(pcoa_loadings$PCoA1), 
                  nudge_y = 0.1 * sign(pcoa_loadings$PCoA2)) + 
  
  labs(title = "PCoA Biplot Post-transposition (Portion)",
       x = "PCoA 1",
       y = "PCoA 2") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

#### Plotagem por transposição??

