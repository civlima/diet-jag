# Dieta Jaguaribe # 
# Escrito por Vitoria Lima

# 2 Amplitude de nicho

# Limpar o ambiente
rm(list=ls())

# Pasta de trabalho
setwd("C:/Users/vitor/OneDrive/Papers/Lima et al/2024/JAG Diet")

# Load the previous script
source("Scripts/1_Organização dos dados e IAi.R")

#### Amplitude de nicho - niche breadth

# Função para calcular o índice de Levins  (Levins, 1968)
# Degree of specialisation in the exploitation of resources 
calc_levins_index <- function(x) {
  B <- apply(x, 1, function(row) {
    p <- row / sum(row)
    B <- 1 / sum(p^2)
    return(B)
  })
  return(B)
}

# Calcular a amplitude de nicho por espécie
amplitude_nicho <- calc_levins_index(iai_num)
results_niche_breadth <- data.frame('Species' = species, 'Niche Breadth' = amplitude_nicho)

# Salvar em .xlsx
#write_xlsx(results_niche_breadth, "Niche Breadth Results - General.xlsx")

# Calcular a amplitude de nicho por variável
# Transposição
# Armazenar os nomes das espécies e tranposição
species_names <- resultado_transp$species
transp_names <- resultado_transp$transposition

# Remover a coluna de espécies
iai_num_transp <- select(resultado_transp, -species, -transposition)

# Converter o dataframe para uma matriz numérica
iai_num_transp <- as.matrix(iai_num_transp)

# Calcular a amplitude de nicho por variável
amplitude_nicho <- calc_levins_index(iai_num_transp)
results_niche_breadth_transp <- data.frame('Species' = species_names, 'Transposition' = transp_names, 'Niche Breadth' = amplitude_nicho)

# Salvar em .xlsx
#write_xlsx(results_niche_breadth_transp, "Niche Breadth Results - Transposition.xlsx")

# Estação
# Armazenar os nomes das espécies e estações para df de resultados
species_names <- resultado_season$species
season_names <- resultado_season$season

# Remover a coluna de espécies
iai_num_season <- select(resultado_season, -species, -season)

# Converter o dataframe para uma matriz numérica
iai_num_season <- as.matrix(iai_num_season)

# Calcular a amplitude de nicho por variável
amplitude_nicho <- calc_levins_index(iai_num_season)
results_niche_breadth_season <- data.frame('Species' = species_names, 'Season' = season_names, 'Niche Breadth' = amplitude_nicho)

# Salvar em .xlsx
#write_xlsx(results_niche_breadth_season, "Niche Breadth Results - Season.xlsx")

# Porção
# Armazenar os nomes das espécies e estações para df de resultados
species_names <- resultado_portion$species
portion_names <- resultado_portion$portion

# Remover a coluna de espécies
iai_num_portion <- select(resultado_portion, -species, -portion)

# Converter o dataframe para uma matriz numérica
iai_num_portion <- as.matrix(iai_num_portion)

# Calcular a amplitude de nicho por variável
amplitude_nicho <- calc_levins_index(iai_num_portion)
results_niche_breadth_portion <- data.frame('Species' = species_names, 'Portion' = portion_names, 'Niche Breadth' = amplitude_nicho)

# Salvar em .xlsx
#write_xlsx(results_niche_breadth_portion, "Niche Breadth Results - Portion.xlsx")

# ANOVA para avaliar significância
# Portion
perform_anova <- function(results_niche_breadth_portion) {
  model <- aov(Niche.Breadth ~ Portion, data = data)
  summary(model)
}

# Niche breadth por estação no pré e no pós
# PRE
species_names_pre <- resultado_season_pre$species
season_names_pre <- resultado_season_pre$season

iai_num_transp_pre <- select(resultado_season_pre, -species, -season)
iai_num_transp_pre <- as.matrix(iai_num_transp_pre)

nb_pre <- calc_levins_index(iai_num_transp_pre)
results_nb_pre <- data.frame('Species' = species_names_pre, 'Season' = season_names_pre, 'Niche Breadth' = nb_pre)

# POS
species_names_pos <- resultado_season_pos$species
season_names_pos <- resultado_season_pos$season

iai_num_transp_pos <- select(resultado_season_pos, -species, -season)
iai_num_transp_pos <- as.matrix(iai_num_transp_pos)

nb_pos <- calc_levins_index(iai_num_transp_pos)
results_nb_pos <- data.frame('Species' = species_names_pos, 'Season' = season_names_pos, 'Niche Breadth' = nb_pos)

# Plotagem
# Carregar o arquivo com todos os valores juntos
NB_Geral <- read.csv("Niche Breadth/Niche Breadth_General.csv", header = TRUE, sep = ";")

# Retirar os valores de Portion
#NB_Geral <- NB_Geral %>%
#  filter(Variable != "Portion")

# Organizar variáveis por fatores
NB_Geral$Type <- factor(NB_Geral$Type, levels = c("Pre", "Post", "Dry", "Wet", "Upper", "Middle", "Lower"))

# Definir a ordem dos níveis da variável para a legenda
NB_Geral$Variable <- factor(NB_Geral$Variable, levels = c("Transposition", "Season", "Portion"))

ggplot(NB_Geral, aes(x = Type, y = Niche.Breadth, fill = Variable, color = Variable, group = Variable)) +
  geom_area(alpha = 0.5, position = 'identity') +  
  geom_smooth(method = "loess", linewidth = 1) + 
  geom_point(aes(shape = factor(ifelse(Niche.Breadth == 0, "zero", "non_zero"))), size = 4, fill = "white") + 
  scale_shape_manual(values = c("zero" = 21, "non_zero" = 19), labels = c("N = 0", "")) +
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "Niche Breadth",
       shape = "") +
  theme(strip.text = element_text(size = 12, face = "italic"),
        axis.title.y = element_text(size = 14), 
        legend.text = element_text(size = 12)) +
  scale_y_continuous(breaks = c(0, 2, 4)) + 
  guides(alpha = "none",
         color = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         shape = guide_legend(override.aes = list(shape = 21, fill = "white", labels = c("N = 0")), order = 2)) +
  facet_wrap(~ Species, ncol = 6)

# Plotagem de dados de season separados por PRÉ e PÓS
# Adicionar coluna "Type" para identificar os dados
results_nb_pre$Transposition <- "Pre"
results_nb_pos$Transposition <- "Post"

# Combinar os DFs
nb_season_transp <- rbind(results_nb_pre, results_nb_pos)

# Converter variáveis em fatores
nb_season_transp$Transposition <- factor(nb_season_transp$Transposition, levels = c("Pre", "Post"))
nb_season_transp$Season <- factor(nb_season_transp$Season, levels = c("dry", "wet"))

# Capitalizar a primeira letra das espécies e trocar _ por . 
nb_season_transp$Species <- str_to_title(as.character(nb_season_transp$Species))
nb_season_transp$Species <- str_replace_all(nb_season_transp$Species, "_", ". ")

nb_season_transp$Season <- str_to_title(as.character(nb_season_transp$Season))

ggplot(nb_season_transp, aes(x = Season, y = Niche.Breadth, fill = Transposition, color = Transposition, group = Transposition)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +  
  geom_point(aes(shape = factor(ifelse(Niche.Breadth == 0, "zero", "non_zero"))), 
             size = 4, fill = "white", position = position_dodge(width = 0.9)) +
  scale_shape_manual(values = c("zero" = 21, "non_zero" = 19), labels = "") +
  theme_minimal() +
  labs(title = "",
       x = "Season",
       y = "Niche Breadth",
       shape = "") +
  theme(strip.text = element_text(size = 12, face = "italic"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        legend.text = element_text(size = 12)) +
  scale_y_continuous(breaks = c(0, 2, 4)) +
  scale_fill_manual(values = c("Pre" = "skyblue", "Post" = "salmon")) + 
  scale_color_manual(values = c("Pre" = "skyblue4", "Post" = "salmon4")) +
  guides(alpha = "none",
         color = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         shape = "none") +
  facet_wrap(~ Species, ncol = 3)