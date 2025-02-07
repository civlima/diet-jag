# Dieta Jaguaribe # 
# Escrito por Vitoria Lima

#1 Organização dos dados e IAi

#### Carregar ambiente ####

# Pasta de trabalho
setwd("C:/Users/vitor/OneDrive/Papers/Lima et al/2024/JAG Diet")

# Pacotes
library(vegan); library(dplyr); library(tidyr); library(RColorBrewer); library(ggplot2); library(permuco); library(writexl); library (ggrepel); library (stringr); library (reshape2); library(gridExtra)

# Upload da matriz de volume por itens alimentares
diet_jag_items <- read.csv("diet_jag.csv", header = TRUE, sep = ";")

# Lista de espécies
species <- unique(diet_jag_items$species)

# Ajustar nomes das espécies
species_labels <- c("a_bimaculatus" = "A. bimaculatus", 
                    "c_jaguaribensis" = "C. jaguaribensis",
                    "c_monoculus" = "C. monoculus",
                    "c_orientale" = "C. orientale",
                    "h_pusarum" = "H. pusarum",
                    "h_marginatus" = "H. marginatus",
                    "h_rodwayi" = "H. rodwayi",
                    "h_sertanejo" = "H. sertanejo",
                    "l_derbyi" = "L. derbyi",
                    "l_platymetopon" = "L. platymetopon",
                    "l_taeniatus" = "L. taeniatus",
                    "m_costae" = "M. costae",
                    "m_intermedia" = "M. intermedia",
                    "o_niloticus" = "O. niloticus",
                    "p_brevis" = "P. brevis",
                    "p_fasciatus" = "P. fasciatus",
                    "p_kennedyi" = "P. kennedyi",
                    "p_vivipara" = "P. vivipara",
                    "s_brandtii" = "S. brandtii",
                    "s_brasiliensis" = "S. brasiliensis",
                    "s_heterodon" = "S. heterodon",
                    "s_notonota" = "S. notonota",
                    "s_piaba" = "S. piaba",
                    "t_signatus" = "T. signatus")

# Copiar o arquivo original para manipular os itens
diet_jag <- diet_jag_items

# Fazer a soma de amostragem de espécies por transposição
diet_sum_transposition <- diet_jag %>%
  group_by(species, transposition) %>%
  summarise(count = n(), .groups = 'drop') %>%
  bind_rows(
    summarise(., species = "Total", count = sum(count)))

# Fazer a soma de amostragem de espécies por porção do rio
diet_sum_portion <- diet_jag %>%
  group_by(species, portion) %>%
  summarise(count = n(), .groups = 'drop') %>%
  bind_rows(
    summarise(., species = "Total", count = sum(count)))

# Fazer a soma de amostragem de espécies por estação
diet_sum_season <- diet_jag %>%
  group_by(species, season) %>%
  summarise(count = n(), .groups = 'drop') %>%
  bind_rows(
    summarise(., species = "Total", count = sum(count)))

# Renomear colunas para organização dos dados
diet_jag <- diet_jag %>% rename(insect_ni = insect)
diet_jag <- diet_jag %>% rename(hig_plants = sup_veg)

# Agrupamento de itens por recursos alimentares/guildas
insetos <- c("ephemeroptera",  "coleoptera",  "odonata", "diptera", "trichoptera",
             "hymenoptera", "hemiptera", "lepidoptera", "orthoptera", "insect_ni")
diet_jag$insect <- rowSums(diet_jag[insetos])
diet_jag <- diet_jag[, !(colnames(diet_jag) %in% c("ephemeroptera",  "coleoptera",  "odonata", "diptera", "trichoptera",
                                                   "hymenoptera", "hemiptera", "lepidoptera", "orthoptera", "insect_ni"))]

# Agrupando "microinvertebrate"
microinvert <- c("copepoda", "cladocera", "ostracoda")
diet_jag$microinvert <- rowSums(diet_jag[microinvert])
diet_jag <- diet_jag[, !(colnames(diet_jag) %in% c("copepoda", "cladocera", "ostracoda"))]

# Agrupando "ter_invert"
ter_invert <- c("collembola", "arachnida")
diet_jag$ter_invert <- rowSums(diet_jag[ter_invert])
diet_jag <- diet_jag[, !(colnames(diet_jag) %in% c("collembola", "arachnida"))]

# Agrupando "mollusc"
mollusc <- c("bivalvia", "gastropoda")
diet_jag$mollusc <- rowSums(diet_jag[mollusc])
diet_jag <- diet_jag[, !(colnames(diet_jag) %in% c("bivalvia", "gastropoda", "sum", "site"))]

# Organizar as colunas
diet_jag <- select(diet_jag, id, species, season, transposition, portion, algae, hig_plants, insect, decapoda, fish,
                   microinvert, mollusc, ter_invert, detritus)

# Separar variáveis categóricas e numéricas
diet_jag_num <- select(diet_jag,-id, -species, -season, -transposition, -portion)
diet_jag_cat <- select(diet_jag, id, species, season, transposition, portion)

# Converter valores para log +1 para reduzir assimetria dos dados
diet_jag_iai <- diet_jag_num %>% 
  mutate(across(where(is.numeric), ~ log(. + 1)))

# Adicionar coluna de especies
species_col <- diet_jag$species
diet_jag_iai <- cbind(species = species_col, diet_jag_iai)

# Separar planilhas de pré e pós
diet_jag_pre <- subset(diet_jag, transposition == "pre")
diet_jag_post <- subset(diet_jag, transposition == "post")

#### Criar funções para aplicar o IAi ####
# Cálculo geral do IAi
calcular_IAi <- function(dados_especie) {
  dados_numericos <- dados_especie %>% select_if(is.numeric)
  Fi <- colSums(dados_numericos > 0) / nrow(dados_numericos)  
  Vi <- colSums(dados_numericos)  
  IAi <- (Fi * Vi) / sum(Fi * Vi) * 100
  IAi <- round(IAi, 2)
  return(IAi)
}

IAi_especies <- function(diet_jag_iai) {
  species_list <- unique(diet_jag_iai$species)
  resultado_IAi_especies <- data.frame(species = species_list)
  
  for (species in species_list) {
    dados_especie <- diet_jag_iai %>% filter(species == !!species) %>% select(-species)
    IAi_resultados <- calcular_IAi(dados_especie)
    resultado_IAi_especies[species == species_list, names(IAi_resultados)] <- IAi_resultados
  }
  
  return(resultado_IAi_especies)
}

# Aplicar a função no dataframe diet_jag_iai
resultado_IAi_especies <- IAi_especies(diet_jag_iai)

# Resultados do IAi por espécie e item alimentar
resultado_IAi_especies <- data.frame(resultado_IAi_especies)

# Salvar em .xlsx
#write_xlsx(resultado_IAi_especies, "IAi Results - General.xlsx")

# Excluir variavel categorica para análises posteriores
iai_num <- resultado_IAi_especies[, -1]

# Converter dataframe para uma matriz numérica
iai_num <- as.matrix(iai_num)

# Calcular IAi por variáveis
# Transposição
IAi_transp <- function(diet_jag) {
  species_list <- unique(diet_jag$species)
  transp_list <- unique(diet_jag$transposition)
  
  lista_resultados <- list()
  
  for (species in species_list) {
    for (transposition in transp_list) {
      
      dados_species_transp <- diet_jag %>% 
        filter(species == !!species & transposition == !!transposition) %>% 
        select(-id, -species, -season, -transposition, -portion)
      
      if (nrow(dados_species_transp) > 0) {
        IAi_resultados <- calcular_IAi(dados_species_transp)
        resultado <- data.frame(species = species,
                                transposition = transposition,
                                t(IAi_resultados))
        lista_resultados <- c(lista_resultados, list(resultado))
        
      }
    }
  }
  resultado_IAi_transp <- do.call(rbind, lista_resultados)
  return(resultado_IAi_transp)  
  
}

# Aplicar a função de transposição no dataframe diet_jag
resultado_transp <- IAi_transp(diet_jag)

# Checar se a soma final é igual a 100, e logo após apagar a coluna
#resultado_transp$rowsum <- rowSums(resultado_transp[, -c(1, 2)], na.rm = TRUE)
#print(resultado_transp$rowsum)
#resultado_transp$rowsum <- NULL

# Exportar para .xlsx
#write_xlsx(resultado_transp, "IAi Results - Transposition.xlsx")

# Resultados por períodos da transposição
resultado_transp_pre <- IAi_transp(diet_jag_pre)
resultado_transp_pos <- IAi_transp(diet_jag_post)

# Estação
IAi_season <- function(diet_jag) {
  species_list <- unique(diet_jag$species)
  season_list <- unique(diet_jag$season)
  
  lista_resultados <- list()
  
  for (species in species_list) {
    for (season in season_list) {
      
      dados_species_season <- diet_jag %>% 
        filter(species == !!species & season == !!season) %>% 
        select(-id, -species, -season, -transposition, -portion)
      
      if (nrow(dados_species_season) > 0) {
        IAi_resultados <- calcular_IAi(dados_species_season)
        resultado <- data.frame(species = species,
                                season = season,
                                t(IAi_resultados))
        lista_resultados <- c(lista_resultados, list(resultado))
        
      }
    }
  }
  resultado_IAi_season <- do.call(rbind, lista_resultados)
  return(resultado_IAi_season)  
  
}

# Aplicar a função de season no dataframe diet_jag
resultado_season <- IAi_season(diet_jag)

# Checar se a soma final é igual a 100, e logo após apagar a coluna
#resultado_season$rowsum <- rowSums(resultado_season[, -c(1, 2)], na.rm = TRUE)
#print(resultado_season$rowsum)
#resultado_season$rowsum <- NULL

# Exportar para .xlsx
#write_xlsx(resultado_season, "IAi Results - Season.xlsx")

## Função IAi por porções do rio
# Portion
IAi_portion <- function(diet_jag) {
  species_list <- unique(diet_jag$species)
  portion_list <- unique(diet_jag$portion)
  
  lista_resultados <- list()
  
  for (species in species_list) {
    for (portion in portion_list) {
      
      dados_species_portion <- diet_jag %>% 
        filter(species == !!species & portion == !!portion) %>% 
        select(-id, -species, -season, -transposition, -portion)
      
      if (nrow(dados_species_portion) > 0) {
        IAi_resultados <- calcular_IAi(dados_species_portion)
        resultado <- data.frame(species = species,
                                portion = portion,
                                t(IAi_resultados))
        lista_resultados <- c(lista_resultados, list(resultado))
        
      }
    }
  }
  resultado_IAi_portion <- do.call(rbind, lista_resultados)
  return(resultado_IAi_portion)  
  
}

# Aplicar a função de season no dataframe diet_jag
resultado_portion <- IAi_portion(diet_jag)

# Checar se a soma final é igual a 100, e logo após apagar a coluna
#resultado_portion$rowsum <- rowSums(resultado_portion[, -c(1, 2)], na.rm = TRUE)
#print(resultado_portion$rowsum)
#resultado_portion$rowsum <- NULL

# Exportar para .xlsx
#write_xlsx(resultado_portion, "IAi Results - Portion.xlsx")

# Resultados por períodos da transposição (season)
resultado_season_pre <- IAi_season(diet_jag_pre)
resultado_season_pos <- IAi_season(diet_jag_post)

# Salvar resultados do pré e pós
#write_xlsx(resultado_season_pre, "IAi Results - Pre.xlsx")
#write_xlsx(resultado_season_pos, "IAi Results - Post.xlsx")


# Resultados por períodos da transposição (portion)
resultado_portion_pre <- IAi_portion(diet_jag_pre)
resultado_portion_pos <- IAi_portion(diet_jag_post)

### Guilds
guilds <- read.csv("guilds.csv", header = TRUE, sep = ";")

# Carregar os arquivos de guildas separados para pré e pós
guilds_pre <- read.csv("guilds_pre.csv", header = TRUE, sep = ";")
guilds_pos <- read.csv("guilds_post.csv", header = TRUE, sep = ";")

# Calcular o total de cada coluna (última linha do dataframe)
totals <- guilds[nrow(guilds), -1]

# Copiar o dataframe original para a versão relativa (sem a linha de total)
guilds_relative <- guilds[-nrow(guilds), ]

# Retirar a linha de total dos df de pré e pós
guilds_relative_pre <- guilds_pre[-nrow(guilds_pre), ]
guilds_relative_pos <- guilds_pos[-nrow(guilds_pos), ]

# Calcular a predominância relativa para cada coluna, exceto a primeira
for (i in 2:ncol(guilds_relative)) {
  guilds_relative[, i] <- guilds_relative[, i] / guilds[nrow(guilds), i]
}

# Converter os dados para long format
guilds_long <- melt(guilds_relative, id.vars = "Guilds")

# Retirar os dados de Portion
#guilds_long <- guilds_long %>%
#  filter(!variable %in% c("Upper", "Middle", "Lower"))

# Adicionar a coluna "type" ao dataframe guilds_long
guilds_long <- guilds_long %>%
  mutate(type = case_when(
    variable %in% c("Pre", "Post") ~ "Transposition",
    variable %in% c("Dry", "Wet") ~ "Season",
    variable %in% c("Upper", "Middle", "Lower") ~ "Portion"
  ))

# Definir a ordem das variáveis
guilds_long$type <- factor(guilds_long$type, levels = c("Transposition", "Season", "Portion"))

# Transformar em dados relativos os df de pré e pós
# Pré
for (i in 2:ncol(guilds_relative_pre)) {
  guilds_relative_pre[, i] <- guilds_relative_pre[, i] / guilds_pre[nrow(guilds_pre), i]
}

# Pós
for (i in 2:ncol(guilds_relative_pos)) {
  guilds_relative_pos[, i] <- guilds_relative_pos[, i] / guilds_pos[nrow(guilds_pos), i]
}

# Converter os dados para formato longo (long format)
guilds_long_pre <- melt(guilds_relative_pre, id.vars = "Guilds_Pre")
guilds_long_pos <- melt(guilds_relative_pos, id.vars = "Guilds_Post")

# Renomear as colunas de Guilds
colnames(guilds_long_pre)[1] <- "Guilds"
colnames(guilds_long_pos)[1] <- "Guilds"

# Mapear cores
# display.brewer.all()
palette <- brewer.pal(9,"Pastel1")
border_palette <- c("Transposition" = scales::alpha("coral1"),
                    "Season" = scales::alpha("darkslategray4"),
                    "Portion" = scales::alpha("cornflowerblue"))

# Criar o gráfico de barras empilhadas
ggplot(guilds_long, aes(x = variable, y = value, fill = Guilds, color = type)) +
  geom_bar(stat = "identity", position = "fill", size = 1.2) +
  scale_fill_manual(values = palette) +  
  scale_color_manual(values = border_palette) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Relative Abundance", x = "", fill = "Guilds", color = "Variable") +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12), 
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14), 
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 14), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(fill = "white")))

# Criar o gráfico de barras empilhadas para o pré
plot_pre <- ggplot(guilds_long_pre, aes(x = variable, y = value, fill = Guilds)) +
  geom_bar(stat = "identity", position = "fill", size = 1.2, color = "darkslategray4") +
  scale_fill_manual(values = palette) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Relative Abundance", x = "Season", fill = "Guilds", title = "Pre-Transposition") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

# Exibir gráfico do pré
plot_pre

# Criar o gráfico de barras empilhadas para o pós
plot_pos <- ggplot(guilds_long_pos, aes(x = variable, y = value, fill = Guilds)) +
  geom_bar(stat = "identity", position = "fill", size = 1.2, color = "coral1") +
  scale_fill_manual(values = palette) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Relative Abundance", x = "Season", fill = "Guilds", title = "Post-Transposition") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Exibir gráfico do pós
plot_pos

# Exibir os gráficos lado a lado (opcional)
grid.arrange(plot_pre, plot_pos, ncol = 2)