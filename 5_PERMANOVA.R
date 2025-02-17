# Dieta Jaguaribe # 
# Escrito por Vitoria Lima

# 5 PERMANOVA

# Limpar o ambiente
rm(list=ls())

# Pasta de trabalho
setwd("C:/Users/vitor/OneDrive/Papers/Lima et al/2024/JAG Diet")

# Load the previous script
source("Scripts/4_PCoA e Plotagem.R")

### Checar a dissimilaridade com Bray-Curtis
## PERMANOVA
# Preparar os dados e remover NAs
relevant_cols <- c("id", "species", "season", "transposition", "portion")

#diet_jag$portion <- NULL

diet_jag_clean <- diet_jag %>%
  filter(rowSums(select(., -all_of(relevant_cols)) %>% 
                   select(where(is.numeric))) > 0)

# Calcular Bray-Curtis
dist_bc_geral <- vegdist(select(diet_jag_clean, -all_of(relevant_cols)) %>% 
                           select_if(is.numeric), method = "bray")

# PERMANOVA com todas as variáveis categóricas
#permanova_result_transp <- adonis2(dist_bc ~ species + transposition + season + portion, data = diet_jag_clean, permutations = 1000)
#print(permanova_result_transp)

# Converter em dataframe e salvar
#permanova_transp_df <- as.data.frame(permanova_result_transp)
#write_xlsx(permanova_transp_df, "PERMANOVA Results.xlsx")

# Extraindo os valores de R2
#r2_values <- c(species = 0,149838557, transposition = 0,026011024, season = 0,009876043, portion = 0,024715801)

# Criando um dataframe para ggplot2
#r2_df <- data.frame(Variable = names(r2_values), R2 = as.numeric(r2_values))

# Plot Barplot
#ggplot(r2_df, aes(x = Variable, y = R2)) +
#  geom_bar(stat = "identity", fill = "steelblue") +
#  labs(title = "R2 Values from PERMANOVA", x = "Variable", y = "R2") +
#  theme_minimal()

## PERMDISP para calcular os centróides por transposição
#permdisp_result <- betadisper(dist_bc, diet_jag_clean$transposition)
#summary(permdisp_result)

# ANOVA para checar diferenças entre os grupos
#distances_to_centroids <- permdisp_result$distances
#anova_result <- anova(permdisp_result)
#print(anova_result)

# Teste post-hoc
#tukey_result <- TukeyHSD(permdisp_result)
#print(tukey_result)

# PERMANOVA entre variáveis para cada espécie
# Criar um dataframe para cada espécie
diet_jag_list <- split(diet_jag_clean, diet_jag_clean$species)

# Loop para criar dataframes dinamicamente
for(species_name in names(diet_jag_list)) {
  assign(species_name, diet_jag_list[[species_name]])
}

# Calcular Bray-Curtis - A. bimaculatus
dist_bc_a_bimaculatus <- vegdist(select(a_bimaculatus, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_a_bimaculatus <- adonis2(dist_bc_a_bimaculatus ~ transposition * season + transposition * portion, 
                                   data = a_bimaculatus, permutations = 9999)

# Calcular Bray-Curtis - C. jaguaribensis
dist_bc_c_jaguaribensis <- vegdist(select(c_jaguaribensis, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_c_jaguaribensis <- adonis2(dist_bc_c_jaguaribensis ~ transposition * season + transposition * portion, 
                                     data = c_jaguaribensis, permutations = 9999)

# Calcular Bray-Curtis - C. monoculus
dist_bc_c_monoculus <- vegdist(select(c_monoculus, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_c_monoculus <- adonis2(dist_bc_c_monoculus ~ transposition * season + transposition * portion, 
                                 data = c_monoculus, permutations = 9999)

# Calcular Bray-Curtis - C. orientale
dist_bc_c_orientale <- vegdist(select(c_orientale, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_c_orientale <- adonis2(dist_bc_c_orientale ~ transposition * season + transposition * portion, 
                                 data = c_orientale, permutations = 9999)

# Calcular Bray-Curtis - H. marginatus
dist_bc_h_marginatus <- vegdist(select(h_marginatus, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_h_marginatus <- adonis2(dist_bc_h_marginatus ~ transposition * season + transposition * portion, 
                                  data = h_marginatus, permutations = 9999)

# Calcular Bray-Curtis - H. pusarum
dist_bc_h_pusarum <- vegdist(select(h_pusarum, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_h_pusarum <- adonis2(dist_bc_h_pusarum ~ transposition * season + transposition * portion, 
                               data = h_pusarum, permutations = 9999)

# Calcular Bray-Curtis - H. rodwayi
#dist_bc_h_rodwayi <- vegdist(select(h_rodwayi, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
#permanova_h_rodwayi <- adonis2(dist_bc_h_rodwayi ~ transposition * season, data = h_rodwayi, permutations = 1000)

# Calcular Bray-Curtis - H. sertanejo
#dist_bc_h_sertanejo <- vegdist(select(h_sertanejo, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
#permanova_h_sertanejo <- adonis2(dist_bc_h_sertanejo ~ transposition + season + portion, data = h_sertanejo, permutations = 1000)

# Calcular Bray-Curtis - L. derbyi
dist_bc_l_derbyi <- vegdist(select(l_derbyi, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_l_derbyi <- adonis2(dist_bc_l_derbyi ~ transposition * season + transposition * portion, 
                              data = l_derbyi, permutations = 9999)

# Calcular Bray-Curtis - L. platymetopon
dist_bc_l_platymetopon <- vegdist(select(l_platymetopon, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_l_platymetopon <- adonis2(dist_bc_l_platymetopon ~ transposition * season + transposition * portion, 
                                    data = l_platymetopon, permutations = 9999)

# Calcular Bray-Curtis - L. taeniatus
#dist_bc_l_taeniatus <- vegdist(select(l_taeniatus, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
#permanova_l_taeniatus <- adonis2(dist_bc_l_taeniatus ~ transposition + season + portion, data = l_taeniatus, permutations = 1000)

# Calcular Bray-Curtis - M. costae
#dist_bc_m_costae <- vegdist(select(m_costae, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
#permanova_m_costae <- adonis2(dist_bc_m_costae ~ transposition * season, data = m_costae, permutations = 1000)

# Calcular Bray-Curtis - M. intermedia
dist_bc_m_intermedia <- vegdist(select(m_intermedia, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_m_intermedia <- adonis2(dist_bc_m_intermedia ~ transposition * season + transposition * portion, 
                                  data = m_intermedia, permutations = 9999)

# Calcular Bray-Curtis - O. niloticus
dist_bc_o_niloticus <- vegdist(select(o_niloticus, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_o_niloticus <- adonis2(dist_bc_o_niloticus ~ transposition * season + transposition * portion, 
                                 data = o_niloticus, permutations = 9999)

# Calcular Bray-Curtis - P. brevis
#dist_bc_p_brevis <- vegdist(select(p_brevis, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
#permanova_p_brevis <- adonis2(dist_bc_p_brevis ~ transposition + season + portion, data = p_brevis, permutations = 1000)

# Calcular Bray-Curtis - P. fasciatus
dist_bc_p_fasciatus <- vegdist(select(p_fasciatus, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_p_fasciatus <- adonis2(dist_bc_p_fasciatus ~ transposition * season + transposition * portion, 
                                 data = p_fasciatus, permutations = 9999)

# Calcular Bray-Curtis - P. kennedyi
#dist_bc_p_kennedyi <- vegdist(select(p_kennedyi, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
#permanova_p_kennedyi <- adonis2(dist_bc_p_kennedyi ~ transposition + season + portion, data = p_kennedyi, permutations = 1000)

# Calcular Bray-Curtis - P. vivipara
dist_bc_p_vivipara <- vegdist(select(p_vivipara, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_p_vivipara <- adonis2(dist_bc_p_vivipara ~ transposition * season + transposition * portion, 
                                data = p_vivipara, permutations = 9999)

# Calcular Bray-Curtis - S. brandtii
dist_bc_s_brandtii <- vegdist(select(s_brandtii, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_s_brandtii <- adonis2(dist_bc_s_brandtii ~ transposition * season + transposition * portion, 
                                data = s_brandtii, permutations = 9999)

# Calcular Bray-Curtis - S. brasiliensis
dist_bc_s_brasiliensis <- vegdist(select(s_brasiliensis, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_s_brasiliensis <- adonis2(dist_bc_s_brasiliensis ~ transposition * season, 
                                    data = s_brasiliensis, permutations = 9999)

# Calcular Bray-Curtis - S. heterodon
dist_bc_s_heterodon <- vegdist(select(s_heterodon, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_s_heterodon <- adonis2(dist_bc_s_heterodon ~ transposition * season + transposition * portion, 
                                 data = s_heterodon, permutations = 9999)

# Calcular Bray-Curtis - S. notonota
#dist_bc_s_notonota <- vegdist(select(s_notonota, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
#permanova_s_notonota <- adonis2(dist_bc_s_notonota ~ transposition + season + portion, data = s_notonota, permutations = 1000)

# Calcular Bray-Curtis - S. piaba
dist_bc_s_piaba <- vegdist(select(s_piaba, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_s_piaba <- adonis2(dist_bc_s_piaba ~ transposition * season + transposition * portion, 
                             data = s_piaba, permutations = 9999)

# Calcular Bray-Curtis - T. signatus
dist_bc_t_signatus <- vegdist(select(t_signatus, -all_of(relevant_cols)), method = "bray")

# PERMANOVA 
permanova_t_signatus <- adonis2(dist_bc_t_signatus ~ transposition * season + transposition * portion, 
                                data = t_signatus, permutations = 9999)
