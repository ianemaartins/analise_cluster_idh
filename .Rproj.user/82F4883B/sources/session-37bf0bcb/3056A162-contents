#library(dbscan)

#ESTADOS

# Definir parâmetros para o DBSCAN
eps_value <- 0.75  # Distância máxima entre dois pontos para ser considerado parte de um cluster
minPts_value <- 2  # Número mínimo de pontos necessários para formar um cluster

# Loop para aplicar DBSCAN por ano para estados
for (ano in 2013:2022) {
  
  # Filtrar os dados por ano
  dados_ano_est <- dados_idh_estados %>%
    filter(ANOOBITO == ano)
  
  # Escalonar as variáveis IDH e taxa de mortalidade
  dados_cluster_ano_est <- dados_ano_est %>%
    select(mortalidade_C2010, IDHM) %>%
    mutate(across(everything(), as.numeric)) %>%
    scale() %>%
    as.data.frame()
  
  # Aplicar o DBSCAN 
  dbscan_result_est <- dbscan(dados_cluster_ano_est, eps = eps_value, minPts = minPts_value)
  
  # Adicionar os clusters ao data frame original
  dados_ano_est$cluster_dbscan <- dbscan_result_est$cluster
  
  # Visualizar os clusters para o ano específico
  cluster_plot_dbscan_est <- ggplot(dados_ano_est, aes(x = IDHM, y = mortalidade_C2010, color = as.factor(cluster_dbscan))) +
    geom_point(size = 2) +
    labs(color = "Cluster", y = "Taxa de Mortalidade", x = "IDH") +
    theme_minimal() +
    ggtitle(paste("DBSCAN: IDH e Taxa de Mortalidade (Estado) -", ano))
  
  # Exibir o gráfico
  print(cluster_plot_dbscan_est)
  
  # Salvar o gráfico dos clusters como um arquivo PNG
  ggsave(filename = paste0("dbscan_est_", ano, ".png"), plot = cluster_plot_dbscan_est)
}






#MUNICIPIOS

# Loop para aplicar DBSCAN por ano para municípios
for (ano in 2013:2022) {
  
  # Filtrar os dados por ano
  dados_ano_mun <- dados_idh_municipios %>%
    filter(ANOOBITO == ano)
  
  # Escalonar as variáveis IDH e taxa de mortalidade
  dados_cluster_ano_mun <- dados_ano_mun %>%
    select(mortalidade_mun_C2010, IDHM) %>%
    mutate(across(everything(), as.numeric)) %>%
    scale()
  
  # Aplicar o DBSCAN
  dbscan_result_mun <- dbscan(dados_cluster_ano_mun, eps = eps_value, minPts = minPts_value)
  
  # Adicionar os clusters ao data frame original
  dados_ano_mun$cluster_dbscan_mun <- dbscan_result_mun$cluster
  
  # Visualizar os clusters para o ano específico
  cluster_plot_dbscan_mun <- ggplot(dados_ano_mun, aes(x = IDHM, y = mortalidade_mun_C2010, color = as.factor(cluster_dbscan_mun))) +
    geom_point(size = 2) +
    labs(color = "Cluster", y = "Taxa de Mortalidade", x = "IDH") +
    theme_minimal() +
    ggtitle(paste("DBSCAN: IDH e Taxa de Mortalidade -", ano))
  
  # Exibir o gráfico
  print(cluster_plot_dbscan_mun)
  
  # Salvar o gráfico dos clusters como um arquivo PNG
  ggsave(filename = paste0("dbscan_mun_", ano, ".png"), plot = cluster_plot_dbscan_mun)
}




#DECADA

# Agregar os dados para toda a década
dados_decada <- dados_idh_municipios %>%
  group_by(CIDADE) %>%
  summarise(
    mortalidade_tot_C2010 = sum(mortalidade_tot_C2010, na.rm = TRUE),
    IDHM = first(IDHM)  # Supondo que o IDHM não muda significativamente ao longo da década
  ) %>%
  ungroup()

# Certificar que as colunas são numéricas
dados_decada <- dados_decada %>%
  mutate(
    mortalidade_tot_C2010 = as.numeric(mortalidade_tot_C2010),
    IDHM = as.numeric(IDHM)
  )

# Escalonar as variáveis IDH e taxa de mortalidade
dados_cluster_decada <- dados_decada %>%
  select(mortalidade_tot_C2010, IDHM) %>%
  scale()

# Aplicar o DBSCAN
eps_value <- 0.75  # Ajuste este valor conforme necessário
minPts_value <- 2  # Ajuste este valor conforme necessário

dbscan_result_decada <- dbscan(dados_cluster_decada, eps = eps_value, minPts = minPts_value)

# Adicionar os clusters ao data frame original
dados_decada$cluster_dbscan <- dbscan_result_decada$cluster

# Visualizar os clusters para toda a década
cluster_plot_dbscan_decada <- ggplot(dados_decada, aes(x = IDHM, y = mortalidade_tot_C2010, color = as.factor(cluster_dbscan))) +
  geom_point(size = 2) +
  labs(color = "Cluster", y = "Taxa de Mortalidade (Total da Década)", x = "IDH") +
  theme_minimal() +
  ggtitle("DBSCAN: IDH e Taxa de Mortalidade por Psicoativos - Década 2013-2022")

# Exibir o gráfico
print(cluster_plot_dbscan_decada)

# Salvar o gráfico dos clusters como um arquivo PNG
ggsave(filename = "dbscan_mun_decada_2013_2022.png", plot = cluster_plot_dbscan_decada)
