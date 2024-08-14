#library(dbscan)
#library(dplyr)
#library(ggplot2)
#library(plotly)

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
  
  # Criar o gráfico com Plotly
  p_dbscan_est <- plot_ly(
    data = dados_ano_est,
    x = ~IDHM,
    y = ~mortalidade_C2010,
    color = ~as.factor(cluster_dbscan),
    colors = "Set1",
    type = 'scatter',
    mode = 'markers',
    text = ~Sigla, 
    hoverinfo = 'text',
    marker = list(size = 10)
  ) %>%
    layout(
      title = paste("DBSCAN: IDH e Taxa de Mortalidade (Estado) -", ano),
      xaxis = list(title = "IDH"),
      yaxis = list(title = "Taxa de Mortalidade")
    )

  
  print(p_dbscan_est)

  # Salvar os gráficos interativos como html 
  htmlwidgets::saveWidget(as_widget(p_dbscan_est), paste0("dbscan2010_est_", ano, ".html"))
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
  
  # Criar o gráfico com Plotly
  p_dbscan_mun <- plot_ly(
    data = dados_ano_mun,
    x = ~IDHM,
    y = ~mortalidade_mun_C2010,
    color = ~as.factor(cluster_dbscan_mun),
    colors = "Set1",
    type = 'scatter',
    mode = 'markers',
    text = ~CIDADE, 
    hoverinfo = 'text',
    marker = list(size = 10)
  ) %>%
    layout(
      title = paste("DBSCAN: IDH e Taxa de Mortalidade (Municípios) -", ano),
      xaxis = list(title = "IDH"),
      yaxis = list(title = "Taxa de Mortalidade")
    )
  

  print(p_dbscan_mun)
  
  # Salvar os gráficos interativos como html 
  htmlwidgets::saveWidget(as_widget(p_dbscan_mun), paste0("dbscan2010_mun_", ano, ".html"))
}




#DECADA

# Agregar os dados para toda a década
dados_decada <- dados_idh_municipios %>%
  group_by(CIDADE) %>%
  summarise(
    mortalidade_tot_C2010 = sum(mortalidade_tot_C2010, na.rm = TRUE),
    IDHM = first(IDHM) 
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
eps_value <- 0.75  
minPts_value <- 2  

dbscan_result_decada <- dbscan(dados_cluster_decada, eps = eps_value, minPts = minPts_value)

# Adicionar os clusters ao data frame original
dados_decada$cluster_dbscan <- dbscan_result_decada$cluster

# Criar o gráfico da década com Plotly
p_dbscan_decada <- plot_ly(
  data = dados_decada,
  x = ~IDHM,
  y = ~mortalidade_tot_C2010,
  color = ~as.factor(cluster_dbscan),
  colors = "Set1",
  type = 'scatter',
  mode = 'markers',
  text = ~CIDADE,  
  hoverinfo = 'text',
  marker = list(size = 10)
) %>%
  layout(
    title = "DBSCAN: IDH e Taxa de Mortalidade por Psicoativos - Década 2013-2022",
    xaxis = list(title = "IDH"),
    yaxis = list(title = "Taxa de Mortalidade (Total da Década)")
  )


print(p_dbscan_decada)

# Salvar o gráfico interativo da década como html 
htmlwidgets::saveWidget(as_widget(p_dbscan_decada), "dbscan_mun_decada.html")
