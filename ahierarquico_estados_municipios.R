# ESTADOS

# Loop para filtrar os dados por ano, realizar a clusterização hierárquica, plotar e salvar os dendrogramas
for (ano in 2013:2022) {
  
  # Filtrar os dados por ano
  dados_ano_est <- dados_idh_estados %>%
    filter(ANOOBITO == ano)
  
  # Escalonar as variáveis IDH e taxa de mortalidade
  dados_cluster_ano_est <- dados_ano_est %>%
    select(mortalidade_C2010, IDHM) %>%
    mutate(across(everything(), as.numeric)) %>%
    scale()
  
  # Calcular a matriz de distância
  dist_ano_est <- dist(dados_cluster_ano_est, method = "euclidean")
  
  # Realizar a clusterização hierárquica
  hc_ano_est <- hclust(dist_ano_est, method = "ward.D2")
  
  # Plotar e salvar o dendrograma
  png(filename = paste0("dendrograma_est_", ano, ".png"))
  plot(hc_ano_est, labels = dados_ano_est$Estado, 
       main = paste("Dendrograma de Clusterização Hierárquica (Estados) -", ano), 
       xlab = "", ylab = "Altura")
  rect.hclust(hc_ano_est, k = 3, border = 2:6)
  dev.off()
  
  # Adicionar os clusters ao data frame original
  dados_ano_est$cluster_hc <- cutree(hc_ano_est, k = 3)
  
  # Criar o gráfico com Plotly
  p_hc_est <- plot_ly(
    data = dados_ano_est,
    x = ~IDHM,
    y = ~mortalidade_C2010,
    color = ~as.factor(cluster_hc),
    colors = "Set1",
    type = 'scatter',
    mode = 'markers',
    text = ~Sigla, 
    hoverinfo = 'text',
    marker = list(size = 10)
  ) %>%
    layout(
      title = paste("Hierárquica: IDH e Taxa de Mortalidade dos Estados -", ano),
      xaxis = list(title = "IDH"),
      yaxis = list(title = "Taxa de Mortalidade")
    )
  

  print(p_hc_est)
  
  # Salvar os gráficos interativos como html 
  htmlwidgets::saveWidget(as_widget(p_hc_est), paste0("hierarquico2010_est_", ano, ".html"))
}





# MUNICÍPIOS

for (ano in 2013:2022) {
  
  # Filtrar os dados por ano
  dados_ano_mun <- dados_idh_municipios %>%
    filter(ANOOBITO == ano)
  
  # Escalonar as variáveis IDH e taxa de mortalidade
  dados_cluster_ano_mun <- dados_ano_mun %>%
    select(mortalidade_mun_C2010, IDHM) %>%
    mutate(across(everything(), as.numeric)) %>%
    scale()
  
  # Calcular a matriz de distância
  dist_ano_mun <- dist(dados_cluster_ano_mun, method = "euclidean")
  
  # Realizar a clusterização hierárquica
  hc_ano_mun <- hclust(dist_ano_mun, method = "ward.D2")
  
  # Plotar e salvar o dendrograma
  png(filename = paste0("dendrograma_mun_", ano, ".png"))
  plot(hc_ano_mun, labels = dados_ano_mun$Municipio, 
       main = paste("Dendrograma de Clusterização Hierárquica (Municípios) -", ano), 
       xlab = "", ylab = "Altura")
  rect.hclust(hc_ano_mun, k = 3, border = 2:4)
  dev.off()
  
  # Adicionar os clusters ao data frame original
  dados_ano_mun$cluster_hc_mun <- cutree(hc_ano_mun, k = 3)
  
  # Criar o gráfico com Plotly
  p_hc_mun <- plot_ly(
    data = dados_ano_mun,
    x = ~IDHM,
    y = ~mortalidade_mun_C2010,
    color = ~as.factor(cluster_hc_mun),
    colors = "Set1",
    type = 'scatter',
    mode = 'markers',
    text = ~CIDADE, 
    hoverinfo = 'text',
    marker = list(size = 10)
  ) %>%
    layout(
      title = paste("Hierárquica: IDH e Taxa de Mortalidade dos Municípios -", ano),
      xaxis = list(title = "IDH"),
      yaxis = list(title = "Taxa de Mortalidade")
    )
  
  
  print(p_hc_mun)
  
  # Salvar os gráficos interativos como html 
  htmlwidgets::saveWidget(as_widget(p_hc_mun), paste0("hierarquico2010_mun_", ano, ".html"))
}




# DÉCADA

# Desagrupar o data frame original 
dados_idh_municipios_decada <- dados_idh_municipios %>%
  ungroup()

# Agregar os dados para toda a década
dados_decada_mun <- dados_idh_municipios_decada %>%
  group_by(CIDADE) %>%
  summarise(
    mortalidade_tot_decada_C2010 = sum(mortalidade_tot_C2010, na.rm = TRUE),
    IDHM_decada = first(IDHM)  
  ) %>%
  ungroup()

# Certificar que as colunas são numéricas
dados_decada_mun <- dados_decada_mun %>%
  mutate(
    mortalidade_tot_decada_C2010 = as.numeric(mortalidade_tot_decada_C2010),
    IDHM_decada = as.numeric(IDHM_decada)
  )

# Escalonar as variáveis IDH e taxa de mortalidade da década
dados_cluster_decada_mun <- dados_decada_mun %>%
  select(mortalidade_tot_decada_C2010, IDHM_decada) %>%
  scale()

# Calcular a matriz de distância
dist_decada_mun <- dist(dados_cluster_decada_mun, method = "euclidean")

# Realizar a clusterização hierárquica
hc_decada_mun <- hclust(dist_decada_mun, method = "ward.D2")

# Plotar e salvar o dendrograma
png(filename = "dendrograma_mun_decada_2013_2022.png")
plot(hc_decada_mun, labels = dados_decada_mun$CIDADE, 
     main = "Hierárquica: Dendrograma dos Municípios - Década 2013-2022", 
     xlab = "", ylab = "Altura")
rect.hclust(hc_decada_mun, k = 3, border = 2:4)
dev.off()

# Adicionar os clusters ao data frame original
dados_decada_mun <- dados_decada_mun %>%
  mutate(cluster_hc_decada_mun = cutree(hc_decada_mun, k = 3))

# Criar o gráfico da década com Plotly
p_decada_hc_mun <- plot_ly(
  data = dados_decada_mun,
  x = ~IDHM_decada,
  y = ~mortalidade_tot_decada_C2010,
  color = ~as.factor(cluster_hc_decada_mun),
  colors = "Set1",
  type = 'scatter',
  mode = 'markers',
  text = ~CIDADE,  
  hoverinfo = 'text',
  marker = list(size = 10)
) %>%
  layout(
    title = "Hierárquica: IDH e Taxa de Mortalidade dos Municípios do ES - Década 2013-2022",
    xaxis = list(title = "IDH"),
    yaxis = list(title = "Taxa de Mortalidade (Total da Década)")
  )


print(p_decada_hc_mun)

# Salvar o gráfico interativo da década como html 
htmlwidgets::saveWidget(as_widget(p_decada_hc_mun), "hierarquico_dec_mun.html")

