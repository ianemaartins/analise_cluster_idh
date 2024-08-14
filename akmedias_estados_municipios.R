#library(dplyr)
#library(ggplot2)
#library(plotly)



# ESTADOS

# Desagrupar o data frame
dados_idh_estados <- dados_idh_estados %>%
  ungroup()

# Escalonar as variáveis IDH 2010 x taxa de mortalidade 2010
dados_cluster_est <- dados_idh_estados %>%
  select(mortalidade_C2010, IDHM) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale()

# Calcular o total within sum of squares (wss) para diferentes valores de k
wss_est <- (nrow(dados_cluster_est)-1)*sum(apply(dados_cluster_est, 2, var))
for (i in 2:15) wss_est[i] <- sum(kmeans(dados_cluster_est, centers=i, nstart=25)$tot.withinss)

# Plotar o gráfico para o método do cotovelo
plot(1:15, wss_est, type="b", xlab="Número de Clusters (k)", ylab="WSS (within sum of squares)")

# Executar K-means com o número de clusters determinado
resultado_kmedia_est <- kmeans(dados_cluster_est, centers = 4, nstart=25)

# Adicionar os clusters no data frame original
dados_idh_estados <- dados_idh_estados %>%
  mutate(cluster_km = resultado_kmedia_est$cluster)

# Visualizar os clusters baseados em IDH e taxa de mortalidade
ggplot(dados_idh_estados, aes(x = IDHM, y = mortalidade_C2010, color = as.factor(cluster_km))) +
  geom_point(size = 2) +
  labs(color = "Cluster") +
  theme_minimal() +
  ggtitle("K-médias: IDH dos Estados Brasileiros e Taxa de Mortalidade")

# Filtrar pelos anos e plotar
for (ano in 2013:2022) {
  dados_ano <- dados_idh_estados %>%
    filter(ANOOBITO == ano)
  
  # Criar o gráfico com Plotly
  p_est <- plot_ly(
    data = dados_ano,
    x = ~IDHM,
    y = ~mortalidade_C2010,
    color = ~as.factor(cluster_km),
    colors = "Set1",
    type = 'scatter',
    mode = 'markers',
    text = ~Sigla, 
    hoverinfo = 'text',
    marker = list(size = 10)
  ) %>%
    layout(
      title = paste("K-médias: IDH e mortalidade por psicoativos em", ano),
      xaxis = list(title = "IDH"),
      yaxis = list(title = "Taxa de Mortalidade")
    )
  
  
  print(p_est)
  
  # Salvar os gráficos interativos como html 
  htmlwidgets::saveWidget(as_widget(p), paste0("kmedia2010_est_", ano, ".html"))
}
  
  





# MUNICIPIOS

# Desagrupar o data frame
dados_idh_municipios <- dados_idh_municipios %>%
  ungroup()

# Escalonar as variáveis IDH 2010 x taxa de mortalidade da decada
dados_cluster_mun <- dados_idh_municipios %>%
  select(mortalidade_mun_C2010, IDHM) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale()

# Calcular o total within sum of squares (wss) 
wss_mun <- (nrow(dados_cluster_mun)-1)*sum(apply(dados_cluster_mun, 2, var))
for (i in 2:15) wss_mun[i] <- sum(kmeans(dados_cluster_mun, centers=i, nstart=25)$tot.withinss)

# Plotar o gráfico para o método do cotovelo
plot(1:15, wss_mun, type="b", xlab="Número de Clusters (k)", ylab="WSS (within sum of squares)")

# Executar K-means com o número de clusters dado
resultado_kmedia_mun <- kmeans(dados_cluster_mun, centers = 4, nstart=25)

# Adicionar os rótulos dos clusters ao data frame original
dados_idh_municipios <- dados_idh_municipios %>%
  mutate(cluster_km_mun = resultado_kmedia_mun$cluster)

# Filtrar pelos anos e plotar
for (ano in 2013:2022) {
  dados_ano_mun <- dados_idh_municipios %>%
    filter(ANOOBITO == ano)
  
  # Criar o gráfico interativo com Plotly
  p_mun <- plot_ly(
    data = dados_ano_mun,
    x = ~IDHM,
    y = ~mortalidade_mun_C2010,
    color = ~as.factor(cluster_km_mun),
    colors = "Set1",
    type = 'scatter',
    mode = 'markers',
    text = ~CIDADE,  
    hoverinfo = 'text',
    marker = list(size = 10)
  ) %>%
    layout(
      title = paste("K-médias: IDH dos municípios e mortalidade por psicoativos em", ano),
      xaxis = list(title = "IDH"),
      yaxis = list(title = "Taxa de Mortalidade")
    )
  
  print(p_mun)
  
  # Salvar os gráficos interativos como html 
  htmlwidgets::saveWidget(as_widget(p_mun), paste0("kmedia2010_mun_", ano, ".html"))
}






#DECADA

# Agregar os dados para toda a década
dados_decada_mun <- dados_idh_municipios %>%
  group_by(CIDADE) %>%
  summarise(
    mortalidade_mun_C2010 = sum(mortalidade_mun_C2010, na.rm = TRUE),
    IDHM = first(IDHM) 
  ) %>%
  ungroup()

# Certificar que as colunas são numéricas
dados_decada_mun <- dados_decada_mun %>%
  mutate(
    mortalidade_mun_C2010 = as.numeric(mortalidade_mun_C2010),
    IDHM = as.numeric(IDHM)
  )

# Escalonar as variáveis IDH e taxa de mortalidade da década
dados_cluster_mun_dec <- dados_decada_mun %>%
  select(mortalidade_mun_C2010, IDHM) %>%
  scale()

# Calcular o total within sum of squares (wss) 
wss_mun <- (nrow(dados_cluster_mun_dec)-1)*sum(apply(dados_cluster_mun_dec, 2, var))
for (i in 2:15) wss_mun[i] <- sum(kmeans(dados_cluster_mun_dec, centers=i, nstart=25)$tot.withinss)

# Plotar o gráfico para o método do cotovelo
plot(1:15, wss_mun, type="b", xlab="Número de Clusters (k)", ylab="WSS (within sum of squares)")

# Executar K-means com o número de clusters dado
resultado_kmedia_mun_dec <- kmeans(dados_cluster_mun_dec, centers = 4, nstart=25)

# Adicionar os rótulos dos clusters ao data frame original
dados_decada_mun <- dados_decada_mun %>%
  mutate(cluster_km_mun_dec = resultado_kmedia_mun_dec$cluster)

# Criar o gráfico com Plotly
p_decada_mun <- plot_ly(
  data = dados_decada_mun,
  x = ~IDHM,
  y = ~mortalidade_mun_C2010,
  color = ~as.factor(cluster_km_mun_dec),
  colors = "Set1",
  type = 'scatter',
  mode = 'markers',
  text = ~CIDADE,  
  hoverinfo = 'text',
  marker = list(size = 10)
) %>%
  layout(
    title = "K-médias: IDH dos Municípios do ES e Taxa de Mortalidade (2013-2022)",
    xaxis = list(title = "IDH"),
    yaxis = list(title = "Taxa de Mortalidade (Total da Década)")
  )


print(p_decada_mun)

# Salvar o gráfico interativo da década como html 
htmlwidgets::saveWidget(as_widget(p_decada_mun), "kmedia_decada_mun.html")

