#Analise de cluster IDH dos Estados x taxa de mortalidade por estado



#Desagrupar o data frame
dados_idh_estados <- dados_idh_estados %>%
  ungroup()

#Escalonar as variaveis IDH 2010 x taxa de mortalidade 2010
dados_cluster_est <- dados_idh_estados %>%
  select(mortalidade_C2010, IDHM) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale()


# ----REVISAR---- Numero de clusters - metodo cotovelo 

# Calcular o total within sum of squares (wss) para diferentes valores de k
wss <- (nrow(dados_cluster_est)-1)*sum(apply(dados_cluster_est, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(dados_cluster_est, centers=i)$tot.withinss)

# Plotar o gráfico para o método do cotovelo
plot(1:15, wss, type="b", xlab="Número de Clusters (k)", ylab="WSS (within sum of squares)")





# K-MEDIAS 

  resultado_kmedia <- kmeans(dados_cluster_est, centers = 4)
  
  # Adicionar os rótulos dos clusters ao data frame original
  dados_idh_estados <- dados_idh_estados %>%
    mutate(cluster_km = resultado_kmedia$cluster)
  
  # Visualizar os clusters baseados em IDH e taxa de mortalidade
  ggplot(dados_idh_estados, aes(x = IDHM, y = mortalidade_C2010, color = as.factor(cluster_km))) +
    geom_point(size = 2) +
    labs(color = "Cluster") +
    theme_minimal() +
    ggtitle("Clusters de Estados Baseados no IDH e Taxa de Mortalidade (2010)")
  
  #Filtrar pelos anos
  for (ano in 2013:2022) {
  dados_ano <- dados_idh_estados %>%
    filter(ANOOBITO == ano)
  
  # Criar o gráfico
  kmedia2010_est13a22 <- ggplot(dados_ano, aes(x = IDHM, y = mortalidade_C2010, color = as.factor(cluster_km))) +
    geom_point(size = 3) +
    labs(color = "Cluster") +
    theme_minimal() +
    ggtitle(paste("Clusters de Estados em", ano))
  
  # Exibir o gráfico
  print(kmedia2010_est13a22)
  
  #Salvar os gráficos
  ggsave(filename = paste0("kmedia2010_est_", ano, ".png"), plot = kmedia2010_est13a22)
  }

  
  

#HIERARQUICA

  # Filtrar os dados para o ano específico
  for (ano in 2013:2022) {
    dados_ano <- dados_idh_estados %>%
      filter(ANOOBITO == ano)

  # Calcular a matriz de distância
  dist_matrix <- dist(dados_ano %>% select(mortalidade_C2010, IDHM) %>% scale())
  
  # Aplicar a clusterização hierárquica usando o método Ward.D2
  hc <- hclust(dist_matrix, method = "ward.D2")
  
  # Cortar o dendrograma para obter 4 clusters
  clusters_hc <- cutree(hc, k = 4)
  
  # Adicionar os clusters ao data frame filtrado
  dados_ano <- dados_ano %>%
    mutate(cluster_hc = clusters_hc)
  
  # Plotar o dendrograma (opcional)
  plot(hc, labels = dados_ano$Sigla, main = paste("Dendrograma de Clusterização Hierárquica -", ano))
  
  # Visualizar os clusters
  hc2010_est13a22 <- ggplot(dados_ano, aes(x = IDHM, y = mortalidade_C2010, color = as.factor(cluster_hc))) +
    geom_point(size = 3) +
    labs(color = "Cluster") +
    theme_minimal() +
    ggtitle(paste("Clusters de Estados Baseados em Clusterização Hierárquica -", ano))
  
    print(hc2010_est13a22)
  
  
  #Salvar os gráficos
  ggsave(filename = paste0("hc2010_est_", ano, ".png"), plot = hc2010_est13a22)
  }

  
  
  
  
#DBSCAN
  
  # Filtrar os dados para o ano específico
  for (ano in 2013:2022) {
    dados_ano <- dados_idh_estados %>%
      filter(ANOOBITO == ano)
    
    # Escalonar as variáveis antes de aplicar o DBSCAN
    dados_cluster_ano <- dados_ano %>%
      select(mortalidade_C2010, IDHM) %>%
      scale()
    
    # Aplicar DBSCAN
    dbscan_result <- dbscan(dados_cluster_ano, eps = 0.5, minPts = 5)
    
    # Adicionar os clusters ao data frame filtrado
    dados_ano <- dados_ano %>%
      mutate(cluster_dbscan = dbscan_result$cluster)
    
    # Visualizar os clusters
    dbscan2010_est13a22 <- ggplot(dados_ano, aes(x = IDHM, y = mortalidade_C2010, color = as.factor(cluster_dbscan))) +
      geom_point(size = 3) +
      labs(color = "Cluster") +
      theme_minimal() +
      ggtitle(paste("Clusters de Estados Baseados em DBSCAN -", ano))
    
    print(dbscan2010_est13a22)
  
    #Salvar os gráficos
    ggsave(filename = paste0("dbscan2010_est_", ano, ".png"), plot = dbscan2010_est13a22)
  }
  