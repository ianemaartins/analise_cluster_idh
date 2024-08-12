# Carregar pacotes
library(remotes)
library(microdatasus)
library(dplyr)
library(tidyr)
library(stringr) 
library(ggplot2) 
library(lubridate)

# Baixar e processar a base de dados
#remotes::install_github("rfsaldanha/microdatasus")

# Baixar, processar e salvar dados
for (ano in 2013:2022) {
  dados <- fetch_datasus(year_start = ano, year_end = ano, information_system = "SIM-DO")
  dados <- process_sim(dados)
  dados <- dados %>%
    select(CAUSABAS, CODMUNOCOR, CODMUNRES, DTNASC, DTOBITO, IDADE, RACACOR, SEXO, ESTCIV, ESC, OCUP, LOCOCOR)
  saveRDS(dados, paste0("dados_", ano,'.rds'))
  }


