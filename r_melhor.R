# ============================
# Instalação e carregamento de pacotes
# ============================
pacotes <- c("janitor", "gt", "forcats", "dplyr", "ggthemes", "viridis", "tidyverse", "scales", "stringr", "readr", "ggplot2")
pacotes_instalados <- pacotes %in% rownames(installed.packages())
if(any(!pacotes_instalados)) install.packages(pacotes[!pacotes_instalados])

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(janitor)
library(scales)
library(viridis)
library(gt)
library(ggthemes)
library(tidyverse)

# ============================
# Carregamento de dados
# ============================
caminho_base <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/tabelas"
arquivo_alunos <- file.path(caminho_base, "alunos-final.csv")

dados <- read_delim(arquivo_alunos, delim = ",", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# Conferir estrutura
head(dados, 10)
str(dados)
summary(dados)
colnames(dados)
View(dados)

# ============================
# Filtrar estudantes 2011-2023
# ============================
dados$`Período de Ingresso` <- as.numeric(dados$`Período de Ingresso`)

dados_filtrados <- dados %>%
  filter(`Período de Ingresso` >= 2011 & `Período de Ingresso` <= 2023)

cat("Total antes do filtro:", nrow(dados), "\n")
cat("Total após o filtro (2011 a 2023):", nrow(dados_filtrados), "\n")

# ===========================
# Taxa de Evasão Por Currículo
# ===========================

# Remover alunos ativos
dados_sem_ativos <- dados_filtrados %>%
  filter(Status != "Ativo")

# Calcular evasão por currículo
evasao_por_curriculo <- dados_sem_ativos %>%
  group_by(Currículo) %>%
  summarise(
    Evadidos = sum(Status == "Evadido"),
    Graduados = sum(Status == "Graduado"),
    Total = Evadidos + Graduados,
    Taxa_Evasao = ifelse(Total > 0, round(Evadidos / Total, 3), NA)
  ) %>%
  filter(Currículo %in% c(1999, 2017))  # manter apenas os currículos desejados

print(evasao_por_curriculo)

# ===========================
# Filtrando apenas os inativos
# ===========================


dados_inativos <- dados_filtrados %>%
  filter(Status == "INATIVO")

# Resumindo por currículo
resumo_evasao <- dados_inativos %>%
  mutate(Grupo = ifelse(`Tipo de Evasão` == "GRADUADO", "Graduado", "Evadido")) %>%
  group_by(Currículo, Grupo) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Grupo, values_from = Quantidade, values_fill = 0) %>%
  mutate(
    Total = Evadido + Graduado,
    Taxa_Evasao = round((Evadido / Total) * 100, 2)
  )

print(resumo_evasao)
