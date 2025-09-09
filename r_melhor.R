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

# =====

resumo_evasao <- dados_filtrados %>%
  # Mantém apenas alunos inativos
  filter(Status == "INATIVO") %>%
  # Marca se é graduado ou evadido
  mutate(Grupo = ifelse(`Tipo de Evasão` == "GRADUADO", "Graduado", "Evadido")) %>%
  # Filtra apenas currículos 1999 e 2017
  filter(Currículo %in% c(1999, 2017)) %>%
  group_by(Currículo, Grupo) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Grupo, values_from = Quantidade, values_fill = 0) %>%
  mutate(
    Total = Evadido + Graduado,
    Taxa_Evasao = round((Evadido / Total) * 100, 2)
  )

print(resumo_evasao)

# ==============
# Criando o gráfico de barras com legendas personalizadas
ggplot(resumo_evasao, aes(x = factor(Currículo), y = Taxa_Evasao, fill = factor(Currículo))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Taxa_Evasao, "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(
    values = c("1999" = "#1E90FF",   # Laranja
               "2017" = "#FFA500"),  # Azul mais escuro
    labels = c("1999" = "Currículo 1999", 
               "2017" = "Currículo 2017")
  ) +
  labs(
    title = "Taxa de Evasão por Currículo (Alunos Inativos)",
    x = "Currículo",
    y = "Taxa de Evasão (%)",
    fill = "Legenda"
  ) +
  theme_minimal(base_size = 14)

# =======================
# Alunos concluintes

# Filtrar apenas alunos graduados (concluintes) e currículos de interesse
graduados_resumo <- dados_filtrados %>%
  filter(Status == "INATIVO", `Tipo de Evasão` == "GRADUADO") %>%
  filter(Currículo %in% c(1999, 2017)) %>%
  group_by(Currículo) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Percentual = round((Quantidade / sum(Quantidade)) * 100, 2))

print(graduados_resumo)

# Gráfico de barras
ggplot(graduados_resumo, aes(x = factor(Currículo), y = Percentual, fill = factor(Currículo))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Percentual, "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("1999" = "#6699CC", "2017" = "#FFCC66")) +
  labs(
    title = "Distribuição de Alunos Graduados por Currículo",
    x = "Currículo",
    y = "Percentual de Concluintes (%)",
    fill = "Currículo"
  ) +
  theme_minimal(base_size = 14)


# ======================
# Filtrar apenas alunos graduados (concluintes) e currículos de interesse

graduados_resumo <- dados_filtrados %>%
  filter(Status == "INATIVO", `Tipo de Evasão` == "GRADUADO") %>%
  filter(Currículo %in% c(1999, 2017)) %>%
  group_by(Currículo) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Percentual = round((Quantidade / sum(Quantidade)) * 100, 1))  # arredonda 1 casa

print(graduados_resumo)

# Gráfico de barras com cores e legendas personalizadas
ggplot(graduados_resumo, aes(x = factor(Currículo), y = Percentual, fill = factor(Currículo))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(Percentual, "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(
    values = c("1999" = "#1E90FF",  # Laranja
               "2017" = "#FFA500"), # Azul mais escuro
    labels = c("1999" = "Currículo 1999", 
               "2017" = "Currículo 2017")
  ) +
  labs(
    title = "Distribuição de Alunos Graduados por Currículo",
    x = "Currículo",
    y = "Percentual de Concluintes (%)",
    fill = "Legenda"
  ) +
  theme_minimal(base_size = 14)

# =======================
#
# Análise de Idade por Currículo
idade_resumo <- dados_filtrados %>%
  filter(Currículo %in% c(1999, 2017)) %>%
  group_by(Currículo) %>%
  summarise(
    Min = min(`Idade Aproximada no Ingresso`, na.rm = TRUE),
    Max = max(`Idade Aproximada no Ingresso`, na.rm = TRUE),
    Media = round(mean(`Idade Aproximada no Ingresso`, na.rm = TRUE), 1),
    Mediana = median(`Idade Aproximada no Ingresso`, na.rm = TRUE),
    .groups = "drop"
  )

print(idade_resumo)

# Gráfico boxplot comparativo
ggplot(dados_filtrados %>% filter(Currículo %in% c(1999, 2017)),
       aes(x = factor(Currículo), y = `Idade Aproximada no Ingresso`, fill = factor(Currículo))) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("1999" = "#FFA500", "2017" = "#1E90FF"),
    labels = c("1999" = "Currículo 1999", "2017" = "Currículo 2017")
  ) +
  labs(
    title = "Distribuição da Idade no Ingresso por Currículo",
    x = "Currículo",
    y = "Idade no Ingresso",
    fill = "Legenda"
  ) +
  theme_minimal(base_size = 14)

# ========================

library(dplyr)

# Função para converter período AAAA.P em decimal (ex: 2011.1 -> 2011.5, 2011.2 -> 2011.8)
convert_periodo <- function(periodo) {
  ano <- as.numeric(substr(periodo, 1, 4))
  sem <- as.numeric(substr(periodo, 6, 6))
  return(ifelse(sem == 1, ano + 0.5, ano + 0.8))
}

dados_filtrados <- dados %>%
  mutate(
    ingresso_num = convert_periodo(`Período de Ingresso`),
    evasao_num = convert_periodo(`Período de Evasão`)
  )

# Calcular diferença em semestres
dados_filtrados <- dados_filtrados %>%
  mutate(dif = evasao_num - ingresso_num)

# Definir janelas de evasão (em anos aproximados: 0.5 ~ 1 semestre)
dados_filtrados <- dados_filtrados %>%
  mutate(
    periodo_evasao = case_when(
      Status == "INATIVO" & `Tipo de Evasão` != "GRADUADO" & dif <= 0.5 ~ "1º período",
      Status == "INATIVO" & `Tipo de Evasão` != "GRADUADO" & dif <= 1.0 ~ "2º período",
      Status == "INATIVO" & `Tipo de Evasão` != "GRADUADO" & dif <= 1.5 ~ "3º período",
      Status == "INATIVO" & `Tipo de Evasão` != "GRADUADO" & dif <= 2.0 ~ "4º período",
      TRUE ~ NA_character_
    )
  )

# Filtrar somente quem teve evasão nos 4 primeiros períodos
evasao_periodos <- dados_filtrados %>%
  filter(!is.na(periodo_evasao))

# Estatísticas globais por período
resumo_global <- evasao_periodos %>%
  group_by(periodo_evasao, `Período de Ingresso`) %>%
  summarise(
    taxa_evasao = mean(Status == "INATIVO"),
    .groups = "drop"
  ) %>%
  group_by(periodo_evasao) %>%
  summarise(
    media = mean(taxa_evasao),
    dp = sd(taxa_evasao),
    .groups = "drop"
  )

print(resumo_global)

# Estratificação por variáveis demográficas
resumo_demografico <- evasao_periodos %>%
  group_by(periodo_evasao, Sexo) %>%
  summarise(
    media = mean(Status == "INATIVO"),
    dp = sd(Status == "INATIVO"),
    .groups = "drop"
  )

print(resumo_demografico)
