# ============================
# Instalação e carregamento de pacotes
# ============================
pacotes <- c("janitor", "gt", "forcats", "dplyr", "ggthemes", 
             "viridis", "tidyverse", "scales", "stringr", "readr", "ggplot2")

pacotes_instalados <- pacotes %in% rownames(installed.packages())
if (any(!pacotes_instalados)) {
  install.packages(pacotes[!pacotes_instalados])
}

invisible(lapply(pacotes, library, character.only = TRUE))

# ============================
# Carregamento de dados
# ============================
# caminho_base <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/tabelas"
caminho_base <- "/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"
arquivo_alunos <- file.path(caminho_base, "alunos-final.csv")

dados <- read_delim(
  arquivo_alunos, 
  delim = ";", 
  locale = locale(encoding = "UTF-8"), 
  show_col_types = FALSE
)

# Padronizar nomes das colunas
dados <- janitor::clean_names(dados)

# Conferir estrutura
glimpse(dados)
head(dados, 10)

# ============================
# Filtrar estudantes 2011-2023
# ============================
# Verificar nome exato da coluna
colnames(dados)

# Supondo que "Período de Ingresso" virou "periodo_de_ingresso"
unique(dados$periodo_de_ingresso)

dados$periodo_de_ingresso <- as.numeric(dados$periodo_de_ingresso)

dados_filtrados <- dados %>%
  filter(periodo_de_ingresso >= 2011 & periodo_de_ingresso <= 2023)

cat("Total antes do filtro:", nrow(dados), "\n")
cat("Total após o filtro (2011 a 2023):", nrow(dados_filtrados), "\n")


# ============================
# Variável demográfica: Idade
# ============================

# Supondo que existam colunas "ano_nascimento" e "periodo_de_ingresso"
# Caso o CSV use outros nomes, ajuste aqui após verificar com colnames(dados)
dados_filtrados <- dados_filtrados %>%
  mutate(
    idade_ingresso = as.numeric(periodo_de_ingresso) - as.numeric(ano_nascimento),
    faixa_etaria = case_when(
      idade_ingresso < 18 ~ "<18",
      idade_ingresso >= 18 & idade_ingresso <= 20 ~ "18-20",
      idade_ingresso >= 21 & idade_ingresso <= 25 ~ "21-25",
      idade_ingresso >= 26 & idade_ingresso <= 30 ~ "26-30",
      idade_ingresso > 30 ~ "31+",
      TRUE ~ NA_character_
    )
  )

# ============================
# Tabelas comparativas evasão x idade
# ============================
# Supondo que exista uma variável "evasao" (0 = não evadido, 1 = evadido)
# e uma variável "curriculo" (1999 ou 2017)

tabela_idade <- dados_filtrados %>%
  group_by(curriculo, faixa_etaria) %>%
  summarise(
    total = n(),
    evadidos = sum(evasao == 1, na.rm = TRUE),
    taxa_evasao = evadidos / total
  ) %>%
  ungroup()

# Visualização em tabela formatada
tabela_idade %>%
  gt() %>%
  fmt_percent(columns = vars(taxa_evasao), decimals = 1) %>%
  tab_header(
    title = "Taxa de Evasão por Faixa Etária e Currículo",
    subtitle = "Ingressantes 2011-2023"
  )

# ============================
# Gráficos descritivos
# ============================
ggplot(tabela_idade, aes(x = faixa_etaria, y = taxa_evasao, fill = as.factor(curriculo))) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(name = "Currículo") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Taxa de Evasão por Faixa Etária e Currículo",
    subtitle = "Ingressantes 2011-2023",
    x = "Faixa Etária no Ingresso",
    y = "Taxa de Evasão"
  )
