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
# Análise Demográfica: Idade
# ============================

# Criar faixas etárias
dados_filtrados <- dados_filtrados %>%
  mutate(faixa_idade = case_when(
    idade_aproximada_no_ingresso < 20 ~ "<20",
    idade_aproximada_no_ingresso >= 20 & idade_aproximada_no_ingresso <= 24 ~ "20-24",
    idade_aproximada_no_ingresso >= 25 & idade_aproximada_no_ingresso <= 29 ~ "25-29",
    idade_aproximada_no_ingresso >= 30 & idade_aproximada_no_ingresso <= 34 ~ "30-34",
    idade_aproximada_no_ingresso >= 35 & idade_aproximada_no_ingresso <= 39 ~ "35-39",
    idade_aproximada_no_ingresso >= 40 ~ "40+",
    TRUE ~ NA_character_
  ))

# Contar total e evasão
tabela_idade <- dados_filtrados %>%
  mutate(evadido = ifelse(status == "INATIVO" & tipo_de_evasao != "GRADUADO", 1, 0)) %>%
  group_by(curriculo, faixa_idade) %>%
  summarise(
    total = n(),
    evadidos = sum(evadido, na.rm = TRUE),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  )

# ============================
# Tabela comparativa
# ============================
tabela_gt <- tabela_idade %>%
  gt(rowname_col = "faixa_idade") %>%
  tab_header(
    title = "Taxa de Evasão por Faixa Etária",
    subtitle = "Comparação entre Currículos 1999 e 2017 (Ingressantes 2011-2023)"
  ) %>%
  fmt_percent(columns = taxa_evasao, scale_values = FALSE) %>%
  cols_label(
    curriculo = "Currículo",
    total = "Total",
    evadidos = "Evadidos",
    taxa_evasao = "Taxa de Evasão"
  )

print(tabela_gt)

# ============================
# Gráfico descritivo
# ============================
ggplot(tabela_idade, aes(x = faixa_idade, y = taxa_evasao, fill = as.factor(curriculo))) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(taxa_evasao, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  scale_fill_viridis_d(name = "Currículo") +
  labs(
    title = "Taxa de Evasão por Faixa Etária",
    subtitle = "Comparação entre Currículos 1999 e 2017",
    x = "Faixa Etária (anos)",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )






















# ============================
# Filtrar estudantes 2011-2023 e currículos válidos
# ============================
dados_filtrados <- dados %>%
  filter(
    periodo_de_ingresso >= 2011 & periodo_de_ingresso <= 2023,
    curriculo %in% c(1999, 2017)  # Mantém apenas os currículos 1999 e 2017
  )

cat("Total após filtro de ingresso e currículos:", nrow(dados_filtrados), "\n")

# ============================
# Análise Demográfica: Idade
# ============================
# Criar faixas etárias
dados_filtrados <- dados_filtrados %>%
  mutate(faixa_idade = case_when(
    idade_aproximada_no_ingresso < 20 ~ "<20",
    idade_aproximada_no_ingresso >= 20 & idade_aproximada_no_ingresso <= 24 ~ "20-24",
    idade_aproximada_no_ingresso >= 25 & idade_aproximada_no_ingresso <= 29 ~ "25-29",
    idade_aproximada_no_ingresso >= 30 & idade_aproximada_no_ingresso <= 34 ~ "30-34",
    idade_aproximada_no_ingresso >= 35 & idade_aproximada_no_ingresso <= 39 ~ "35-39",
    idade_aproximada_no_ingresso >= 40 ~ "40+",
    TRUE ~ NA_character_
  ))

# Contar total e evasão
tabela_idade <- dados_filtrados %>%
  mutate(evadido = ifelse(status == "INATIVO" & tipo_de_evasao != "GRADUADO", 1, 0)) %>%
  group_by(curriculo, faixa_idade) %>%
  summarise(
    total = n(),
    evadidos = sum(evadido, na.rm = TRUE),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  )

# ============================
# Mostrar tabela no console
# ============================
print(tabela_idade)

# ============================
# Gráfico descritivo
# ============================
ggplot(tabela_idade, aes(x = faixa_idade, y = taxa_evasao, fill = as.factor(curriculo))) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(taxa_evasao, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  scale_fill_manual(
    name = "Currículo", 
    values = c("1999" = "#1f468b", "2017" = "#f29c11") # azul escuro e laranja
  ) +
  labs(
    title = "Taxa de Evasão por Faixa Etária",
    subtitle = "Comparação entre Currículos 1999 e 2017 (Ingressantes 2011-2023)",
    x = "Faixa Etária (anos)",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",  # legenda lateral
    plot.title = element_text(face = "bold")
  )

