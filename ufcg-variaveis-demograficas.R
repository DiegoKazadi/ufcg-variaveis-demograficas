# Carregando pacotes
library(tidyverse)
library(janitor)
library(stringr)
library(data.table)
library(ggplot2)
library(plotly)
library(ggthemes)
library(psych)
library(summarytools)
library(gt)
library(kableExtra)
library(rmarkdown)

# Caminho do arquivo
caminho <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas/alunos-final.csv"

# Leitura correta do CSV com delimitador ";"
alunos <- read_delim(
  caminho,
  delim = ";",
  locale = locale(encoding = "ISO-8859-1"),
  quote = "\"",
  escape_double = FALSE,
  trim_ws = TRUE
)

# Verificar se foi carregado corretamente
if (ncol(alunos) == 1) {
  stop("O arquivo foi carregado com apenas uma coluna. Verifique o delimitador ou codificação.")
}

# Limpar e renomear colunas
alunos <- alunos %>%
  janitor::clean_names() %>%
  rename_with(~ c("cpf", "matricula", "periodo_ingresso", "forma_ingresso", 
                  "curriculo", "estado_civil", "sexo", "idade", 
                  "cor", "cota", "status", "tipo_evasao", "periodo_evasao"))

# Conversões de tipo
alunos <- alunos %>%
  mutate(
    idade = as.numeric(idade),
    periodo_ingresso = as.character(periodo_ingresso),
    periodo_evasao = as.character(periodo_evasao)
  )

# Função para converter períodos como "2011.1" para valor contínuo
converter_periodo <- function(periodo) {
  periodo <- str_trim(as.character(periodo))
  ano <- as.numeric(str_sub(periodo, 1, 4))
  semestre <- as.numeric(str_sub(periodo, 6, 6))
  return((ano - 2000) * 2 + semestre)
}

# Gerar colunas de períodos contínuos e diferença
alunos_filtrados <- alunos %>%
  mutate(
    per_ing = converter_periodo(periodo_ingresso),
    per_eva = converter_periodo(periodo_evasao),
    diff_periodos = per_eva - per_ing
  ) %>%
  filter(
    status == "INATIVO",
    !is.na(tipo_evasao),
    tipo_evasao != "GRADUADO"
  )

# Função de estatísticas por período de evasão
estatisticas_evasao <- function(df, diff_target, ingresso_min, ingresso_max) {
  df %>%
    filter(
      !is.na(per_ing),
      !is.na(per_eva),
      per_ing >= converter_periodo(ingresso_min),
      per_ing <= converter_periodo(ingresso_max),
      diff_periodos == diff_target
    ) %>%
    summarise(
      total_evadidos = n(),
      idade_media = round(mean(idade, na.rm = TRUE), 2),
      idade_dp = round(sd(idade, na.rm = TRUE), 2),
      prop_mulheres = round(mean(sexo == "FEMININO", na.rm = TRUE), 4),
      prop_cotas = round(mean(str_to_lower(cota) %in% c("sim", "s"), na.rm = TRUE), 4),
      .groups = 'drop'
    )
}

# Lista de períodos-alvo
periodos <- list(
  `1º Período` = list(diff = 1, min = "2011.1", max = "2017.2"),
  `2º Período` = list(diff = 2, min = "2011.1", max = "2017.1"),
  `3º Período` = list(diff = 3, min = "2011.1", max = "2016.2"),
  `4º Período` = list(diff = 4, min = "2011.1", max = "2016.1")
)

# Aplicar estatísticas para cada período
resultado <- map_dfr(periodos, ~ estatisticas_evasao(
  alunos_filtrados,
  diff_target = .x$diff,
  ingresso_min = .x$min,
  ingresso_max = .x$max
), .id = "periodo")

# Exibir resultados
print(resultado)


###

head(alunos_filtrados)

