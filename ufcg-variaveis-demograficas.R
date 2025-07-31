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
names(alunos_filtrados)

# Converter períodos contínuos
alunos <- alunos %>%
  mutate(
    per_ing = converter_periodo(periodo_ingresso),
    per_eva = converter_periodo(periodo_evasao),
    diff_periodos = per_eva - per_ing
  )

# Lista de períodos de interesse
periodos <- list(
  `1º Período` = list(diff = 1, min = "2011.1", max = "2017.2"),
  `2º Período` = list(diff = 2, min = "2011.1", max = "2017.1"),
  `3º Período` = list(diff = 3, min = "2011.1", max = "2016.2"),
  `4º Período` = list(diff = 4, min = "2011.1", max = "2016.1")
)

# Função para calcular taxa de evasão por sexo e currículo
calcular_taxa_evasao <- function(df, diff_target, ingresso_min, ingresso_max) {
  per_min <- converter_periodo(ingresso_min)
  per_max <- converter_periodo(ingresso_max)
  
  # Total de ingressantes
  ingressantes <- df %>%
    filter(
      per_ing >= per_min,
      per_ing <= per_max
    ) %>%
    group_by(curriculo, sexo) %>%
    summarise(total_ingressantes = n(), .groups = "drop")
  
  # Total de evadidos no período alvo
  evadidos <- df %>%
    filter(
      status == "INATIVO",
      tipo_evasao != "GRADUADO",
      !is.na(tipo_evasao),
      diff_periodos == diff_target,
      per_ing >= per_min,
      per_ing <= per_max
    ) %>%
    group_by(curriculo, sexo) %>%
    summarise(total_evadidos = n(), .groups = "drop")
  
  # Combinar e calcular taxa
  resultado <- left_join(ingressantes, evadidos, by = c("curriculo", "sexo")) %>%
    replace_na(list(total_evadidos = 0)) %>%
    mutate(
      taxa_evasao = round(100 * total_evadidos / total_ingressantes, 2),
      diff = diff_target
    )
  
  return(resultado)
}

# Aplicar para todos os períodos
resultado_final <- map_dfr(names(periodos), function(nome) {
  p <- periodos[[nome]]
  calcular_taxa_evasao(alunos, p$diff, p$min, p$max) %>%
    mutate(periodo = nome)
})

# Reorganizar colunas
resultado_final <- resultado_final %>%
  select(periodo, curriculo, sexo, total_ingressantes, total_evadidos, taxa_evasao)

# Visualizar tabela
print(resultado_final)

# Tabela em formato largo para exportação ou visualização
tabela_larga <- resultado_final %>%
  pivot_wider(
    names_from = periodo,
    values_from = taxa_evasao
  )

# Exibir tabela formatada
print(tabela_larga)


###

ggplot(resultado_final, aes(x = periodo, y = taxa_evasao, fill = sexo)) +
  geom_col(position = "dodge") +
  facet_wrap(~ curriculo) +
  labs(
    title = "Taxa de Evasão por Sexo e Currículo nos 4 Primeiros Períodos",
    x = "Período Letivo",
    y = "Taxa de Evasão (%)",
    fill = "Sexo"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("MASCULINO" = "#1f77b4", "FEMININO" = "#ff7f0e"))



































