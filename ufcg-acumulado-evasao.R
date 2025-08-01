library(tidyverse)
library(janitor)
library(stringr)
library(scales) # Para formatar porcentagens no gráfico

# --- 1. CARREGAMENTO E PREPARAÇÃO INICIAL DOS DADOS ---
# Caminho do arquivo (ajuste conforme necessário)
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

# Conversões de tipo e preparação
alunos <- alunos %>%
  mutate(
    idade = as.numeric(idade),
    periodo_ingresso = as.character(periodo_ingresso),
    periodo_evasao = as.character(periodo_evasao),
    curriculo = as.character(curriculo) # Garantir que currículo é caractere
  ) %>%
  filter(
    !is.na(curriculo),
    !is.na(sexo),
    !is.na(periodo_ingresso),
    curriculo %in% c("1999", "2017")
  )

# --- 2. FUNÇÃO PARA CONVERTER PERÍODOS ---
converter_periodo <- function(periodo) {
  periodo <- str_trim(as.character(periodo))
  if (nchar(periodo) < 6 || is.na(periodo)) return(NA_real_)
  ano <- as.numeric(str_sub(periodo, 1, 4))
  semestre <- as.numeric(str_sub(periodo, 6, 6))
  return((ano - 2000) * 2 + semestre)
}

# Adicionar colunas de períodos convertidos ao dataframe
alunos_com_periodos <- alunos %>%
  mutate(
    per_ing = converter_periodo(periodo_ingresso),
    per_eva = converter_periodo(periodo_evasao)
  )

# --- 3. DEFINIÇÃO DOS PERÍODOS DE ANÁLISE ---
# Definir os períodos de análise para cada currículo (mantendo os corretos para acumulado)
periodos_1999 <- list(
  `1º Período` = list(diff = 1, min = "2011.1", max = "2017.2"),
  `2º Período` = list(diff = 2, min = "2011.1", max = "2017.1"),
  `3º Período` = list(diff = 3, min = "2011.1", max = "2016.2"),
  `4º Período` = list(diff = 4, min = "2011.1", max = "2016.1")
)

periodos_2017 <- list(
  `1º Período` = list(diff = 1, min = "2018.1", max = "2022.2"),
  `2º Período` = list(diff = 2, min = "2018.1", max = "2022.1"),
  `3º Período` = list(diff = 3, min = "2018.1", max = "2021.2"),
  `4º Período` = list(diff = 4, min = "2018.1", max = "2021.1")
)

# --- 4. FUNÇÃO PARA CALCULAR TAXA DE EVASÃO ACUMULADA POR SEXO E CURRÍCULO ---
calcular_taxa_evasao_acumulada <- function(df, diff_target, ingresso_min, ingresso_max, curriculo_nome) {
  per_min <- converter_periodo(ingresso_min)
  per_max <- converter_periodo(ingresso_max)
  
  # Total de ingressantes por sexo para o currículo específico
  ingressantes <- df %>%
    filter(
      curriculo == curriculo_nome,
      per_ing >= per_min,
      per_ing <= per_max,
      !is.na(sexo)
    ) %>%
    group_by(sexo) %>%
    summarise(total_ingressantes = n(), .groups = "drop")
  
  # Total de evadidos ACUMULADOS até o período alvo por sexo para o currículo específico
  # A mudança está aqui: per_eva <= (per_ing + diff_target)
  evadidos <- df %>%
    filter(
      curriculo == curriculo_nome,
      status == "INATIVO",
      tipo_evasao != "GRADUADO",
      !is.na(tipo_evasao),
      !is.na(per_ing),
      !is.na(per_eva),
      per_ing >= per_min,
      per_ing <= per_max,
      per_eva <= (per_ing + diff_target) # <-- Evasão ACUMULADA até o período alvo
    ) %>%
    group_by(sexo) %>%
    summarise(total_evadidos = n(), .groups = "drop")
  
  # Combinar e calcular taxa
  resultado <- left_join(ingressantes, evadidos, by = "sexo") %>%
    replace_na(list(total_evadidos = 0)) %>%
    mutate(
      taxa_evasao = round(100 * total_evadidos / total_ingressantes, 2),
      diff = diff_target,
      curriculo = curriculo_nome
    )
  
  return(resultado)
}

# --- 5. FUNÇÃO PARA PROCESSAR TODOS OS PERÍODOS DE UM CURRÍCULO (ACUMULADO) ---
processar_currículo_acumulado <- function(df, periodos, curriculo_nome) {
  map_dfr(names(periodos), function(nome) {
    p <- periodos[[nome]]
    calcular_taxa_evasao_acumulada(df, p$diff, p$min, p$max, curriculo_nome) %>%
      mutate(periodo = nome)
  }) %>%
    select(periodo, curriculo, sexo, total_ingressantes, total_evadidos, taxa_evasao)
}

# --- 6. EXECUÇÃO DOS CÁLCULOS ---
cat("Calculando estatísticas ACUMULADAS para o currículo 1999...\n")
resultado_1999_acum <- processar_currículo_acumulado(alunos_com_periodos, periodos_1999, "1999")

cat("Calculando estatísticas ACUMULADAS para o currículo 2017...\n")
resultado_2017_acum <- processar_currículo_acumulado(alunos_com_periodos, periodos_2017, "2017")

# Combinar resultados finais
resultado_final_acumulado <- bind_rows(resultado_1999_acum, resultado_2017_acum)

# --- 7. EXIBIÇÃO E VISUALIZAÇÃO DOS RESULTADOS ---
# Exibir resultados