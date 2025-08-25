# Carregando pacotes
library(tidyverse)
library(janitor)
library(stringr)
library(scales) # Para formatar porcentagens no gráfico

# --- 1. CARREGAMENTO E PREPARAÇÃO INICIAL DOS DADOS ---
# Caminho do arquivo (ajuste conforme necessário)
# caminho <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas/alunos-final.csv"

caminho_base <- "/home/diego/Documentos/Semestre 2025.2/Tabelas/alunos-final.csv"


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

# Conversões de tipo e preparação inicial
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

# --- 2. FUNÇÃO CORRIGIDA PARA CONVERTER PERÍODOS ---
# Esta função agora lida corretamente com vetores
converter_periodo <- function(periodo) {
  # Garantir que a entrada é um vetor de caracteres
  periodo <- str_trim(as.character(periodo))
  
  # Inicializar vetor de resultados
  result <- rep(NA_real_, length(periodo))
  
  # Identificar posições válidas (não NA e com comprimento adequado)
  validos <- !is.na(periodo) & nchar(periodo) >= 6
  
  # Processar apenas os válidos
  if (any(validos)) {
    ano <- as.numeric(str_sub(periodo[validos], 1, 4))
    semestre <- as.numeric(str_sub(periodo[validos], 6, 6))
    result[validos] <- (ano - 2000) * 2 + semestre
  }
  
  return(result)
}

# Adicionar colunas de períodos convertidos ao dataframe
alunos_com_periodos <- alunos %>%
  mutate(
    per_ing = converter_periodo(periodo_ingresso),
    per_eva = converter_periodo(periodo_evasao)
  )

# --- 3. DEFINIÇÃO DOS PERÍODOS DE ANÁLISE ---
# Definir os períodos de análise para cada currículo (ACUMULADO)
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
  # A mudança está aqui: per_eva <= (per_ing + diff_target) - Evasão ACUMULADA
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

# --- 6. EXECUÇÃO DOS CÁLCULOS ACUMULADOS ---
cat("Calculando estatísticas ACUMULADAS para o currículo 1999...\n")
# CORREÇÃO: Nome da variável deve ser 'resultado_1999_acum'
resultado_1999_acum <- processar_currículo_acumulado(alunos_com_periodos, periodos_1999, "1999")

cat("Calculando estatísticas ACUMULADAS para o currículo 2017...\n")
# CORREÇÃO: Nome da variável deve ser 'resultado_2017_acum'
resultado_2017_acum <- processar_currículo_acumulado(alunos_com_periodos, periodos_2017, "2017")

# Combinar resultados finais ACUMULADOS
# CORREÇÃO: Usar os nomes corretos das variáveis
resultado_final_acumulado <- bind_rows(resultado_1999_acum, resultado_2017_acum)

# --- 7. EXIBIÇÃO E VISUALIZAÇÃO DOS RESULTADOS ACUMULADOS ---
# Exibir resultados
print("Resultados ACUMULADOS por sexo e currículo:")
print(resultado_final_acumulado)

# Tabela em formato largo para melhor visualização
tabela_larga_acum <- resultado_final_acumulado %>%
  select(periodo, curriculo, sexo, taxa_evasao) %>%
  pivot_wider(
    names_from = periodo,
    values_from = taxa_evasao,
    names_prefix = "Periodo_"
  ) %>%
  arrange(curriculo, sexo)

# Exibir tabela formatada
print("Tabela resumo ACUMULADA (taxas de evasão por período):")
print(tabela_larga_acum)

# Gráfico de comparação ACUMULADA
p_acum <- ggplot(resultado_final_acumulado, aes(x = periodo, y = taxa_evasao, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ curriculo, labeller = label_both) +
  labs(
    title = "Taxa de Evasão ACUMULADA por Sexo e Currículo",
    subtitle = "Contando alunos que evadiram até o período especificado",
    x = "Período Letivo",
    y = "Taxa de Evasão (%)",
    fill = "Sexo"
  ) +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c("MASCULINO" = "#1f77b4", "FEMININO" = "#ff7f0e")) +
  theme(
    strip.text = element_text(size = 13),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "right"
  )

print(p_acum)

# --- 8. (OPCIONAL) ANÁLISE POR FAIXA ETÁRIA ACUMULADA ---
# Criar variável faixa etária
alunos_com_faixa_acum <- alunos_com_periodos %>%
  mutate(
    faixa_etaria = case_when(
      idade < 18 ~ "< 18",
      idade >= 18 & idade <= 20 ~ "18-20",
      idade >= 21 & idade <= 23 ~ "21-23",
      idade >= 24 & idade <= 26 ~ "24-26",
      idade >= 27 ~ "27+",
      TRUE ~ NA_character_
    )
  )

# Função para calcular evasão ACUMULADA por faixa etária
calcular_taxa_evasao_faixa_acum <- function(df, diff_target, ingresso_min, ingresso_max, curriculo_nome) {
  per_min <- converter_periodo(ingresso_min)
  per_max <- converter_periodo(ingresso_max)
  
  # Total de ingressantes por faixa etária
  ingressantes <- df %>%
    filter(
      curriculo == curriculo_nome,
      per_ing >= per_min,
      per_ing <= per_max,
      !is.na(faixa_etaria)
    ) %>%
    group_by(faixa_etaria) %>%
    summarise(total_ingressantes = n(), .groups = "drop")
  
  # Total de evadidos ACUMULADOS até o período alvo por faixa etária
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
      per_eva <= (per_ing + diff_target), # <-- Evasão ACUMULADA
      !is.na(faixa_etaria)
    ) %>%
    group_by(faixa_etaria) %>%
    summarise(total_evadidos = n(), .groups = "drop")
  
  # Combinar e calcular taxa
  resultado <- left_join(ingressantes, evadidos, by = "faixa_etaria") %>%
    replace_na(list(total_evadidos = 0)) %>%
    mutate(
      taxa_evasao = round(100 * total_evadidos / total_ingressantes, 2),
      diff = diff_target,
      curriculo = curriculo_nome
    )
  
  return(resultado)
}

# Função para processar todos os períodos por faixa etária (ACUMULADO)
processar_currículo_faixa_acum <- function(df, periodos, curriculo_nome) {
  map_dfr(names(periodos), function(nome) {
    p <- periodos[[nome]]
    calcular_taxa_evasao_faixa_acum(df, p$diff, p$min, p$max, curriculo_nome) %>%
      mutate(periodo = nome)
  }) %>%
    select(periodo, curriculo, faixa_etaria, total_ingressantes, total_evadidos, taxa_evasao)
}

# Calcular resultados por faixa etária acumulada para ambos os currículos
cat("Calculando estatísticas ACUMULADAS por faixa etária...\n")
resultado_faixa_1999_acum <- processar_currículo_faixa_acum(alunos_com_faixa_acum, periodos_1999, "1999")
resultado_faixa_2017_acum <- processar_currículo_faixa_acum(alunos_com_faixa_acum, periodos_2017, "2017")
resultado_faixa_final_acum <- bind_rows(resultado_faixa_1999_acum, resultado_faixa_2017_acum)

# Exibir resultados por faixa etária ACUMULADOS
print("Resultados ACUMULADOS por faixa etária e currículo:")
print(resultado_faixa_final_acum)

# Gráfico por faixa etária acumulada
p_faixa_acum <- ggplot(resultado_faixa_final_acum, aes(x = faixa_etaria, y = taxa_evasao, fill = curriculo)) +
  geom_col(position = "dodge") +
  facet_wrap(~periodo) +
  labs(
    title = "Taxa de Evasão ACUMULADA por Faixa Etária e Currículo",
    subtitle = "Contando alunos que evadiram até o período especificado",
    x = "Faixa Etária",
    y = "Taxa de Evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_faixa_acum)

# --- 9. (OPCIONAL) COMPARAÇÃO COM EVASÃO EXATA ---
# Se você quiser também manter/gerar os resultados da evasão exata para comparação

# Função para calcular taxa de evasão EXATA (mantendo a lógica original)
calcular_taxa_evasao_exata <- function(df, diff_target, ingresso_min, ingresso_max, curriculo_nome) {
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
  
  # Total de evadidos EXATOS no período alvo por sexo para o currículo específico
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
      per_eva == (per_ing + diff_target) # <-- Evasão EXATA no período alvo
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

# Função para processar todos os períodos de um currículo (EXATA)
processar_currículo_exata <- function(df, periodos, curriculo_nome) {
  map_dfr(names(periodos), function(nome) {
    p <- periodos[[nome]]
    calcular_taxa_evasao_exata(df, p$diff, p$min, p$max, curriculo_nome) %>%
      mutate(periodo = nome)
  }) %>%
    select(periodo, curriculo, sexo, total_ingressantes, total_evadidos, taxa_evasao)
}

# Calcular resultados EXATOS para comparação
cat("Calculando estatísticas EXATAS para comparação...\n")
resultado_1999_exata <- processar_currículo_exata(alunos_com_periodos, periodos_1999, "1999")
resultado_2017_exata <- processar_currículo_exata(alunos_com_periodos, periodos_2017, "2017")
resultado_final_exata <- bind_rows(resultado_1999_exata, resultado_2017_exata)

# Exibir resultados EXATOS para comparação
print("Resultados EXATOS por sexo e currículo (para comparação):")
print(resultado_final_exata)
