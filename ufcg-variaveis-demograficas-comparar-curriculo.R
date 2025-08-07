library(tidyverse)
library(janitor)
library(scales) # Para formatar porcentagens no gráfico

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

# Conversões de tipo
alunos <- alunos %>%
  mutate(
    idade = as.numeric(idade),
    periodo_ingresso = as.character(periodo_ingresso),
    periodo_evasao = as.character(periodo_evasao),
    curriculo = as.character(curriculo) # Garantir que currículo é caractere
  )

# Função para converter períodos como "2011.1" para valor contínuo
converter_periodo <- function(periodo) {
  periodo <- str_trim(as.character(periodo))
  if (nchar(periodo) < 6) return(NA_real_) # Proteção contra períodos mal formatados
  ano <- as.numeric(str_sub(periodo, 1, 4))
  semestre <- as.numeric(str_sub(periodo, 6, 6))
  return((ano - 2000) * 2 + semestre)
}

# Definir os períodos de análise para cada currículo
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

# Função para calcular taxa de evasão exata por sexo e currículo
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
  
  # Total de evadidos no período alvo por sexo para o currículo específico
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
      per_eva == (per_ing + diff_target) # Evasão exatamente no período alvo
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

# Adicionar colunas de períodos convertidos ao dataframe
alunos_com_periodos <- alunos %>%
  mutate(
    per_ing = converter_periodo(periodo_ingresso),
    per_eva = converter_periodo(periodo_evasao)
  )

# Função para processar todos os períodos de um currículo
processar_currículo <- function(df, periodos, curriculo_nome) {
  map_dfr(names(periodos), function(nome) {
    p <- periodos[[nome]]
    calcular_taxa_evasao_exata(df, p$diff, p$min, p$max, curriculo_nome) %>%
      mutate(periodo = nome)
  }) %>%
    select(periodo, curriculo, sexo, total_ingressantes, total_evadidos, taxa_evasao)
}

# Calcular resultados para o currículo 1999
cat("Calculando estatísticas para o currículo 1999...\n")
resultado_1999 <- processar_currículo(alunos_com_periodos, periodos_1999, "1999")

# Calcular resultados para o currículo 2017
cat("Calculando estatísticas para o currículo 2017...\n")
resultado_2017 <- processar_currículo(alunos_com_periodos, periodos_2017, "2017")

# Combinar resultados
resultado_final <- bind_rows(resultado_1999, resultado_2017)

# Exibir resultados
print("Resultados por sexo e currículo:")
print(resultado_final)

# Tabela em formato largo para melhor visualização
tabela_larga <- resultado_final %>%
  select(periodo, curriculo, sexo, taxa_evasao) %>%
  pivot_wider(
    names_from = periodo,
    values_from = taxa_evasao,
    names_prefix = "Periodo_"
  ) %>%
  arrange(curriculo, sexo)

# Exibir tabela formatada
print("Tabela resumo (taxas de evasão por período):")
print(tabela_larga)

# Gráfico de comparação
ggplot(resultado_final, aes(x = periodo, y = taxa_evasao, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ curriculo, labeller = label_both) +
  labs(
    title = "Taxa de Evasão por Sexo e Currículo nos 4 Primeiros Períodos",
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

# Análise por faixa etária (opcional)
# Criar variável faixa etária
alunos_com_faixa <- alunos_com_periodos %>%
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

# Função para calcular evasão por faixa etária
calcular_taxa_evasao_faixa <- function(df, diff_target, ingresso_min, ingresso_max, curriculo_nome) {
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
  
  # Total de evadidos no período alvo por faixa etária
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
      per_eva == (per_ing + diff_target),
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

# Função para processar todos os períodos por faixa etária
processar_currículo_faixa <- function(df, periodos, curriculo_nome) {
  map_dfr(names(periodos), function(nome) {
    p <- periodos[[nome]]
    calcular_taxa_evasao_faixa(df, p$diff, p$min, p$max, curriculo_nome) %>%
      mutate(periodo = nome)
  }) %>%
    select(periodo, curriculo, faixa_etaria, total_ingressantes, total_evadidos, taxa_evasao)
}

# Calcular resultados por faixa etária para ambos os currículos
cat("Calculando estatísticas por faixa etária...\n")
resultado_faixa_1999 <- processar_currículo_faixa(alunos_com_faixa, periodos_1999, "1999")
resultado_faixa_2017 <- processar_currículo_faixa(alunos_com_faixa, periodos_2017, "2017")
resultado_faixa_final <- bind_rows(resultado_faixa_1999, resultado_faixa_2017)

# Exibir resultados por faixa etária
print("Resultados por faixa etária e currículo:")
print(resultado_faixa_final)

# Gráfico por faixa etária
ggplot(resultado_faixa_final, aes(x = faixa_etaria, y = taxa_evasao, fill = curriculo)) +
  geom_col(position = "dodge") +
  facet_wrap(~periodo) +
  labs(
    title = "Taxa de Evasão por Faixa Etária e Currículo",
    x = "Faixa Etária",
    y = "Taxa de Evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###

# Recodificar status com base no tipo de evasão
distribuicao_idade_status <- alunos %>%
  filter(!is.na(idade), !is.na(status)) %>%
  mutate(
    status_discente = case_when(
      status == "ATIVO" ~ "Ativo",
      status == "INATIVO" & tipo_evasao == "GRADUADO" ~ "Egresso",
      status == "INATIVO" & tipo_evasao != "GRADUADO" ~ "Evadido",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(status_discente))

# Paleta com azul, laranja e verde
cores_personalizadas <- c(
  "Ativo" = "#0072B2",   # azul
  "Evadido" = "#E69F00", # laranja
  "Egresso" = "#009E73"  # verde
)

# Gráfico de densidade
ggplot(distribuicao_idade_status, aes(x = idade, fill = status_discente)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = cores_personalizadas) +
  labs(
    title = "Distribuição da Idade segundo o Status do Discente",
    x = "Idade",
    y = "Densidade",
    fill = "Status do Discente"
  ) +
  theme_minimal(base_size = 13)