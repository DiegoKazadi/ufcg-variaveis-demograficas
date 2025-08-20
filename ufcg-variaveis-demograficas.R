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
# caminho <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas/alunos-final.csv"

caminho <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/tabelas"
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



library(tidyverse)

#------------------#
# FUNÇÃO: Converter período "2011.1" → contínuo
#------------------#
converter_periodo <- function(periodo) {
  ano <- as.numeric(substr(periodo, 1, 4))
  semestre <- as.numeric(substr(periodo, 6, 6))
  return((ano - 2000) * 2 + semestre)
}

#------------------#
# FILTRAGEM E TRANSFORMAÇÃO INICIAL
#------------------#
alunos <- alunos %>%
  filter(!is.na(curriculo), !is.na(sexo), !is.na(periodo_ingresso), curriculo %in% c("1999", "2017")) %>%
  mutate(
    curriculo = as.character(curriculo),
    per_ing = converter_periodo(periodo_ingresso),
    per_eva = converter_periodo(periodo_evasao)
  )

#------------------#
# FUNÇÃO DE CÁLCULO DE TAXA DE EVASÃO
#------------------#
calcular_taxa_evasao_exata <- function(df, diff_target, ingresso_min, ingresso_max) {
  per_min <- converter_periodo(ingresso_min)
  per_max <- converter_periodo(ingresso_max)
  
  ingressantes <- df %>%
    filter(per_ing >= per_min, per_ing <= per_max) %>%
    group_by(curriculo, sexo) %>%
    summarise(total_ingressantes = n(), .groups = "drop")
  
  evadidos <- df %>%
    filter(
      status == "INATIVO",
      tipo_evasao != "GRADUADO",
      !is.na(tipo_evasao),
      per_ing >= per_min,
      per_ing <= per_max,
      per_eva == per_ing + diff_target
    ) %>%
    group_by(curriculo, sexo) %>%
    summarise(total_evadidos = n(), .groups = "drop")
  
  left_join(ingressantes, evadidos, by = c("curriculo", "sexo")) %>%
    replace_na(list(total_evadidos = 0)) %>%
    mutate(
      taxa_evasao = round(100 * total_evadidos / total_ingressantes, 2),
      diff = diff_target
    )
}

#------------------#
# DEFINIÇÃO DOS PERÍODOS (1999 e 2017)
#------------------#
periodos_definidos <- list(
  `1º Período` = list(diff = 1, min = "2011.1", max = "2022.2"),
  `2º Período` = list(diff = 2, min = "2011.1", max = "2022.1"),
  `3º Período` = list(diff = 3, min = "2011.1", max = "2021.2"),
  `4º Período` = list(diff = 4, min = "2011.1", max = "2021.1")
)

#------------------#
# APLICAÇÃO DA FUNÇÃO PARA CADA PERÍODO
#------------------#
calcular_periodos <- function(df, periodos) {
  map_dfr(names(periodos), function(nome) {
    p <- periodos[[nome]]
    calcular_taxa_evasao_exata(df, p$diff, p$min, p$max) %>%
      mutate(periodo = nome)
  })
}

resultado_final <- calcular_periodos(alunos, periodos_definidos) %>%
  select(periodo, curriculo, sexo, total_ingressantes, total_evadidos, taxa_evasao)

#------------------#
# TABELA EM FORMATO LARGO
#------------------#
tabela_larga <- resultado_final %>%
  pivot_wider(
    names_from = periodo,
    values_from = taxa_evasao
  )

#------------------#
# VISUALIZAÇÃO DO RESULTADO
#------------------#
print(resultado_final)
print(tabela_larga)

#------------------#
# GRÁFICO ATUALIZADO
#------------------#
ggplot(resultado_final, aes(x = periodo, y = taxa_evasao, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ curriculo, labeller = label_both) +
  labs(
    title = "Taxa de Evasão por Sexo e Currículo",
    x = "Período Letivo",
    y = "Taxa de Evasão (%)",
    fill = "Sexo"
  ) +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c("MASCULINO" = "#1f77b4", "FEMININO" = "#ff7f0e")) +
  theme(
    strip.text = element_text(size = 13),      # <-- sem negrito
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # <-- rotação 90 graus
    legend.position = "right"                  # <-- legenda à direita
  )


###

# 1. Criar variável faixa etária
alunos <- alunos %>%
  mutate(
    idade = as.numeric(`Idade Aproximada no Ingresso`),
    faixa_etaria = case_when(
      idade < 18 ~ "< 18",
      idade >= 18 & idade <= 20 ~ "18-20",
      idade >= 21 & idade <= 23 ~ "21-23",
      idade >= 24 & idade <= 26 ~ "24-26",
      idade >= 27 ~ "27+",
      TRUE ~ NA_character_
    ),
    Periodo_Ingresso = `Período de Ingresso`,
    Periodo_Evasao = `Período de Evasão`,
    curriculo = as.character(Currículo),
    evadiu = !is.na(`Tipo de Evasão`) & Status == "INATIVO"
  )

# 2. Definir períodos alvo
periodos <- list(
  `1º Período` = list(diff = 1, min = "2011.1", max = "2017.2"),
  `2º Período` = list(diff = 2, min = "2011.1", max = "2017.1"),
  `3º Período` = list(diff = 3, min = "2011.1", max = "2016.2"),
  `4º Período` = list(diff = 4, min = "2011.1", max = "2016.1")
)

# 3. Função para calcular evasão
calcular_taxa_evasao <- function(data, curriculo_alvo, periodo_nome, diff, min_periodo, max_periodo) {
  data %>%
    filter(curriculo == curriculo_alvo,
           Periodo_Ingresso >= min_periodo,
           Periodo_Ingresso <= max_periodo,
           !is.na(faixa_etaria)) %>%
    mutate(
      evasao_periodo = as.numeric(substr(Periodo_Evasao, 1, 4)) * 2 + as.numeric(substr(Periodo_Evasao, 6, 6)),
      ingresso_periodo = as.numeric(substr(Periodo_Ingresso, 1, 4)) * 2 + as.numeric(substr(Periodo_Ingresso, 6, 6)),
      periodo_diff = evasao_periodo - ingresso_periodo
    ) %>%
    group_by(faixa_etaria) %>%
    summarise(
      total = n(),
      evadiram = sum(evadiu & periodo_diff == diff, na.rm = TRUE),
      taxa = round(100 * evadiram / total, 1),
      .groups = "drop"
    ) %>%
    mutate(curriculo = curriculo_alvo, Periodo = periodo_nome)
}

# 4. Gerar tabela final
tabela_final <- bind_rows(
  lapply(names(periodos), function(p) {
    bind_rows(
      calcular_taxa_evasao(alunos, "1999", p, periodos[[p]]$diff, periodos[[p]]$min, periodos[[p]]$max),
      calcular_taxa_evasao(alunos, "2017", p, periodos[[p]]$diff, periodos[[p]]$min, periodos[[p]]$max)
    )
  })
)

# 5. Visualizar tabela final
print(tabela_final)

# 6. Gerar gráfico
ggplot(tabela_final, aes(x = faixa_etaria, y = taxa, fill = curriculo)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Periodo) +
  labs(title = "Taxa de Evasão por Faixa Etária e Currículo (1º ao 4º Período)",
       x = "Faixa Etária",
       y = "Taxa de Evasão (%)",
       fill = "Currículo") +
  theme_minimal() +
  scale_y_continuous(labels = percent_format(scale = 1))

###





















