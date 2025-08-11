# Pacotes
library(tidyverse)
library(janitor)
library(stringr)
library(data.table)
library(ggthemes)
library(psych)
library(summarytools)
library(gt)
library(kableExtra)
library(rmarkdown)
library(scales)    # para formatar porcentagens

# Caminho do arquivo (ajuste para o teu ambiente)
caminho <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas/alunos-final.csv"

# Função para ler o arquivo de forma tolerante
read_file_safe <- function(path) {
  # tentativa 1: delimitador ";", ISO-8859-1
  try1 <- tryCatch(readr::read_delim(path, delim = ";",
                                     locale = locale(encoding = "ISO-8859-1"),
                                     quote = "\"", escape_double = FALSE, trim_ws = TRUE,
                                     guess_max = 10000),
                   error = function(e) e)
  if (!inherits(try1, "error") && ncol(try1) > 1) return(try1)
  
  # tentativa 2: separador ","
  try2 <- tryCatch(readr::read_delim(path, delim = ",",
                                     locale = locale(encoding = "ISO-8859-1"),
                                     guess_max = 10000),
                   error = function(e) e)
  if (!inherits(try2, "error") && ncol(try2) > 1) return(try2)
  
  # tentativa 3: read_csv2 (padrão ; em algumas configurações)
  try3 <- tryCatch(readr::read_csv2(path, locale = locale(encoding = "ISO-8859-1")),
                   error = function(e) e)
  if (!inherits(try3, "error") && ncol(try3) > 1) return(try3)
  
  stop("Não foi possível ler o arquivo com as estratégias tentadas. Verifique delimitador/encoding/arquivo.")
}

# Leitura segura
alunos <- read_file_safe(caminho)

# Limpar nomes com janitor
alunos <- alunos %>% janitor::clean_names()

# Nome esperado das colunas (em ordem)
esperado <- c("cpf", "matricula", "periodo_ingresso", "forma_ingresso",
              "curriculo", "estado_civil", "sexo", "idade",
              "cor", "cota", "status", "tipo_evasao", "periodo_evasao")

# Se o número de colunas bater, renomeia por posição; caso contrário tenta mapear por similaridade
if (length(esperado) == ncol(alunos)) {
  names(alunos) <- esperado
} else {
  # tenta detectar se os nomes já contêm as esperadas
  faltantes <- setdiff(esperado, names(alunos))
  if (length(faltantes) > 0) {
    warning("O número de colunas lidas difere do esperado. Colunas faltantes: ",
            paste(faltantes, collapse = ", "),
            ". Verifique a ordem/nomes das colunas no CSV.")
  }
  # não renomeia por segurança; assume que o usuário verificará
}

# Normalizar e converter tipos
alunos <- alunos %>%
  mutate(across(everything(), ~ ifelse(. == "", NA, .))) %>%
  mutate(
    idade = suppressWarnings(as.numeric(idade)),
    periodo_ingresso = as.character(periodo_ingresso),
    periodo_evasao = as.character(periodo_evasao),
    curriculo = as.character(curriculo),
    status = toupper(str_trim(as.character(status))),
    tipo_evasao = toupper(str_trim(as.character(tipo_evasao))),
    sexo = str_trim(as.character(sexo))
  )

# Normalizar sexo para categorias "Masculino", "Feminino", "Outro"
alunos <- alunos %>%
  mutate(
    sexo = case_when(
      str_to_upper(sexo) %in% c("M", "MASCULINO") ~ "Masculino",
      str_to_upper(sexo) %in% c("F", "FEMININO") ~ "Feminino",
      TRUE ~ "Outro"
    )
  )

# Conversor robusto de período "YYYY.S" -> valor contínuo (número de semestres desde 2000.0)
converter_periodo <- function(periodo) {
  periodo <- as.character(periodo)
  periodo <- str_trim(periodo)
  # Regex captura ano (4 dígitos) e semestre (primeiro dígito após separador . ou -)
  m <- str_match(periodo, "^(\\d{4})[\\.\\-]?(\\d)")
  if (is.na(m[1,1])) return(NA_real_)
  ano <- as.numeric(m[1,2])
  semestre <- as.numeric(m[1,3])
  # Representação contínua: (ano - 2000)*2 + semestre
  return((ano - 2000) * 2 + semestre)
}

# Adicionar colunas numéricas de períodos (per_ing, per_eva)
alunos <- alunos %>%
  mutate(
    per_ing = converter_periodo(periodo_ingresso),
    per_eva = converter_periodo(periodo_evasao)
  )

# Criar coluna status_discente consolidada: Ativo / Egresso / Evadido
alunos <- alunos %>%
  mutate(
    status_discente = case_when(
      status == "ATIVO" ~ "Ativo",
      status == "INATIVO" & tipo_evasao == "GRADUADO" ~ "Egresso",
      status == "INATIVO" & tipo_evasao != "GRADUADO" & !is.na(tipo_evasao) ~ "Evadido",
      TRUE ~ NA_character_
    )
  )

# Criar tabela_final (compatibilidade com scripts anteriores)
tabela_final <- alunos

# --------------------------------------------------------
# Funções de cálculo e análise (mantidas/adaptadas)
# --------------------------------------------------------

# Definição de períodos (exemplo: ajustar se necessário)
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
  
  ingressantes <- df %>%
    filter(curriculo == curriculo_nome, !is.na(per_ing), per_ing >= per_min, per_ing <= per_max, !is.na(sexo)) %>%
    group_by(sexo) %>%
    summarise(total_ingressantes = n(), .groups = "drop")
  
  evadidos <- df %>%
    filter(curriculo == curriculo_nome,
           status_discente == "Evadido",
           !is.na(per_ing), !is.na(per_eva),
           per_ing >= per_min, per_ing <= per_max,
           per_eva == (per_ing + diff_target)) %>%
    group_by(sexo) %>%
    summarise(total_evadidos = n(), .groups = "drop")
  
  resultado <- left_join(ingressantes, evadidos, by = "sexo") %>%
    replace_na(list(total_evadidos = 0)) %>%
    mutate(
      taxa_evasao = ifelse(total_ingressantes > 0, round(100 * total_evadidos / total_ingressantes, 2), NA_real_),
      diff = diff_target,
      curriculo = curriculo_nome
    )
  
  return(resultado)
}

# Processar currículo (utiliza purrr::map_dfr via tidyverse)
processar_curriculo <- function(df, periodos, curriculo_nome) {
  purrr::map_dfr(names(periodos), function(nome) {
    p <- periodos[[nome]]
    calcular_taxa_evasao_exata(df, p$diff, p$min, p$max, curriculo_nome) %>%
      mutate(periodo = nome)
  }) %>%
    select(periodo, curriculo, sexo, total_ingressantes, total_evadidos, taxa_evasao)
}

# Executar cálculos para os dois currículos
resultado_1999 <- processar_curriculo(tabela_final, periodos_1999, "1999")
resultado_2017 <- processar_curriculo(tabela_final, periodos_2017, "2017")
resultado_final <- bind_rows(resultado_1999, resultado_2017)

# Exibir
cat("Resultados por sexo e currículo (primeiros períodos):\n")
print(resultado_final)

# Tabela larga (taxas por período para visualização)
tabela_larga <- resultado_final %>%
  select(periodo, curriculo, sexo, taxa_evasao) %>%
  pivot_wider(names_from = periodo, values_from = taxa_evasao) %>%
  arrange(curriculo, sexo)
print("Tabela resumo (taxas de evasão por período):")
print(tabela_larga)

# Gráfico de comparação por sexo e currículo
ggplot(resultado_final, aes(x = periodo, y = taxa_evasao, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, colour = "black", alpha = 0.9) +
  facet_wrap(~ curriculo, labeller = label_both) +
  labs(title = "Taxa de Evasão por Sexo e Currículo nos 4 Primeiros Períodos",
       x = "Período Letivo", y = "Taxa de Evasão (%)", fill = "Sexo") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --------------------------------------------------------
# Faixas etárias (opcional)
# --------------------------------------------------------
alunos_faixa <- tabela_final %>%
  mutate(
    faixa_etaria = case_when(
      !is.na(idade) & idade < 18 ~ "<18",
      !is.na(idade) & idade >= 18 & idade <= 20 ~ "18-20",
      !is.na(idade) & idade >= 21 & idade <= 23 ~ "21-23",
      !is.na(idade) & idade >= 24 & idade <= 26 ~ "24-26",
      !is.na(idade) & idade >= 27 ~ "27+",
      TRUE ~ NA_character_
    )
  )

calcular_taxa_evasao_faixa <- function(df, diff_target, ingresso_min, ingresso_max, curriculo_nome) {
  per_min <- converter_periodo(ingresso_min)
  per_max <- converter_periodo(ingresso_max)
  
  ingressantes <- df %>%
    filter(curriculo == curriculo_nome, !is.na(per_ing), per_ing >= per_min, per_ing <= per_max, !is.na(faixa_etaria)) %>%
    group_by(faixa_etaria) %>%
    summarise(total_ingressantes = n(), .groups = "drop")
  
  evadidos <- df %>%
    filter(curriculo == curriculo_nome, status_discente == "Evadido", !is.na(per_ing), !is.na(per_eva),
           per_ing >= per_min, per_ing <= per_max, per_eva == (per_ing + diff_target), !is.na(faixa_etaria)) %>%
    group_by(faixa_etaria) %>%
    summarise(total_evadidos = n(), .groups = "drop")
  
  resultado <- left_join(ingressantes, evadidos, by = "faixa_etaria") %>%
    replace_na(list(total_evadidos = 0)) %>%
    mutate(taxa_evasao = ifelse(total_ingressantes > 0, round(100 * total_evadidos / total_ingressantes, 2), NA_real_),
           diff = diff_target, curriculo = curriculo_nome)
  
  return(resultado)
}

processar_curriculo_faixa <- function(df, periodos, curriculo_nome) {
  purrr::map_dfr(names(periodos), function(nome) {
    p <- periodos[[nome]]
    calcular_taxa_evasao_faixa(df, p$diff, p$min, p$max, curriculo_nome) %>%
      mutate(periodo = nome)
  }) %>%
    select(periodo, curriculo, faixa_etaria, total_ingressantes, total_evadidos, taxa_evasao)
}

resultado_faixa_1999 <- processar_curriculo_faixa(alunos_faixa, periodos_1999, "1999")
resultado_faixa_2017 <- processar_curriculo_faixa(alunos_faixa, periodos_2017, "2017")
resultado_faixa_final <- bind_rows(resultado_faixa_1999, resultado_faixa_2017)

print("Resultados por faixa etária e currículo:")
print(resultado_faixa_final)

# Gráfico por faixa etária
ggplot(resultado_faixa_final %>% filter(!is.na(faixa_etaria)),
       aes(x = faixa_etaria, y = taxa_evasao, fill = curriculo)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ periodo) +
  labs(title = "Taxa de Evasão por Faixa Etária e Currículo",
       x = "Faixa Etária", y = "Taxa de Evasão (%)", fill = "Currículo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --------------------------------------------------------
# Distribuições por status (Ativo / Egresso / Evadido)
# --------------------------------------------------------
# Verificar se tabela_final existe e criar objetos Ativos/Egressos/Evadidos
ativos <- tabela_final %>% filter(status_discente == "Ativo")
egressos <- tabela_final %>% filter(status_discente == "Egresso")
evadidos <- tabela_final %>% filter(status_discente == "Evadido")

# Exemplo: distribuição por idade para Ativos
resumo_idade_ativos <- ativos %>%
  filter(!is.na(idade)) %>%
  group_by(idade) %>%
  summarise(qtd = n(), .groups = "drop") %>%
  arrange(idade)

print("Resumo idade - Ativos:")
print(resumo_idade_ativos)

ggplot(ativos %>% filter(!is.na(idade)), aes(x = idade)) +
  geom_bar(fill = "#1f77b4", color = "black") +
  labs(title = "Distribuição por idade — estudantes com status Ativo",
       x = "Idade", y = "Quantidade de estudantes") +
  theme_minimal(base_size = 14)
