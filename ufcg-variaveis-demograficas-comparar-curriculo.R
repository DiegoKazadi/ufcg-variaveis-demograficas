# Carregar apenas pacotes necessários
library(readr)
library(dplyr)
library(janitor)
library(stringr)
library(tidyverse)  # carrega: ggplot2, dplyr, readr, stringr, forcats, etc.
library(janitor)    # limpeza de nomes e tabelas
library(gt)         # para gerar tabelas elegantes
library(viridis)    # paleta de cores acessível
library(ggthemes)   # temas extras para gráficos
library(ggplot2)
mtcars %>% head()

# Definir o caminho base onde estão os arquivos
caminho_base <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/tabelas"

# Nome do arquivo que será carregado
arquivo_alunos <- file.path(caminho_base, "alunos-final.csv")

# Leitura do arquivo CSV
# Utilizamos read_csv do pacote readr por ser mais rápido e robusto com grandes volumes de dados
alunos <- readr::read_delim(arquivo_alunos, delim = ",", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# Visualizar as primeiras linhas para garantir que foi carregado corretamente
head(alunos)

# Verificar estrutura do dataframe: tipos de variáveis, dimensões, etc.
glimpse(alunos)

# Exemplo de verificação rápida: contagem de linhas e colunas
cat("Total de linhas:", nrow(alunos), "\n")
cat("Total de colunas:", ncol(alunos), "\n")

# Verificar se há valores ausentes por coluna
colSums(is.na(alunos))
alunos <- alunos %>% clean_names()

# Mostrar nomes das colunas para referência
names(alunos)

###

# Filtro de estudantes ingressantes entre 2011 e 2023

# Converter o período de ingresso para numérico, se necessário
alunos$`periodo_de_ingresso` <- as.numeric(alunos$`periodo_de_ingresso`)

# Filtrar os dados entre 2011 e 2023
alunos_filtrados <- alunos %>%
  filter(`periodo_de_ingresso` >= 2011 & `periodo_de_ingresso` <= 2023)

nrow(alunos_filtrados)  # Quantidade de registros após o filtro

# Filtrar apenas os evadidos (inativos ≠ graduados)

evadidos <- alunos_filtrados %>%
  filter(
    status == "INATIVO",
    tipo_de_evasao != "GRADUADO"
  )

# Contar e calcular proporção dos tipos de evasão

motivos_evasao <- evadidos %>%
  group_by(tipo_de_evasao) %>%
  summarise(total = n(), .groups = "drop") %>%
  mutate(
    porcentagem = round(total / sum(total) * 100, 2)
  ) %>%
  arrange(desc(total))

print(motivos_evasao)

# Gráfico da distribuição dos motivos

ggplot(motivos_evasao, aes(x = reorder(tipo_de_evasao, -total), y = total)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(porcentagem, "%")), vjust = -0.5, size = 3.5) +
  labs(
    title = "Distribuição dos Motivos de Evasão (Inativos não Graduados)",
    x = "Motivo da Evasão",
    y = "Número de Estudantes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

###

# Distribuição das taxas de evasão por Currículo.

# ------------------------------
# Função para calcular taxas de evasão por currículo e período
# ------------------------------
calcular_taxa_evasao <- function(dados, nome_curriculo, max_periodo = 4) {
  resultados <- list()
  
  # Garantir que periodo_de_ingresso é numérico
  dados <- dados %>%
    mutate(periodo_de_ingresso = as.numeric(periodo_de_ingresso))
  
  for (p in 1:max_periodo) {
    # Para cada período curricular (P1, P2, ...)
    periodo_nome <- paste0("P", p)
    
    df <- dados %>%
      # Seleciona alunos cujo currículo permite análise até o período p
      # (ex: ingresso em 2023 só pode ter P1 até 2023.1, mas não P4)
      filter(
        !is.na(periodo_de_ingresso),
        periodo_de_ingresso <= 2023 - (p - 1)  # ex: P4 precisa ter ingressado até 2020
      ) %>%
      # Calcula o período alvo: ingresso + p - 1
      mutate(
        periodo_alvo = periodo_de_ingresso + p - 1,
        curriculo = nome_curriculo,
        periodo = periodo_nome
      ) %>%
      # Aqui você poderia filtrar por status até o período_alvo, 
      # mas como não tem data de evasão, assumimos que status é atual
      # → então estamos calculando a evasão **até hoje**, não por período
      group_by(curriculo, periodo) %>%
      summarise(
        total = n(),
        evadidos = sum(status == "INATIVO" & tipo_de_evasao != "GRADUADO"),
        taxa_evasao = round(evadidos / total * 100, 2),
        .groups = "drop"
      )
    
    resultados[[periodo_nome]] <- df
  }
  
  bind_rows(resultados)
}

# ------------------------------
# Aplicar a função para cada currículo
# ------------------------------
taxas_1999 <- calcular_taxa_evasao(alunos_filtrados, periodos_1999, "Currículo 1999")
taxas_2017 <- calcular_taxa_evasao(alunos_filtrados, periodos_2017, "Currículo 2017")

# Unir resultados
taxas_evasao <- bind_rows(taxas_1999, taxas_2017)

print(taxas_evasao)

# ------------------------------
# Gráfico comparativo
# ------------------------------
ggplot(taxas_evasao, aes(x = periodo, y = taxa_evasao, fill = curriculo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(taxa_evasao, "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3.5) +
  labs(
    title = "Distribuição das Taxas de Evasão por Currículo e Período",
    x = "Período do Curso",
    y = "Taxa de Evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

###

# ------------------------------
# Boxplot: Distribuição das taxas de evasão por currículo
# ------------------------------
ggplot(taxas_evasao, aes(x = curriculo, y = taxa_evasao, fill = curriculo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) + # evita sobreposição com jitter
  geom_jitter(width = 0.15, size = 2, color = "black", alpha = 0.6) + # pontos individuais
  labs(
    title = "Distribuição das Taxas de Evasão por Currículo",
    subtitle = "Comparação entre os currículos de 1999 e 2017 nos quatro primeiros períodos",
    x = "Currículo",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40")
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10)
  )

ggplot(taxas_evasao, aes(x = curriculo, y = taxa_evasao, color = curriculo)) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    title = "Taxas de Evasão por Currículo",
    subtitle = "Média e dispersão nos quatro primeiros períodos",
    x = "Currículo",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal()

###

ggplot(taxas_evasao, aes(x = curriculo, y = taxa_evasao, color = curriculo)) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 6, color = "black") +
  labs(
    title = "Distribuição das Taxas de Evasão por Currículo",
    subtitle = "Cada ponto representa um período; losango preto indica a média",
    x = "Currículo",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

###

ggplot(taxas_evasao, aes(x = curriculo, y = taxa_evasao, color = curriculo)) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 6, color = "black") +
  labs(
    title = "Distribuição das Taxas de Evasão por Currículo",
    subtitle = "Cada ponto representa um período; losango preto indica a média",
    x = "Currículo",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


###

# violino + pontos individuais

ggplot(taxas_evasao, aes(x = curriculo, y = taxa_evasao, fill = curriculo)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 3, color = "black", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 6, color = "red") +
  labs(
    title = "Distribuição das Taxas de Evasão por Currículo",
    subtitle = "Currículos de 1999 e 2017 nos quatro primeiros períodos",
    x = "Currículo",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


###

# densidade de pontos (stripplot)

ggplot(taxas_evasao, aes(x = curriculo, y = taxa_evasao, color = curriculo)) +
  geom_jitter(width = 0.2, height = 0, size = 4, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 6, color = "black") +
  labs(
    title = "Distribuição das Taxas de Evasão por Currículo",
    subtitle = "Cada ponto representa um período; losango preto indica a média",
    x = "Currículo",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

###

# ggplot(taxas_evasao, aes(x = curriculo, y = taxa_evasao, color = curriculo)) +
geom_jitter(width = 0.2, height = 0, size = 4, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 6, color = "black") +
  labs(
    title = "Distribuição das Taxas de Evasão por Currículo",
    subtitle = "Cada ponto representa um período; losango preto indica a média",
    x = "Currículo",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


###
 ggplot(taxas_evasao, aes(x = curriculo, y = taxa_evasao, color = curriculo)) +
  geom_jitter(width = 0.2, height = 0, size = 4, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 6, color = "black") +
  labs(
    title = "Distribuição das Taxas de Evasão por Currículo",
    subtitle = "Cada ponto representa um período; losango preto indica a média",
    x = "Currículo",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

###

# linha por período (comparação direta)

ggplot(taxas_evasao, aes(x = periodo, y = taxa_evasao, color = curriculo, group = curriculo)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(
      title = "Taxas de Evasão por Período e Currículo",
      subtitle = "Comparação dos currículos de 1999 e 2017",
      x = "Período",
      y = "Taxa de Evasão (%)",
      color = "Currículo"
    ) +
    theme_minimal()
  
### 


















# grafico 

ggplot(motivos_evasao, aes(x = reorder(tipo_de_evasao, -total), y = total, fill = tipo_de_evasao)) +
  geom_col(show.legend = TRUE) +
  geom_text(aes(label = paste0(porcentagem, "%")), 
            vjust = -0.5, size = 3.5, color = "black") +
  labs(
    title = "Distribuição dos Motivos de Evasão (Inativos não Graduados)",
    x = "Motivo da Evasão",
    y = "Número de Estudantes",
    fill = "Tipo de Evasão"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  )
















# Função para converter períodos no formato "2011.1" em valor contínuo
converter_periodo <- function(periodo) {
  periodo <- str_trim(as.character(periodo))
  if (nchar(periodo) < 6) return(NA_real_)
  ano <- as.numeric(str_sub(periodo, 1, 4))
  semestre <- as.numeric(str_sub(periodo, 6, 6))
  (ano - 2000) * 2 + semestre
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
    per_ing = converter_periodo(periodo_de_ingresso),
    per_eva = converter_periodo(tipo_de_evasao)
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



# Criar coluna status_discente padronizada
alunos <- alunos %>%
  mutate(
    status_padrao = toupper(trimws(status)),
    tipo_evasao_padrao = toupper(trimws(tipo_evasao)),
    sexo_padrao = tolower(trimws(sexo)),
    status_discente = case_when(
      status_padrao == "ATIVO" ~ "Ativo",
      status_padrao == "INATIVO" & tipo_evasao_padrao == "GRADUADO" ~ "Egresso",
      status_padrao == "INATIVO" & tipo_evasao_padrao != "GRADUADO" ~ "Evadido",
      TRUE ~ "Outro"
    ),
    sexo_final = case_when(
      sexo_padrao %in% c("m", "masculino") ~ "Masculino",
      sexo_padrao %in% c("f", "feminino") ~ "Feminino",
      TRUE ~ NA_character_
    )
  )


# 

# Verificar se a tabela existe e tem dados
if (!exists("alunos") || nrow(alunos) == 0) {
  stop("A tabela 'alunos' não existe ou está vazia. Verifique o carregamento dos dados.")
}

# Verificar estrutura da tabela
cat("\nEstrutura da tabela 'alunos':\n")
str(alunos)

# Verificar primeiras linhas
cat("\nPrimeiras linhas da tabela:\n")
head(alunos)


# Verificar colunas necessárias
colunas_necessarias <- c("status", "tipo_evasao", "idade", "sexo")
colunas_faltantes <- setdiff(colunas_necessarias, names(alunos))

if (length(colunas_faltantes) > 0) {
  stop(paste("Colunas faltantes na tabela:", paste(colunas_faltantes, collapse = ", ")))
}

# Verificar valores únicos na coluna status
cat("\nValores únicos na coluna 'status':\n")
print(unique(alunos$status))

# Verificar valores únicos na coluna tipo_evasao
cat("\nValores únicos na coluna 'tipo_evasao':\n")
print(unique(alunos$tipo_evasao))

# Verificar valores na coluna idade
cat("\nResumo estatístico da coluna 'idade':\n")
print(summary(alunos$idade))

### Criação dos Gráficos

# Função para padronizar os status
padronizar_status <- function(df) {
  df %>%
    mutate(
      status_padronizado = case_when(
        toupper(trimws(status)) == "ATIVO" ~ "Ativo",
        toupper(trimws(status)) == "INATIVO" & 
          toupper(trimws(tipo_evasao)) == "GRADUADO" ~ "Egresso",
        toupper(trimws(status)) == "INATIVO" & 
          !is.na(tipo_evasao) ~ "Evadido",
        TRUE ~ "Outro/Indefinido"
      )
    ) %>%
    filter(status_padronizado %in% c("Ativo", "Egresso", "Evadido"))
}

# Aplicar padronização
alunos_status <- padronizar_status(alunos)

# Verificar contagem por status
cat("\nContagem de estudantes por status:\n")
print(table(alunos_status$status_padronizado))

# Função para criar gráfico com verificações
criar_grafico_idade <- function(df, status_filtro, cor) {
  dados_filtrados <- df %>% 
    filter(status_padronizado == status_filtro) %>%
    filter(!is.na(idade))
  
  if (nrow(dados_filtrados) == 0) {
    message(paste("Nenhum dado disponível para status:", status_filtro))
    return(NULL)
  }
  
  media_idade <- mean(dados_filtrados$idade, na.rm = TRUE)
  moda_idade <- as.numeric(names(sort(table(dados_filtrados$idade), decreasing = TRUE)[1]))
  
  ggplot(dados_filtrados, aes(x = idade)) +
    geom_bar(fill = cor, color = "black", alpha = 0.8) +
    geom_vline(xintercept = media_idade, linetype = "dashed", color = "black", size = 1) +
    annotate("text", x = media_idade, y = Inf, 
             label = paste("Média:", round(media_idade, 1)), 
             vjust = 1.5, hjust = 1.1) +
    labs(
      title = paste("Distribuição de Idade -", status_filtro),
      x = "Idade",
      y = "Quantidade de Estudantes",
      caption = paste("Total:", nrow(dados_filtrados), "estudantes | Moda:", moda_idade)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.caption = element_text(hjust = 0.5)
    )
}

# Criar gráficos
g1 <- criar_grafico_idade(alunos_status, "Ativo", "#1f77b4")
g2 <- criar_grafico_idade(alunos_status, "Egresso", "#2ca02c")
g3 <- criar_grafico_idade(alunos_status, "Evadido", "#d62728")

# Combinar gráficos (apenas se existirem)
if (!is.null(g1) || !is.null(g2) || !is.null(g3)) {
  # Remover gráficos nulos
  graficos <- list(g1, g2, g3)
  graficos <- graficos[!sapply(graficos, is.null)]
  
  # Combinar os gráficos disponíveis
  if (length(graficos) > 0) {
    combined_plot <- wrap_plots(graficos, ncol = min(3, length(graficos)))
    print(combined_plot)
  }
} else {
  message("Nenhum gráfico pôde ser gerado. Verifique os dados de entrada.")
}

# Gráfico de densidade comparativo (se houver dados)
if (nrow(alunos_status) > 0) {
  ggplot(alunos_status, aes(x = idade, fill = status_padronizado)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Ativo" = "#1f77b4", "Egresso" = "#2ca02c", "Evadido" = "#d62728")) +
    labs(
      title = "Distribuição de Idade por Status",
      x = "Idade",
      y = "Densidade",
      fill = "Status"
    ) +
    theme_minimal(base_size = 14)
} else {
  message("Não há dados suficientes para gerar o gráfico de densidade.")
}



library(ggplot2)

# Versão mínima para teste
teste_plot <- ggplot(alunos, aes(x = idade)) + 
  geom_histogram(bins = 20, fill = "#d62728", color = "black") +
  labs(title = "Distribuição de Idade - Todos os Alunos")

print(teste_plot)



library(dplyr)
library(ggplot2)

# Criar faixas etárias
alunos <- alunos %>%
  mutate(faixa_idade = cut(
    idade,
    breaks = c(15, 18, 21, 24, 27, 30, 35, 40, 50, Inf),
    labels = c("15-18", "19-21", "22-24", "25-27", "28-30", "31-35", "36-40", "41-50", "50+"),
    right = TRUE
  ))

# Gráfico de barras com faixas etárias
ggplot(alunos, aes(x = faixa_idade)) + 
  geom_bar(fill = "#d62728", color = "black", width = 0.7) +
  labs(
    title = "Distribuição de Idade - Todos os Alunos",
    x = "Faixa Etária (anos)",
    y = "Quantidade"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



###

library(ggplot2)
library(dplyr)

# Filtrar apenas alunos com status Ativo e idade não nula
dados_ativos <- alunos %>%
  filter(status_discente == "Ativo", !is.na(idade))

# Criar faixas de idade
dados_ativos <- dados_ativos %>%
  mutate(faixa_idade = cut(
    idade,
    breaks = seq(15, max(idade, na.rm = TRUE) + 1, by = 3),  # intervalos de 3 anos
    right = FALSE,
    labels = paste(seq(15, max(idade, na.rm = TRUE), by = 3),
                   seq(17, max(idade, na.rm = TRUE) + 2, by = 3),
                   sep = "–")
  ))

# Gráfico com barras por faixa etária
ggplot(dados_ativos, aes(x = faixa_idade)) + 
  geom_bar(fill = "#0072B2", color = "black") +
  labs(
    title = "Distribuição de Idade — Estudantes com Status Ativo",
    x = "Faixa Etária (anos)",
    y = "Quantidade"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotaciona os rótulos
  )


###

library(dplyr)
library(ggplot2)

# Filtrando egressos (status = INATIVO e tipo_evasao = GRADUADO)
egressos <- alunos %>%
  filter(status == "INATIVO", tipo_evasao == "GRADUADO", !is.na(idade))

# Faixas etárias
egressos <- egressos %>%
  mutate(faixa_etaria = cut(
    idade,
    breaks = seq(15, max(idade, na.rm = TRUE) + 1, by = 1), # de ano em ano p/ mais precisão
    right = FALSE,
    labels = paste(seq(15, max(idade, na.rm = TRUE), by = 1),
                   seq(15, max(idade, na.rm = TRUE), by = 1), sep = "")
  ))

# Gráfico de barras
ggplot(egressos, aes(x = idade)) +
  geom_histogram(binwidth = 1, fill = "#009E73", color = "black") +
  geom_vline(xintercept = 21, linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = 24, linetype = "dashed", color = "blue", size = 1) +
  annotate("text", x = 22.5, y = max(table(egressos$idade)) * 0.9,
           label = "Faixa ideal de término (21-24 anos)",
           color = "blue", size = 4, angle = 90, vjust = -0.5) +
  labs(
    title = "Distribuição de Idade — Estudantes Egressos",
    x = "Idade na Conclusão (anos)",
    y = "Quantidade"
  ) +
  theme_minimal(base_size = 13)

###

library(dplyr)
library(ggplot2)

# Filtrando egressos (status = INATIVO e tipo_evasao = GRADUADO)
egressos <- alunos %>%
  filter(status == "INATIVO", tipo_evasao == "GRADUADO", !is.na(idade))

# Verificar se há dados suficientes
if(nrow(egressos) == 0) {
  stop("Nenhum egresso encontrado com os critérios especificados")
}

# Gráfico de barras
p <- ggplot(egressos, aes(x = idade)) +
  geom_histogram(binwidth = 1, fill = "#009E73", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 21, linetype = "dashed", color = "blue", size = 1) +
  geom_vline(xintercept = 24, linetype = "dashed", color = "blue", size = 1) +
  annotate("text", x = 22.5, y = max(table(egressos$idade)) * 0.8,
           label = "Faixa ideal de término\n(21-24 anos)",
           color = "blue", size = 3, angle = 90, vjust = -0.5) +
  labs(
    title = "Distribuição de Idade — Estudantes Egressos",
    x = "Idade na Conclusão (anos)",
    y = "Quantidade"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

print(p)

###

# Filtrando egressos
egressos <- alunos %>%
  filter(status == "INATIVO", tipo_evasao == "GRADUADO", !is.na(idade))

# Verificar dados
if(nrow(egressos) == 0) {
  stop("Nenhum egresso encontrado com os critérios especificados")
}

# Criar tabela de distribuição de idade
tabela_idade <- egressos %>%
  count(idade, name = "Quantidade") %>%
  arrange(idade) %>%
  mutate(
    Porcentagem = round(Quantidade / sum(Quantidade) * 100, 1),
    Acumulado = cumsum(Quantidade),
    '% Acumulado' = round(cumsum(Porcentagem), 1)
  )

# Imprimir tabela formatada
cat("\nTABELA 1: Distribuição de Idade dos Egressos na Conclusão\n")
print(kable(tabela_idade, 
            align = 'c',
            col.names = c("Idade", "N", "%", "Acum.N", "% Acum."),
            format = "simple",  # Pode usar "html" ou "latex" para relatórios
            caption = "Fonte: Sistema acadêmico da UFCG"))

# Calcular estatísticas-chave para o gráfico
idade_media <- mean(egressos$idade)
pct_fora_faixa <- mean(egressos$idade < 21 | egressos$idade > 24) * 100

# Gráfico com ajustes
p <- ggplot(egressos, aes(x = idade)) +
  geom_histogram(binwidth = 1, fill = "#009E73", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 21, linetype = "dashed", color = "blue", linewidth = 1) +
  geom_vline(xintercept = 24, linetype = "dashed", color = "blue", linewidth = 1) +
  annotate("text", x = 22.5, y = max(table(egressos$idade)) * 0.8,
           label = "Faixa ideal de término\n(21-24 anos)",
           color = "blue", size = 3.5, angle = 90, vjust = -0.5) +
  labs(
    title = "Distribuição de Idade dos Egressos",
    subtitle = sprintf("Média: %.1f anos | Fora da faixa ideal: %.1f%%", 
                       idade_media, pct_fora_faixa),
    x = "Faixa Etária (anos)",
    y = "Número de Egressos"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    panel.grid.minor = element_blank()
  )

print(p)

###
library(dplyr)
library(ggplot2)
library(knitr)  # Para impressão formatada da tabela

evadidos <- alunos %>%
  filter(status == "INATIVO", tipo_evasao != "GRADUADO", !is.na(idade))

# Verificar dados
if(nrow(evadidos) == 0) {
  stop("Nenhum estudante evadido encontrado com os critérios especificados")
}

# Criar tabela de distribuição de idade
tabela_idade <- evadidos %>%
  count(idade, name = "Quantidade") %>%
  arrange(idade) %>%
  mutate(
    Porcentagem = round(Quantidade / sum(Quantidade) * 100, 1),
    Acumulado = cumsum(Quantidade),
    '% Acumulado' = round(cumsum(Porcentagem), 1)
  )

# Imprimir tabela formatada
cat("\nTABELA 1: Distribuição de Idade dos Estudantes Evadidos\n")
print(kable(tabela_idade, 
            align = 'c',
            col.names = c("Idade", "N", "%", "Acum.N", "% Acum."),
            format = "simple",
            caption = "Fonte: Sistema acadêmico da UFCG"))

# Calcular estatísticas-chave para o gráfico
idade_media <- mean(evadidos$idade)
pct_fora_faixa <- mean(evadidos$idade < 21 | evadidos$idade > 24) * 100

# Gráfico com ajustes solicitados
p <- ggplot(evadidos, aes(x = idade)) +
  geom_histogram(binwidth = 1, fill =  "#009E73", color = "black", alpha = 0.7) +  # cor diferente para evadidos
  labs(
    title = "Distribuição de Idade dos Estudantes Evadidos",
    x = "Faixa Etária (anos)",
    y = "Número de Evadidos"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "plain", hjust = 0.5),  # remove negrito
    panel.grid.minor = element_blank()
  )

print(p)


###
# Pacotes necessários
library(readr)
library(dplyr)
library(ggplot2)
library(scales)  # para formatar percentuais

# 1. Leitura dos dados
alunos <- read_csv("/home/diego/Documentos/Semestre 2025.2/alunos-final.csv")

# 2. Filtrar apenas evadidos (excluindo graduados)
evadidos <- alunos %>%
  filter(status == "INATIVO", tipo_evasao != "GRADUADO")

# 3. Criar faixas etárias
evadidos <- evadidos %>%
  mutate(faixa_idade = cut(
    idade,
    breaks = seq(15, max(idade, na.rm = TRUE) + 1, by = 3),
    right = FALSE,
    labels = paste(seq(15, max(idade, na.rm = TRUE)-2, by = 3),
                   seq(15+2, max(idade, na.rm = TRUE), by = 3),
                   sep = "-")
  ))

# 4. Calcular percentuais por tipo_evasao e faixa
evadidos_pct <- evadidos %>%
  group_by(faixa_idade, tipo_evasao) %>%
  summarise(qtd = n(), .groups = "drop") %>%
  group_by(faixa_idade) %>%
  mutate(percentual = qtd / sum(qtd) * 100)

# 5. Plotar gráfico
ggplot(evadidos_pct, aes(x = faixa_idade, y = percentual, fill = tipo_evasao)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Distribuição de Idade — Estudantes Evadidos",
    x = "Faixa Etária (anos)",
    y = "Proporção (%)",
    fill = "Tipo de Evasão"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###

library(readr)
library(dplyr)
library(janitor)

# Caminho do arquivo CSV
caminho <- "/home/diego/Documentos/Semestre 2025.2/Tabelas/alunos-final.csv"

# Tentativa de leitura automática (detecta delimitador)
alunos <- read_delim(
  caminho,
  delim = NULL,  # auto-detecção do delimitador (, ; ou tab)
  locale = locale(encoding = "ISO-8859-1"),
  quote = "\"",
  escape_double = FALSE,
  trim_ws = TRUE
)

# Caso ainda venha como uma coluna só, forçar leitura com vírgula
if (ncol(alunos) == 1) {
  alunos <- read_delim(
    caminho,
    delim = ",",
    locale = locale(encoding = "ISO-8859-1"),
    quote = "\"",
    escape_double = FALSE,
    trim_ws = TRUE
  )
}

# Padronizar nomes das colunas
alunos <- alunos %>%
  janitor::clean_names()

# Renomear colunas (ajuste conforme necessário, dependendo da ordem real do CSV)
colnames(alunos) <- c(
  "cpf", "matricula", "periodo_ingresso", "forma_ingresso",
  "curriculo", "estado_civil", "sexo", "idade",
  "cor", "cota", "status", "tipo_evasao", "periodo_evasao"
)

# Converter colunas para tipos adequados
alunos <- alunos %>%
  mutate(
    idade = as.numeric(idade),
    periodo_ingresso = as.character(periodo_ingresso),
    periodo_evasao = as.character(periodo_evasao),
    curriculo = as.character(curriculo)
  )

# Filtrar apenas estudantes evadidos com status "INATIVO" e tipo_evasao diferente de "GRADUADO"
evadidos <- alunos %>%
  filter(status == "INATIVO", tipo_evasao != "GRADUADO", !is.na(idade))

# Conferir resultado
glimpse(evadidos)
head(evadidos)

