# Carrega as bibliotecas necessárias para manipulação e análise de dados
library(janitor)      # Para padronizar nomes de colunas
library(readr)        # Para leitura eficiente de dados tabulares
library(dplyr)        # Para manipulação de dados (verbos como filter, mutate, select)
library(tidyverse)    # Coleção de pacotes para ciência de dados (inclui dplyr, ggplot2, readr, etc.)
library(stringr)      # Para manipulação de strings
library(data.table)   # Alternativa para manipulação de grandes volumes de dados (opcional, pode ser removido se não for usado)
library(ggplot2)      # Para criação de gráficos estáticos
library(plotly)       # Para criação de gráficos interativos
library(ggthemes)     # Temas adicionais para ggplot2
library(psych)        # Para análise psicométrica e estatísticas descritivas (opcional, pode ser removido se não for usado)
library(summarytools) # Para tabelas de frequência e estatísticas descritivas (opcional, pode ser removido se não for usado)
library(gt)           # Para criação de tabelas bonitas e formatadas
library(kableExtra)   # Para estilizar tabelas criadas com knitr::kable
library(rmarkdown)    # Para criação de relatórios dinâmicos (se o objetivo for gerar relatórios)

# Define o caminho para o arquivo CSV de entrada
caminho_arquivo <- "/home/diego/Documentos/Semestre 2025.2/Tabelas/alunos-final.csv"

# Lê o arquivo CSV com as configurações corretas

alunos <- read_delim(
  caminho_arquivo,
  delim = ";",
  locale = locale(encoding = "ISO-8859-1"),
  trim_ws = TRUE,
  show_col_types = FALSE
)

# Padroniza os nomes das colunas para um formato limpo e consistente (snake_case).
# Isso facilita a manipulação e evita problemas com caracteres especiais ou espaços.
alunos <- clean_names(alunos)

# Verifica a estrutura e as primeiras linhas do dataframe após a leitura e padronização.

glimpse(alunos)  # Verifica a estrutura do dataframe (deve mostrar 13 colunas)
head(alunos)     # Exibe as primeiras linhas do dataframe

# Se a coluna 'cpf' (ou a primeira coluna lida) contiver todos os dados delimitados por ';',

alunos_corrigido <- alunos %>%
  separate_wider_delim(
    col = cpf, # Ajuste 'cpf' para o nome da coluna que contém os dados concatenados, se for diferente.
    delim = ";",  # Delimitador usado dentro da string da coluna 'cpf' (ou a coluna que foi lida incorretamente).
    names = c("cpf", "matricula", "periodo_ingresso", "forma_ingresso",
              "curriculo", "estado_civil", "sexo", "idade",
              "cor", "cota", "status", "tipo_evasao", "periodo_evasao"),
    too_few = "warn"
  )

# Verifica a estrutura do dataframe corrigido após a separação das colunas.
# 'glimpse()' deve agora mostrar as 13 colunas separadas corretamente.
glimpse(alunos_corrigido)


