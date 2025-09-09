# Instalar pacotes necessários (se ainda não estiverem instalados)
install.packages("janitor")
install.packages("gt")
install.packages("forcats")
install.packages("dplyr")
install.packages("ggthemes")
install.packages("viridis")
install.packages("tidyverse")

# Carregar bibliotecas
library(readr)       # Leitura de arquivos CSV
library(dplyr)       # Manipulação de dados
library(stringr)     # Operações com strings
library(ggplot2)     # Visualizações
library(janitor)     # Limpeza de dados
library(scales)      # Formatação de escalas
library(viridis)     # Paleta de cores acessível
library(gt)          # Tabelas estilizadas
library(ggthemes)    # Temas visuais aprimorados
library(tidyverse)   # Conjunto completo de pacotes

# Caminho correto para a pasta
caminho_base <- "/home/diego/Documentos/Semestre 2024.2/Nova_Analise/tabelas"

# Agora sim, concatenando com o nome do arquivo
arquivo_alunos <- file.path(caminho_base, "alunos-final.csv")

# Carregamento
dados <- read_delim(arquivo_alunos, delim = ",", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# Visualizar a estrutura geral do dataframe

head(dados, 10)  # Mostra as 10 primeiras linhas da tabela
str(dados)  # Mostra os nomes das variáveis e seus tipos
summary(dados)  # Fornece estatísticas básicas para cada coluna
colnames(dados)
View(dados)  # Abre uma aba de visualização em estilo de planilha

###

# Filtro de estudantes ingressantes entre 2011 e 2023

# Converter o período de ingresso para numérico, se necessário
dados$`Período de Ingresso` <- as.numeric(dados$`Período de Ingresso`)

# Filtrar os dados entre 2011 e 2023
dados_filtrados <- dados %>%
  filter(`Período de Ingresso` >= 2011 & `Período de Ingresso` <= 2023)

nrow(dados_filtrados)  # Quantidade de registros após o filtro

### 

# Comparação de Tamanho do Conjunto de Dados
# Total de registros antes do filtro
n_total <- nrow(dados)


# Total de registros após o filtro (2011–2023)
n_filtrado <- nrow(dados_filtrados)

cat("Total antes do filtro:", n_total, "\n")
cat("Total após o filtro (2011 a 2023):", n_filtrado, "\n")

###

# Distribuição por Período de Ingresso (Antes e Depois)
# Antes
table(dados$`Período de Ingresso`)

# Depois
table(dados_filtrados$`Período de Ingresso`)

### Distribuição por Currículo (Antes e Depois)

table(dados$Currículo)
table(dados_filtrados$Currículo)

### Distribuição por Status (Antes e Depois)

table(dados$Status)
table(dados_filtrados$Status)

###

# Tratamento de Valores Ausentes

# 1. Verificar a presença de valores ausentes em cada coluna
colSums(is.na(dados_filtrados))

# Visualizar colunas com maior incidência de NA

# Visualizar apenas as colunas que possuem NAs
na_por_coluna <- colSums(is.na(dados_filtrados))
na_por_coluna[na_por_coluna > 0]

# Tratar valores ausentes (algumas opções)
dados_tratados <- dados_filtrados %>%
  filter(!is.na(Status) & !is.na(Currículo) & !is.na(`Período de Ingresso`))

# Substituir valores ausentes em variáveis categóricas por "Não informado":

dados_tratados$`Forma de Ingresso`[is.na(dados_tratados$`Forma de Ingresso`)] <- "Não informado"
dados_tratados$`Cor`[is.na(dados_tratados$`Cor`)] <- "Não informado"
dados_tratados$`Estado Civil`[is.na(dados_tratados$`Estado Civil`)] <- "Não informado"

# Verificar distribuição antes e depois da substituição (opcional):

table(dados_filtrados$`Cor`, useNA = "ifany")
table(dados_tratados$`Cor`)

###

# 4.2.2.2  Detecção e Tratamento de Outliers

# Visualizar a distribuição de idade
library(ggplot2)

# Histograma da idade no ingresso
ggplot(dados_filtrados, aes(x = `Idade Aproximada no Ingresso`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribuição da Idade no Ingresso", x = "Idade", y = "Frequência")

# Verificar estatísticas básicas
summary(dados_filtrados$`Idade Aproximada no Ingresso`)

# Detectar outliers usando IQR
# Calcular limites de outliers pelo método do IQR
Q1 <- quantile(dados_filtrados$`Idade Aproximada no Ingresso`, 0.25)
Q3 <- quantile(dados_filtrados$`Idade Aproximada no Ingresso`, 0.75)
IQR <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

# Visualizar os limites
cat("Limite inferior:", limite_inferior, "\n")
cat("Limite superior:", limite_superior, "\n")

# Identificar outliers
outliers <- dados_filtrados %>%
  filter(`Idade Aproximada no Ingresso` < limite_inferior |
           `Idade Aproximada no Ingresso` > limite_superior)

# Verificar quantidade
nrow(outliers)

# Excluir ou marcar outliers
# Remover outliers (se for decisão metodológica)
dados_sem_outliers <- dados_filtrados %>%
  filter(`Idade Aproximada no Ingresso` >= limite_inferior &
           `Idade Aproximada no Ingresso` <= limite_superior)

# Verificar quantidade
nrow(outliers)

###

# Transformações Realizadas nas Variáveis

# Criar a variável "Situacao_Final" com base em "Status" e "Tipo de Evasao"
dados_transformados <- dados_filtrados %>%
  mutate(Situacao_Final = case_when(
    Status == "ATIVO" ~ "Ativo",
    `Tipo de Evasão` == "GRADUADO" ~ "Graduado",
    `Tipo de Evasão` %in% c("CANCELAMENTO POR ABANDONO", "CANCELAMENTO P SOLICITACAO ALUNO", "CANCELADO 3 REPROV MESMA DISCIPLINA", 
                            "CANCELADO NOVO INGRESSO OUTRO CURSO","CANCELADO REPROVOU TODAS POR FALTAS","CANCELAMENTO DE MATRICULA",
                            "CANCELAMENTO P MUDANCA CURSO", "CANCELAMENTO P SOLICITACAO ALUNO","CANCELAMENTO POR ABANDONO","REGULAR", 
                            "TRANSFERIDO PARA OUTRA IES") ~ "Evadido",
    TRUE ~ NA_character_
  ))

# Verificar resultado
table(dados_transformados$Situacao_Final)

# Visualizar categorias originais (opcional)
table(dados_filtrados$Status)
table(dados_filtrados$`Tipo de Evasão`)

# Criar nova variável 'Situacao_Final' conforme as regras:
# - "Ativo" quando Status == "ATIVO"
# - "Graduado" quando Tipo de Evasão == "GRADUADO"
# - "Evadido" para os outros tipos de evasão (cancelamentos)

dados_transformados <- dados_filtrados %>%
  mutate(Situacao_Final = case_when(
    Status == "ATIVO" ~ "Ativo",
    `Tipo de Evasão` == "GRADUADO" ~ "Graduado",
    `Tipo de Evasão` %in% c("CANCELAMENTO POR ABANDONO", "CANCELAMENTO P SOLICITACAO ALUNO") ~ "Evadido",
    TRUE ~ NA_character_  # Caso haja algum registro não previsto
  ))

# Verificar resultado
table(dados_transformados$Situacao_Final)

###

# 4.4 Análise Exploratória dos Dados

# Converter "Período de Ingresso" para fator ordenado (caso seja string)
dados_filtrados <- dados_filtrados %>%
  mutate(`Período de Ingresso` = factor(`Período de Ingresso`, levels = sort(unique(`Período de Ingresso`))))

# Contar número de alunos por ano/período de ingresso
contagem_ingressos <- dados_filtrados %>%
  group_by(`Período de Ingresso`) %>%
  summarise(Total_Alunos = n())

# Visualizar tabela
print(contagem_ingressos)

# Gráfico de barras da distribuição por período de ingresso
ggplot(contagem_ingressos, aes(x = `Período de Ingresso`, y = Total_Alunos)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Distribuição dos Alunos por Período de Ingresso (2011-2023)",
    x = "Período de Ingresso",
    y = "Número de Alunos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###

# Certifique-se que 'Período de Ingresso' é fator ordenado
dados_filtrados <- dados_filtrados %>%
  mutate(`Período de Ingresso` = factor(`Período de Ingresso`, levels = sort(unique(`Período de Ingresso`))))

# Contagem por período
contagem_ingressos <- dados_filtrados %>%
  group_by(`Período de Ingresso`) %>%
  summarise(Total_Alunos = n())

# Verificar níveis do fator e conferir se 2023.2 está presente
print(levels(dados_filtrados$`Período de Ingresso`))
print(contagem_ingressos)

# Gráfico com valores acima das barras
ggplot(contagem_ingressos, aes(x = `Período de Ingresso`, y = Total_Alunos)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Total_Alunos), vjust = -0.5, size = 3.5) +
  labs(
    title = "Distribuição dos Alunos por Período de Ingresso (2011-2023)",
    x = "Período de Ingresso",
    y = "Número de Alunos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, max(contagem_ingressos$Total_Alunos) * 1.1)


###

# Contar número de alunos por sexo
distribuicao_genero <- dados_filtrados %>%
  group_by(Sexo) %>%
  summarise(Total = n()) %>%
  mutate(Sexo = factor(Sexo, levels = c("MASCULINO", "FEMININO")))  # organiza ordem, se necessário

# Gráfico de barras
ggplot(distribuicao_genero, aes(x = Sexo, y = Total, fill = Sexo)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(aes(label = Total), vjust = -0.3, size = 4, color = "black") +
  labs(
    title = "Distribuição de Estudantes por Gênero (2011–2023)",
    x = "Gênero",
    y = "Quantidade de Estudantes",
    fill = "Gênero"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("MASCULINO" = "#0072B2", "FEMININO" = "#D55E00")) +
  theme(legend.position = "right")



# Calcular total por gênero e percentual
distribuicao_genero <- dados_filtrados %>%
  group_by(Sexo) %>%
  summarise(Total = n()) %>%
  mutate(
    Percentual = round((Total / sum(Total)) * 100, 1),
    Label = paste0(Total, " (", Percentual, "%)"),
    Sexo = factor(Sexo, levels = c("MASCULINO", "FEMININO"))
  )

# Gráfico de barras com legenda e rótulos formatados
ggplot(distribuicao_genero, aes(x = Sexo, y = Total, fill = Sexo)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(aes(label = Label), vjust = -0.5, size = 4, color = "black") +
  labs(
    title = "Distribuição de Estudantes por Gênero (2011–2023)",
    x = "Gênero",
    y = "Quantidade de Estudantes",
    fill = "Gênero"
  ) +
  scale_fill_manual(values = c("MASCULINO" = "#0072B2", "FEMININO" = "#D55E00")) +
  theme_minimal() +
  theme(legend.position = "right")

###

# Alunos ATIVO

# Filtrar apenas alunos com status ATIVO
dados_ativos <- dados_filtrados %>%
  filter(Status == "ATIVO")

# Contar por período de ingresso
ativos_por_periodo <- dados_ativos %>%
  group_by(`Período de Ingresso`) %>%
  summarise(Total_Ativos = n()) %>%
  arrange(`Período de Ingresso`) %>%
  mutate(`Período de Ingresso` = factor(`Período de Ingresso`, levels = unique(`Período de Ingresso`)))

# Gráfico de barras - termine até (2011.1 à 2023.2)
ggplot(ativos_por_periodo, aes(x = `Período de Ingresso`, y = Total_Ativos)) +
  geom_bar(stat = "identity", fill = "#009E73") +
  geom_text(aes(label = Total_Ativos), vjust = -0.5, size = 3.5, color = "black") +
  labs(
    title = "Número de Estudantes Ativos por Período de Ingresso ",
    x = "Período de Ingresso",
    y = "Total de Ativos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, max(ativos_por_periodo$Total_Ativos) * 1.1)


###

# Filtrar apenas os alunos ativos
ativos <- dados_filtrados %>%
  filter(Status == "ATIVO")

# Contar por Sexo
ativos_por_genero <- ativos %>%
  group_by(Sexo) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

# Visualizar a tabela
print(ativos_por_genero)

# Gráfico de barras
ggplot(ativos_por_genero, aes(x = Sexo, y = Total, fill = Sexo)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribuição de Alunos Ativos por Gênero",
    x = "Sexo",
    y = "Número de Alunos Ativos"
  ) +
  theme_minimal()


###

# Filtrar apenas os alunos inativos com tipo de evasão 'GRADUADO'
graduados <- dados_filtrados %>%
  filter(Status == "INATIVO", Tipo_de_Evasao == "GRADUADO")

# Contar por Sexo
graduados_por_genero <- graduados %>%
  group_by(Sexo) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

# Visualizar a tabela
print(graduados_por_genero)

# Gráfico de barras
ggplot(graduados_por_genero, aes(x = Sexo, y = Total, fill = Sexo)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribuição de Alunos Graduados por Gênero",
    x = "Sexo",
    y = "Número de Alunos Graduados"
  ) +
  theme_minimal()

# --- Distribuição das taxas de evasão por Período e Currículo ---

# Calcular número de alunos por Período e Currículo
resumo_periodo_curriculo <- dados_transformados %>%
  group_by(`Período de Ingresso`, Currículo, Situacao_Final) %>%
  summarise(Total = n(), .groups = "drop") %>%
  group_by(`Período de Ingresso`, Currículo) %>%
  mutate(Total_Periodo_Curriculo = sum(Total),
         Taxa = round((Total / Total_Periodo_Curriculo) * 100, 1))

# Visualizar tabela resumida
print(resumo_periodo_curriculo)

# Gráfico de barras empilhadas com proporção
ggplot(resumo_periodo_curriculo, aes(x = `Período de Ingresso`, y = Taxa, fill = Situacao_Final)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Currículo, scales = "free_y") +
  labs(
    title = "Distribuição das Taxas de Evasão por Período e Currículo",
    x = "Período de Ingresso",
    y = "Taxa (%)",
    fill = "Situação Final"
  ) +
  scale_fill_manual(values = c("Ativo" = "#009E73", "Graduado" = "#0072B2", "Evadido" = "#D55E00")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Opcional: Tabela em formato gt para relatório
resumo_periodo_curriculo %>%
  select(`Período de Ingresso`, Currículo, Situacao_Final, Taxa) %>%
  gt() %>%
  tab_header(
    title = "Distribuição das Taxas de Evasão por Período e Currículo",
    subtitle = "Entre 2011 e 2023"
  )


###

### =====================================================
### Distribuição das taxas de evasão apenas Currículos 1999 e 2017
### =====================================================

# Filtrar dados válidos: excluir INATIVO não graduado
dados_curriculos <- dados_transformados %>%
  filter(Currículo %in% c("1999", "2017")) %>%
  filter(!(Status == "INATIVO" & `Tipo de Evasão` != "GRADUADO"))

# Calcular evasão por currículo
evasao_curriculo <- dados_curriculos %>%
  group_by(Currículo) %>%
  summarise(
    Total = n(),
    Evadidos = sum(Situacao_Final == "Evadido", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Taxa_Evasao = round((Evadidos / Total) * 100, 1),
         Label = paste0(Taxa_Evasao, "%"))

# Visualizar tabela
print(evasao_curriculo)


# Filtrar apenas alunos que já encerraram sua trajetória (INATIVO ≠ ativo OU GRADUADO)
finalizados <- alunos %>%
  filter(status %in% c("INATIVO", "GRADUADO"))

# Calcular evadidos e graduados por currículo
resumo_curriculo <- finalizados %>%
  group_by(curriculo) %>%
  summarise(
    graduados = sum(status == "GRADUADO"),
    evadidos = sum(status == "INATIVO" & tipo_de_evasao != "GRADUADO"),
    total_finalizados = graduados + evadidos,
    taxa_evasao = round((evadidos / total_finalizados) * 100, 2),
    taxa_conclusao = round((graduados / total_finalizados) * 100, 2),
    .groups = "drop"
  )

print(resumo_curriculo)

# Gráfico comparando graduados e evadidos por currículo
ggplot(resumo_curriculo, aes(x = curriculo)) +
  geom_col(aes(y = graduados, fill = "Graduados"), position = "stack") +
  geom_col(aes(y = evadidos, fill = "Evadidos"), position = "stack") +
  geom_text(aes(y = graduados + evadidos/2, 
                label = paste0("Conclusão: ", taxa_conclusao, "%\nEvasão: ", taxa_evasao, "%")),
            vjust = 0.5, color = "white", size = 3.5) +
  labs(
    title = "Distribuição de Concluintes e Evadidos por Currículo",
    x = "Currículo",
    y = "Número de Estudantes",
    fill = "Situação Final"
  ) +
  theme_minimal()


###

# Ver nomes das colunas
names(dados_transformados)
names(dados)
# Ver amostra das primeiras linhas
head(dados)

# Estrutura das variáveis
str(dados)

# Resumo estatístico
summary(dados)
