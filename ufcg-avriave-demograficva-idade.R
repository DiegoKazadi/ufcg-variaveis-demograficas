# ===========================================================================
# 1. Instalação e carregamento de pacotes
# ===========================================================================
pacotes <- c("janitor", "dplyr", "ggplot2", "ggthemes", "viridis", 
             "stringr", "readr", "tidyverse", "scales", "forcats", "gt")

novos_pacotes <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(novos_pacotes) > 0) {
  install.packages(novos_pacotes, dependencies = TRUE)
}

invisible(lapply(pacotes, library, character.only = TRUE))

# ===========================================================================
# 2. Definir caminho do diretório (Linux)
# ===========================================================================
caminho_base <- "/home/diego/Documentos/Semestre 2025.2/tabelas"
arquivo_alunos <- file.path(caminho_base, "alunos-final.csv")

if (!file.exists(arquivo_alunos)) {
  stop("Arquivo não encontrado: ", arquivo_alunos)
}

# ===========================================================================
# 3. Carregar e padronizar dados
# ===========================================================================
dados <- read_delim(
  arquivo_alunos,
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

dados <- janitor::clean_names(dados)

# ===========================================================================
# 4. Validar colunas essenciais
# ===========================================================================
colunas_necessarias <- c("idade_aproximada_no_ingresso", "periodo_de_ingresso",
                         "curriculo", "status", "tipo_de_evasao")

faltantes <- colunas_necessarias[!colunas_necessarias %in% names(dados)]
if (length(faltantes) > 0) {
  stop("Colunas ausentes: ", paste(faltantes, collapse = ", "))
}

# ===========================================================================
# 5. Definir quem é evadido (com base nos motivos fornecidos)
# ===========================================================================
motivos_evasao <- c(
  "CANCELAMENTO POR ABANDONO",
  "CANCELAMENTO P SOLICITACAO ALUNO",
  "CANCELADO 3 REPROV MESMA DISCIPLINA",
  "CANCELADO NOVO INGRESSO OUTRO CURSO",
  "CANCELADO REPROVOU TODAS POR FALTAS",
  "CANCELAMENTO DE MATRICULA",
  "CANCELAMENTO P MUDANCA CURSO",
  "TRANSFERIDO PARA OUTRA IES"
)

dados <- dados %>%
  mutate(
    periodo_de_ingresso = as.numeric(periodo_de_ingresso),
    # Classificar como evadido apenas se INATIVO E com motivo de evasão
    evadido = ifelse(
      status == "INATIVO" & tipo_de_evasao %in% motivos_evasao,
      1, 0
    )
  )

# ===========================================================================
# 6. Limpeza e criação de faixas etárias
# ===========================================================================
dados_limpos <- dados %>%
  # Remover NA em variáveis críticas
  filter(
    !is.na(idade_aproximada_no_ingresso),
    !is.na(periodo_de_ingresso),
    !is.na(curriculo)
  ) %>%
  # Idade razoável (15 a 70 anos)
  filter(idade_aproximada_no_ingresso >= 15 & idade_aproximada_no_ingresso <= 70) %>%
  # Criar faixas etárias
  mutate(
    faixa_idade = case_when(
      idade_aproximada_no_ingresso < 20 ~ "< 20",
      idade_aproximada_no_ingresso < 25 ~ "20-24",
      idade_aproximada_no_ingresso < 30 ~ "25-29",
      idade_aproximada_no_ingresso < 35 ~ "30-34",
      idade_aproximada_no_ingresso < 40 ~ "35-39",
      idade_aproximada_no_ingresso >= 40 ~ "40+",
      TRUE ~ NA_character_
    ),
    faixa_idade = fct_relevel(faixa_idade, "< 20", "20-24", "25-29", "30-34", "35-39", "40+")
  ) %>%
  filter(!is.na(faixa_idade))

# ===========================================================================
# 7. Filtrar: ingressantes 2011–2023 e currículos 1999 e 2017
# ===========================================================================
dados_filtrados <- dados_limpos %>%
  filter(
    between(periodo_de_ingresso, 2011, 2023),
    curriculo %in% c(1999, 2017)
  )

cat("Total após filtros (2011–2023, currículos 1999/2017):", nrow(dados_filtrados), "\n")

# ===========================================================================
# 8. Tabela de evasão por faixa etária e currículo
# ===========================================================================
tabela_idade <- dados_filtrados %>%
  group_by(curriculo, faixa_idade) %>%
  summarise(
    total = n(),
    evadidos = sum(evadido, na.rm = TRUE),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  ) %>%
  # Remover grupos com pouca amostra (opcional: total < 5)
  # filter(total >= 5)  # descomente se quiser ignorar grupos pequenos
  arrange(curriculo, faixa_idade)

# Exibir tabela
print(tabela_idade)

# Salvar tabela (opcional)
# write_csv(tabela_idade, "tabela_evasao_faixa_etaria.csv")

# ===========================================================================
# 9. Gráfico com anotações de n e taxa
# ===========================================================================
# Converter curriculo para factor com rótulos
tabela_idade <- tabela_idade %>%
  mutate(curriculo = factor(curriculo, levels = c(1999, 2017), labels = c("Currículo 1999", "Currículo 2017")))

# Gerar gráfico
p <- ggplot(tabela_idade, aes(x = faixa_idade, y = taxa_evasao, fill = curriculo)) +
  geom_col(position = "dodge", alpha = 0.85, color = "black") +
  # Rótulo da taxa de evasão
  geom_text(
    aes(label = paste0(taxa_evasao, "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.5,
    fontface = "bold"
  ) +
  # Rótulo do tamanho da amostra (n)
  geom_text(
    aes(label = paste("n =", total)),
    position = position_dodge(width = 0.9),
    vjust = 1.5,
    size = 2.8,
    color = "gray30"
  ) +
  scale_fill_manual(
    values = c("Currículo 1999" = "#1f468b", "Currículo 2017" = "#f29c11")
  ) +
  labs(
    title = "Taxa de Evasão por Faixa Etária ao Ingresso",
    subtitle = "Ingressantes entre 2011 e 2023 — Currículos 1999 vs 2017\n(Incluem: abandono, cancelamento, transferência)",
    x = "Faixa Etária ao Ingresso (anos)",
    y = "Taxa de Evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(p)

# Salvar gráfico (opcional)
# ggsave("evasao_faixa_etaria.png", p, width = 14, height = 7, dpi = 300)

# ===========================================================================
# Gráfico de Linhas: Evolução da Taxa de Evasão por Faixa Etária
# ===========================================================================

# Carregar pacotes necessários (se ainda não estiverem carregados)
# install.packages("ggplot2")
# install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Simulação de dados (substitua com seus dados reais)
# Certifique-se de que 'tabela_idade' esteja disponível no seu ambiente
# Exemplo de estrutura de dados esperada:
# tabela_idade <- data.frame(
#   faixa_idade = c("18-20", "21-23", "24-26", "27-29", "30+", "18-20", "21-23", "24-26", "27-29", "30+"),
#   curriculo = c(1999, 1999, 1999, 1999, 1999, 2017, 2017, 2017, 2017, 2017),
#   taxa_evasao = c(10, 15, 12, 8, 5, 12, 18, 15, 10, 7)
# )

# Garantir que 'curriculo' seja fator com rótulos claros
tabela_linha <- tabela_idade %>%
  mutate(
    curriculo = factor(
      curriculo,
      levels = c(1999, 2017),
      labels = c("Currículo 1999", "Currículo 2017")
    )
  )

# Verificar se os dados estão certos (opcional)
print(tabela_linha)

# Gerar gráfico
p_linhas <- ggplot(tabela_linha, aes(x = faixa_idade)) +
  # Linhas: uma para cada currículo, com cor definida
  geom_line(
    aes(y = taxa_evasao, group = curriculo, color = curriculo),
    linewidth = 1.4,
    alpha = 0.95
  ) +
  # Pontos para destacar os valores
  geom_point(
    aes(y = taxa_evasao, color = curriculo),
    size = 4
  ) +
  # Rótulos com porcentagem (acima dos pontos)
  geom_text(
    aes(y = taxa_evasao, label = paste0(taxa_evasao, "%")),
    vjust = -1.3,
    hjust = 0.5,
    size = 3.2,
    fontface = "bold",
    color = "black"
  ) +
  # Escala de cores personalizada
  scale_color_manual(
    name = "Currículo",
    values = c(
      "Currículo 1999" = "#1f468b",   # azul escuro
      "Currículo 2017" = "#f29c11"    # laranja
    ),
    breaks = c("Currículo 1999", "Currículo 2017")
  ) +
  # Títulos
  labs(
    title = "Evolução da Taxa de Evasão por Faixa Etária",
    subtitle = "Ingressantes entre 2011 e 2023 — Comparação entre Currículos",
    x = "Faixa Etária ao Ingresso (anos)",
    y = "Taxa de Evasão (%)"
  ) +
  # Eixo Y com espaço para rótulos
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, by = 10),
    expand = expansion(mult = c(0, 0.12))  # espaço extra no topo
  ) +
  # Tema: legenda à direita
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",           # legenda à direita
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "gray90"),
    legend.key.size = unit(1, "lines"),
    legend.key.height = unit(1.2, "lines"),
    legend.key.width = unit(2, "lines")
  )

# Mostrar gráfico
print(p_linhas)

# Opcional: salvar como imagem
# ggsave("evasao_linhas_comparativo.png", p_linhas, width = 12, height = 6, dpi = 300, bg = "white")






# Filtrar apenas o currículo 1999
tabela_1999 <- tabela_idade %>%
  filter(curriculo == "Currículo 1999")

# Garantir ordem correta das faixas
tabela_1999$faixa_idade <- factor(tabela_1999$faixa_idade,
                                  levels = c("< 20", "20-24", "25-29", "30-34", "35-39", "40+"))

# Ordenar por faixa
tabela_1999 <- tabela_1999 %>% arrange(faixa_idade)

# Gerar gráfico de linha
p_linha <- ggplot(tabela_1999, aes(x = faixa_idade, y = taxa_evasao, group = 1)) +
  # Linha suave conectando os pontos
  geom_line(color = "#1f468b", size = 1.1, alpha = 0.8, linetype = "solid") +
  # Pontos
  geom_point(size = 4, color = "#1f468b", fill = "white", shape = 21, stroke = 1.5) +
  # Rótulo da taxa de evasão
  geom_text(
    aes(label = paste0(taxa_evasao, "%")),
    vjust = -0.8,
    size = 3.5,
    fontface = "bold",
    color = "#1f468b"
  ) +
  # Rótulo do tamanho da amostra (n)
  geom_text(
    aes(label = paste("n =", total)),
    vjust = 1.8,
    size = 2.8,
    color = "gray30"
  ) +
  # Título e rótulos
  labs(
    title = "Evolução da Taxa de Evasão por Faixa Etária",
    subtitle = "Currículo 1999 — Ingressantes entre 2011 e 2023",
    x = "Faixa Etária ao Ingresso (anos)",
    y = "Taxa de Evasão (%)"
  ) +
  # Tema limpo
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", color = "gray30"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  # Limite do eixo Y para melhor visualização
  ylim(0, 110)

# Mostrar gráfico
print(p_linha)

# Opcional: salvar
# ggsave("evasao_curriculo_1999_linha.png", p_linha, width = 10, height = 6, dpi = 300)



# Filtrar apenas o currículo 2017
tabela_2017 <- tabela_idade %>%
  filter(curriculo == "Currículo 2017")

# Garantir ordem correta das faixas etárias
tabela_2017$faixa_idade <- factor(tabela_2017$faixa_idade,
                                  levels = c("< 20", "20-24", "25-29", "30-34", "35-39", "40+"))

# Ordenar por faixa
tabela_2017 <- tabela_2017 %>% arrange(faixa_idade)

# Gerar gráfico de linha
p_linha_2017 <- ggplot(tabela_2017, aes(x = faixa_idade, y = taxa_evasao, group = 1)) +
  # Linha em laranja (cor do currículo 2017)
  geom_line(color = "#f29c11", size = 1.1, alpha = 0.85, linetype = "solid") +
  # Pontos com borda escura
  geom_point(size = 4, color = "#f29c11", fill = "white", shape = 21, stroke = 1.5) +
  # Rótulo da taxa de evasão (em laranja escuro para contraste)
  geom_text(
    aes(label = paste0(taxa_evasao, "%")),
    vjust = -0.8,
    size = 3.5,
    fontface = "bold",
    color = "#d67a00"  # laranja mais escuro para melhor leitura
  ) +
  # Rótulo do tamanho da amostra (n)
  geom_text(
    aes(label = paste("n =", total)),
    vjust = 1.8,
    size = 2.8,
    color = "gray30"
  ) +
  # Título e rótulos
  labs(
    title = "Evolução da Taxa de Evasão por Faixa Etária",
    subtitle = "Currículo 2017 — Ingressantes entre 2011 e 2023",
    x = "Faixa Etária ao Ingresso (anos)",
    y = "Taxa de Evasão (%)"
  ) +
  # Tema limpo
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", color = "gray30"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    # Destaque sutil no fundo
    panel.background = element_rect(fill = "white")
  ) +
  # Ajustar limite do eixo Y
  ylim(0, 110)

# Mostrar gráfico
print(p_linha_2017)

# Opcional: salvar
# ggsave("evasao_curriculo_2017_linha.png", p_linha_2017, width = 10, height = 6, dpi = 300)

# ===========================================================================
# Preparar dados para o gráfico comparativo
# ===========================================================================
tabela_comparativa <- tabela_idade %>%
  # Garantir ordem das faixas
  mutate(faixa_idade = factor(faixa_idade,
                              levels = c("< 20", "20-24", "25-29", "30-34", "35-39", "40+"))) %>%
  # Ordenar
  arrange(curriculo, faixa_idade) %>%
  # Converter curriculo para factor com rótulos (já deve estar, mas garantir)
  mutate(curriculo = factor(curriculo, 
                            levels = c("Currículo 1999", "Currículo 2017")))


# ===========================================================================
# Criar gráfico com as duas linhas
# ===========================================================================
p_comparativo <- ggplot(tabela_comparativa, aes(
  x = faixa_idade,
  y = taxa_evasao,
  group = curriculo,
  color = curriculo,
  shape = curriculo
)) +
  # Linhas para cada currículo
  geom_line(size = 1.1, alpha = 0.85) +
  # Pontos com tamanhos e formas diferentes
  geom_point(size = 4, stroke = 1.2, fill = "white") +
  # Rótulos da taxa de evasão
  geom_text(
    aes(label = paste0(taxa_evasao, "%")),
    vjust = -0.8,
    size = 3.2,
    fontface = "bold",
    color = "black"
  ) +
  # Rótulos do tamanho da amostra (n)
  geom_text(
    aes(label = paste("n =", total)),
    vjust = 1.8,
    size = 2.6,
    color = "gray30"
  ) +
  # Escala de cores personalizada
  scale_color_manual(
    name = "Currículo",
    values = c("Currículo 1999" = "#1f468b", "Currículo 2017" = "#f29c11"),
    labels = c("1999", "2017")
  ) +
  # Formas diferentes para diferenciar (opcional)
  scale_shape_manual(
    name = "Currículo",
    values = c(15, 17),  # quadrado e triângulo
    labels = c("1999", "2017")
  ) +
  # Título e rótulos
  labs(
    title = "Evolução da Taxa de Evasão por Faixa Etária",
    subtitle = "Comparação entre os Currículos 1999 e 2017 ",
    x = "Faixa Etária ao Ingresso (anos)",
    y = "Taxa de Evasão (%)"
  ) +
  # Tema limpo
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", face = "italic"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",  # legenda no topo
    legend.title = element_text(face = "bold"),
    legend.box.spacing = unit(0.3, "cm")
  ) +
  # Ajustar limite do eixo Y
  ylim(0, 110)

# Mostrar gráfico
print(p_comparativo)

# Opcional: salvar
# ggsave("evasao_comparativo_1999_vs_2017.png", 
#        p_comparativo, width = 12, height = 7, dpi = 300, bg = "white")





# ===========================================================================
# 1. Preparação: Limpeza e filtro de estado_civil
# ===========================================================================
# Certifique-se de que a coluna existe
if (!"estado_civil" %in% names(dados_limpos)) {
  stop("Coluna 'estado_civil' não encontrada.")
}

# Limpar valores inválidos (ex: "-") e converter para factor
dados_estado <- dados_filtrados %>%
  mutate(
    estado_civil = na_if(estado_civil, "-"),  # transformar "-" em NA
    estado_civil = ifelse(estado_civil %in% c("Solteiro", "Casado", "Divorciado"), 
                          estado_civil, NA_character_),
    # Converter para factor com ordem personalizada (opcional)
    estado_civil = factor(estado_civil, levels = c("Solteiro", "Casado", "Divorciado"))
  ) %>%
  # Remover linhas com estado_civil NA
  filter(!is.na(estado_civil))

# Mensagem
cat("Total após limpeza de estado civil:", nrow(dados_estado), "\n")

# ===========================================================================
# 2. Tabela de evasão por estado civil e currículo
# ===========================================================================
tabela_estado <- dados_estado %>%
  group_by(curriculo, estado_civil) %>%
  summarise(
    total = n(),
    evadidos = sum(evadido, na.rm = TRUE),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  ) %>%
  # Converter curriculo para rótulo
  mutate(curriculo = factor(curriculo, 
                            levels = c(1999, 2017), 
                            labels = c("Currículo 1999", "Currículo 2017"))) %>%
  # Ordenar
  arrange(curriculo, estado_civil)

# Exibir tabela
print(tabela_estado)

# Salvar tabela (opcional)
# write_csv(tabela_estado, "tabela_evasao_estado_civil.csv")

# ===========================================================================
# 3. Gráfico: Evasão por Estado Civil (com legenda à direita)
# ===========================================================================
p_estado <- ggplot(tabela_estado, 
                   aes(x = estado_civil, y = taxa_evasao, fill = curriculo)) +
  geom_col(position = "dodge", alpha = 0.85, color = "black", width = 0.7) +
  # Rótulos da taxa
  geom_text(
    aes(label = paste0(taxa_evasao, "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.3,
    size = 3.2,
    fontface = "bold"
  ) +
  # Rótulos do n (tamanho da amostra)
  geom_text(
    aes(label = paste("n =", total)),
    position = position_dodge(width = 0.7),
    vjust = 1.5,
    size = 2.8,
    color = "gray30"
  ) +
  # Cores personalizadas
  scale_fill_manual(
    values = c("Currículo 1999" = "#1f468b", "Currículo 2017" = "#f29c11"),
    labels = c("1999", "2017")
  ) +
  labs(
    title = "Taxa de Evasão por Estado Civil ao Ingresso",
    subtitle = "Ingressantes entre 2011 e 2023 — Currículos 1999 vs 2017",
    x = "Estado Civil",
    y = "Taxa de Evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",  # legenda à direita
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  # Ajustar limite do eixo Y
  ylim(0, max(tabela_estado$taxa_evasao, na.rm = TRUE) * 1.15)

# Mostrar gráfico
print(p_estado)

# Salvar (opcional)
# ggsave("evasao_estado_civil.png", p_estado, width = 10, height = 6, dpi = 300, bg = "white")


# ===========================================================================
# 1. Preparação: Limpeza e filtro de estado_civil (repetido do anterior)
# ===========================================================================
# Verificar coluna
if (!"estado_civil" %in% names(dados_limpos)) {
  stop("Coluna 'estado_civil' não encontrada.")
}

# Limpar e preparar dados
dados_estado <- dados_filtrados %>%
  mutate(
    estado_civil = na_if(estado_civil, "-"),  # "-" → NA
    estado_civil = ifelse(estado_civil %in% c("Solteiro", "Casado", "Divorciado"), 
                          estado_civil, NA_character_),
    # Converter para factor com ordem desejada
    estado_civil = factor(estado_civil, levels = c("Solteiro", "Casado", "Divorciado"))
  ) %>%
  filter(!is.na(estado_civil))

cat("Total após limpeza de estado civil:", nrow(dados_estado), "\n")

# ===========================================================================
# 2. Tabela de evasão por estado civil e currículo
# ===========================================================================
tabela_estado <- dados_estado %>%
  group_by(curriculo, estado_civil) %>%
  summarise(
    total = n(),
    evadidos = sum(evadido, na.rm = TRUE),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  ) %>%
  # Converter curriculo para rótulo
  mutate(curriculo = factor(curriculo, 
                            levels = c(1999, 2017), 
                            labels = c("Currículo 1999", "Currículo 2017"))) %>%
  # Ordenar por estado_civil conforme o factor
  arrange(curriculo, estado_civil)

# Exibir tabela
print(tabela_estado)

# Salvar tabela (opcional)
# write_csv(tabela_estado, "tabela_evasao_estado_civil_linha.csv")

# ===========================================================================
# 3. Gráfico de linhas paralelas (como no gráfico de faixa etária)
# ===========================================================================
p_linha_estado <- ggplot(tabela_estado, aes(
  x = estado_civil,
  y = taxa_evasao,
  group = curriculo,
  color = curriculo
)) +
  # Linhas conectando os pontos
  geom_line(size = 1.1, alpha = 0.85) +
  # Pontos estilizados
  geom_point(size = 4, stroke = 1.2, fill = "white", shape = 21) +
  # Rótulo da taxa de evasão
  geom_text(
    aes(label = paste0(taxa_evasao, "%")),
    vjust = -0.8,
    size = 3.2,
    fontface = "bold",
    color = "black"
  ) +
  # Rótulo do tamanho da amostra (n)
  geom_text(
    aes(label = paste("n =", total)),
    vjust = 1.8,
    size = 2.8,
    color = "gray30"
  ) +
  # Cores personalizadas
  scale_color_manual(
    values = c("Currículo 1999" = "#1f468b", "Currículo 2017" = "#f29c11")
  ) +
  # Título e rótulos
  labs(
    title = "Evolução da Taxa de Evasão por Estado Civil",
    subtitle = "Comparação entre os Currículos 1999 e 2017",
    x = "Estado Civil ao Ingresso",
    y = "Taxa de Evasão (%)",
    color = "Currículo"
  ) +
  # Tema limpo
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30", face = "italic"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right",  # legenda à direita
    legend.title = element_text(face = "bold")
  ) +
  # Ajustar limite do eixo Y
  ylim(0, max(tabela_estado$taxa_evasao, na.rm = TRUE) * 1.15)

# Mostrar gráfico
print(p_linha_estado)

# Salvar (opcional)
# ggsave("evasao_estado_civil_linhas.png", p_linha_estado, width = 10, height = 6, dpi = 300, bg = "white")



# ===========================================================================
# 1. Preparação: Limpeza e filtro de estado_civil
# ===========================================================================
if (!"estado_civil" %in% names(dados_limpos)) {
  stop("Coluna 'estado_civil' não encontrada.")
}

dados_estado <- dados_filtrados %>%
  mutate(
    estado_civil = na_if(estado_civil, "-"),
    estado_civil = ifelse(estado_civil %in% c("Solteiro", "Casado", "Divorciado"), 
                          estado_civil, NA_character_),
    estado_civil = factor(estado_civil, levels = c("Solteiro", "Casado", "Divorciado"))
  ) %>%
  filter(!is.na(estado_civil))

cat("Total após limpeza de estado civil:", nrow(dados_estado), "\n")

# ===========================================================================
# 2. Tabela de evasão por estado civil e currículo
# ===========================================================================
tabela_estado <- dados_estado %>%
  group_by(curriculo, estado_civil) %>%
  summarise(
    total = n(),
    evadidos = sum(evadido, na.rm = TRUE),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(curriculo = factor(curriculo, 
                            levels = c(1999, 2017), 
                            labels = c("Currículo 1999", "Currículo 2017"))) %>%
  arrange(curriculo, estado_civil)

print(tabela_estado)

# ===========================================================================
# 3. Gráfico com linhas e formas distintas (quadrado e triângulo)
# ===========================================================================
p_linha_estado <- ggplot(tabela_estado, aes(
  x = estado_civil,
  y = taxa_evasao,
  group = curriculo,
  color = curriculo,
  shape = curriculo
)) +
  # Linhas conectando os pontos
  geom_line(size = 1.1, alpha = 0.85) +
  # Pontos com formas diferentes
  geom_point(size = 4, stroke = 1.2, fill = "white") +
  # Rótulos da taxa de evasão
  geom_text(
    aes(label = paste0(taxa_evasao, "%")),
    vjust = -0.8,
    size = 3.2,
    fontface = "bold",
    color = "black"
  ) +
  # Rótulos do tamanho da amostra (n)
  geom_text(
    aes(label = paste("n =", total)),
    vjust = 1.8,
    size = 2.8,
    color = "gray30"
  ) +
  # Cores personalizadas
  scale_color_manual(
    values = c("Currículo 1999" = "#1f468b", "Currículo 2017" = "#f29c11")
  ) +
  # Formas: quadrado (15) para 1999, triângulo (17) para 2017
  scale_shape_manual(
    values = c("Currículo 1999" = 15, "Currículo 2017" = 17),
    name = "Currículo"
  ) +
  # Título e rótulos
  labs(
    title = "Evolução da Taxa de Evasão por Estado Civil",
    subtitle = "Comparação entre os Currículos 1999 e 2017",
    x = "Estado Civil ao Ingresso",
    y = "Taxa de Evasão (%)",
    color = "Currículo",
    shape = "Currículo"
  ) +
  # Tema limpo
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30", face = "italic"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.box.spacing = unit(0.3, "cm")
  ) +
  ylim(0, max(tabela_estado$taxa_evasao, na.rm = TRUE) * 1.15)

# Mostrar gráfico
print(p_linha_estado)

# Salvar (opcional)
# ggsave("evasao_estado_civil_linhas_formas.png", p_linha_estado, width = 10, height = 6, dpi = 300, bg = "white")






# ===========================================================================
# 1. Verificar e preparar a coluna de cor/raça
# ===========================================================================
colunas <- names(dados_limpos)
cor_raca_cols <- grep("cor|raca|raça|etnia", colunas, ignore.case = TRUE, value = TRUE)

if (length(cor_raca_cols) == 0) {
  stop("Nenhuma coluna relacionada a cor/raça encontrada. Verifique os nomes.")
}

nome_coluna_cor <- cor_raca_cols[1]
dados_cor <- dados_filtrados %>%
  rename(cor_raca = !!nome_coluna_cor) %>%
  mutate(
    cor_raca = trimws(toupper(cor_raca)),
    cor_raca = case_when(
      cor_raca %in% c("BRANCA", "B") ~ "Branca",
      cor_raca %in% c("PRETA", "P") ~ "Preta",
      cor_raca %in% c("PARDA", "PARD", "Parda (morena)") ~ "Parda",
      cor_raca %in% c("AMARELA", "A") ~ "Amarela",
      cor_raca %in% c("INDÍGENA", "INDIGENA", "I") ~ "Indígena",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cor_raca))

# Fator com ordem fixa
ordem_cor <- c("Branca", "Preta", "Parda", "Amarela", "Indígena")
dados_cor$cor_raca <- factor(dados_cor$cor_raca, levels = ordem_cor)

cat("Total após limpeza de cor/raça:", nrow(dados_cor), "\n")

# ===========================================================================
# 2. Tabela de evasão por cor/raça e currículo
# ===========================================================================
tabela_cor <- dados_cor %>%
  group_by(curriculo, cor_raca) %>%
  summarise(
    total = n(),
    evadidos = sum(evadido, na.rm = TRUE),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(curriculo = factor(curriculo,
                            levels = c(1999, 2017),
                            labels = c("Currículo 1999", "Currículo 2017"))) %>%
  arrange(curriculo, cor_raca)

print(tabela_cor)

# Salvar (opcional)
# write_csv(tabela_cor, "tabela_evasao_cor_raca.csv")

# ===========================================================================
# 3. Gráfico com linhas, quadrado (1999) e triângulo (2017)
# ===========================================================================
p_cor <- ggplot(tabela_cor, aes(
  x = cor_raca,
  y = taxa_evasao,
  group = curriculo,
  color = curriculo,
  shape = curriculo
)) +
  geom_line(size = 1.1, alpha = 0.85) +
  geom_point(size = 4.5, stroke = 1.2, fill = "white", shape = 21) +  # borda escura, fundo branco
  # Rótulo da taxa
  geom_text(
    aes(label = paste0(taxa_evasao, "%")),
    vjust = -0.8,
    size = 3.3,
    fontface = "bold",
    color = "black"
  ) +
  # Rótulo do n
  geom_text(
    aes(label = paste("n =", total)),
    vjust = 1.8,
    size = 2.8,
    color = "gray30"
  ) +
  # Cores
  scale_color_manual(
    values = c("Currículo 1999" = "#1f468b", "Currículo 2017" = "#f29c11"),
    name = "Currículo"
  ) +
  # Formas: quadrado (15) e triângulo (17)
  scale_shape_manual(
    values = c("Currículo 1999" = 15, "Currículo 2017" = 17),
    name = "Currículo"
  ) +
  labs(
    title = "Evolução da Taxa de Evasão por Cor/Raça",
    subtitle = "Comparação entre os Currículos 1999 e 2017",
    x = "Cor/Raça",
    y = "Taxa de Evasão (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30", face = "italic"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  ylim(0, max(tabela_cor$taxa_evasao, na.rm = TRUE) * 1.15)

# Mostrar gráfico
print(p_cor)

# Salvar (opcional)
# ggsave("evasao_cor_raca_linhas.png", p_cor, width = 11, height = 6, dpi = 300, bg = "white")



# ===========================================================================
# 3. Gráfico de linhas paralelas com formas distintas: QUADRADO (1999) e TRIÂNGULO (2017)
# ===========================================================================
p_cor <- ggplot(tabela_cor, aes(
  x = cor_raca,
  y = taxa_evasao,
  group = curriculo,
  color = curriculo,
  shape = curriculo
)) +
  # Linhas conectando os pontos
  geom_line(size = 1.1, alpha = 0.85) +
  # Pontos com formas distintas: quadrado (15) e triângulo (17), borda escura, preenchimento branco
  geom_point(
    size = 4.5,
    stroke = 1.2,
    fill = "white",  # preenchimento branco
    aes(shape = curriculo)  # mapeia forma por currículo
  ) +
  # Rótulo da taxa de evasão
  geom_text(
    aes(label = paste0(taxa_evasao, "%")),
    vjust = -0.8,
    size = 3.3,
    fontface = "bold",
    color = "black"
  ) +
  # Rótulo do tamanho da amostra (n)
  geom_text(
    aes(label = paste("n =", total)),
    vjust = 1.8,
    size = 2.8,
    color = "gray30"
  ) +
  # Cores personalizadas
  scale_color_manual(
    values = c("Currículo 1999" = "#1f468b", "Currículo 2017" = "#f29c11"),
    name = "Currículo"
  ) +
  # Formas: quadrado (15) para 1999, triângulo (17) para 2017
  scale_shape_manual(
    values = c("Currículo 1999" = 15, "Currículo 2017" = 17),
    name = "Currículo"
  ) +
  # Título e rótulos
  labs(
    title = "Evolução da Taxa de Evasão por Cor/Raça",
    subtitle = "Comparação entre os Currículos 1999 e 2017",
    x = "Cor/Raça",
    y = "Taxa de Evasão (%)"
  ) +
  # Tema limpo
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30", face = "italic"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  # Ajustar limite do eixo Y
  ylim(0, max(tabela_cor$taxa_evasao, na.rm = TRUE) * 1.15)

# Mostrar gráfico
print(p_cor)


# ===========================================================================
# Análise da variável: Tipo de Evasão
# ===========================================================================

# Filtrar apenas alunos evadidos (removendo "GRADUADO" se não for evasão)
tabela_tipo <- dados %>%
  filter(status == "INATIVO", tipo_de_evasao != "GRADUADO") %>%
  group_by(curriculo, tipo_de_evasao) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(curriculo) %>%
  mutate(percentual = round(100 * total / sum(total), 1))

# Exibir tabela
print(tabela_tipo)

# Padronizar labels dos currículos
tabela_tipo <- tabela_tipo %>%
  mutate(curriculo = factor(curriculo,
                            levels = c(1999, 2017),
                            labels = c("Currículo 1999", "Currículo 2017")))

# ===========================================================================
# Gráfico de barras lado a lado
# ===========================================================================
p_barras <- ggplot(tabela_tipo, aes(x = tipo_de_evasao, y = percentual,
                                    fill = curriculo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(percentual, "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Currículo 1999" = "#1f468b",
                               "Currículo 2017" = "#f29c11")) +
  labs(
    title = "Distribuição da Evasão por Tipo",
    subtitle = "Currículos 1999 e 2017 — Ingressantes 2011–2023",
    x = "Tipo de Evasão",
    y = "Percentual de Estudantes (%)",
    fill = "Currículo"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

print(p_barras)







# ===========================================================================
# 3. Análise da variável: Tipo de Evasão (sem NA)
# ===========================================================================
# Filtrar apenas inativos, não graduados, e remover NA/vazios
tabela_tipo <- dados %>%
  filter(
    status == "INATIVO",
    tipo_de_evasao != "GRADUADO",
    !is.na(tipo_de_evasao),
    tipo_de_evasao != "",
    trimws(tipo_de_evasao) != ".",
    trimws(tipo_de_evasao) != "-"
  ) %>%
  # Limpar e padronizar os tipos de evasão
  mutate(
    tipo_de_evasao = trimws(tipo_de_evasao),
    tipo_de_evasao = case_when(
      str_detect(tipo_de_evasao, regex("ABANDONO", ignore_case = TRUE)) ~ "Abandono",
      str_detect(tipo_de_evasao, regex("SOLICITACAO", ignore_case = TRUE)) ~ "Solicitação do Aluno",
      str_detect(tipo_de_evasao, regex("REPROV MESMA DISCIPLINA", ignore_case = TRUE)) ~ "3 Reprovas na Mesma Disciplina",
      str_detect(tipo_de_evasao, regex("NOVO INGRESSO OUTRO CURSO", ignore_case = TRUE)) ~ "Novo Ingresso em Outro Curso",
      str_detect(tipo_de_evasao, regex("REPROVOU TODAS POR FALTAS", ignore_case = TRUE)) ~ "Reprovou Todas por Faltas",
      str_detect(tipo_de_evasao, regex("CANCELAMENTO DE MATRICULA", ignore_case = TRUE)) ~ "Cancelamento de Matrícula",
      str_detect(tipo_de_evasao, regex("MUDANCA CURSO", ignore_case = TRUE)) ~ "Mudança de Curso",
      str_detect(tipo_de_evasao, regex("TRANSFERIDO PARA OUTRA IES", ignore_case = TRUE)) ~ "Transferido para Outra IES",
      TRUE ~ tipo_de_evasao
    ),
    # Converter curriculo para factor com rótulos
    curriculo = factor(curriculo, levels = c(1999, 2017), labels = c("Currículo 1999", "Currículo 2017"))
  ) %>%
  # Remover linhas com curriculo NA (se houver)
  filter(!is.na(curriculo)) %>%
  # Agrupar por currículo e tipo de evasão
  group_by(curriculo, tipo_de_evasao) %>%
  summarise(total = n(), .groups = "drop") %>%
  # Calcular percentual dentro de cada currículo
  group_by(curriculo) %>%
  mutate(percentual = round(100 * total / sum(total), 1)) %>%
  ungroup()

# ===========================================================================
# 4. Ordenar tipos de evasão por frequência no Currículo 1999
# ===========================================================================
ordem_tipos <- tabela_tipo %>%
  filter(curriculo == "Currículo 1999") %>%
  arrange(desc(percentual)) %>%
  pull(tipo_de_evasao)

tabela_tipo <- tabela_tipo %>%
  mutate(tipo_de_evasao = factor(tipo_de_evasao, levels = ordem_tipos))

# ===========================================================================
# 5. Gráfico de linhas: Evasão por Tipo
# ===========================================================================
p_linhas <- ggplot(tabela_tipo, aes(
  x = tipo_de_evasao,
  y = percentual,
  group = curriculo,
  color = curriculo,
  shape = curriculo
)) +
  geom_line(size = 1.2, alpha = 0.9) +
  geom_point(size = 3.5, stroke = 1.1, fill = "white") +
  geom_text(
    aes(label = paste0(percentual, "%")),
    vjust = -1.2,
    size = 3,
    color = "black",
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c("Currículo 1999" = "#1f468b", "Currículo 2017" = "#f29c11")
  ) +
  scale_shape_manual(
    values = c("Currículo 1999" = 15, "Currículo 2017" = 17)
  ) +
  labs(
    title = "Evolução da Evasão por Tipo",
    subtitle = "Currículos 1999 e 2017 — Ingressantes 2011–2023",
    x = "Tipo de Evasão",
    y = "Percentual de Estudantes (%)",
    color = "Currículo",
    shape = "Currículo"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

# Mostrar gráfico
print(p_linhas)

# Opcional: salvar
# ggsave("evasao_por_tipo_linhas.png", p_linhas, width = 12, height = 6, dpi = 300, bg = "white")


