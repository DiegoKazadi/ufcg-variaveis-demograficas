# -----------------------------
# Script R: Cálculo de Média e Desvio Padrão das Taxas de Evasão por Sexo e Currículo
# -----------------------------

# 1. Criar o data frame com os dados fornecidos
dados <- data.frame(
  periodo = c("1º Período", "1º Período", "1º Período", "1º Período",
              "2º Período", "2º Período", "2º Período", "2º Período",
              "3º Período", "3º Período", "3º Período", "3º Período",
              "4º Período", "4º Período", "4º Período", "4º Período"),
  curriculo = c("1999", "1999", "2017", "2017",
                "1999", "1999", "2017", "2017",
                "1999", "1999", "2017", "2017",
                "1999", "1999", "2017", "2017"),
  sexo = c("FEMININO", "MASCULINO", "FEMININO", "MASCULINO",
           "FEMININO", "MASCULINO", "FEMININO", "MASCULINO",
           "FEMININO", "MASCULINO", "FEMININO", "MASCULINO",
           "FEMININO", "MASCULINO", "FEMININO", "MASCULINO"),
  taxa_evasao = c(21.5, 16.6, 4.97, 4.42,
                  9.92, 10.1, 4.49, 3.76,
                  8.26, 10.8, 4.0, 2.02,
                  4.96, 10.7, 4.19, 3.67),
  stringsAsFactors = FALSE
)

# 2. Carregar pacote dplyr para manipulação de dados (se não tiver, instale com install.packages("dplyr"))
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

# 3. Calcular média e desvio padrão das taxas de evasão
# Agrupando por currículo e sexo
resumo <- dados %>%
  group_by(curriculo, sexo) %>%
  summarise(
    media_taxa = round(mean(taxa_evasao), 2),
    desvio_padrao = round(sd(taxa_evasao), 2),
    .groups = 'drop'
  )

# 4. Exibir o resultado
print("Média e Desvio Padrão das Taxas de Evasão por Sexo e Currículo:")
print(resumo)

# Opcional: salvar o resultado em um arquivo CSV
# write.csv(resumo, "resumo_taxas_evasao.csv", row.names = FALSE)

# -----------------------------
# Script R: Boxplot das Taxas de Evasão por Sexo e Currículo
# -----------------------------
## -----------------------------
# Script R: Boxplot das Taxas de Evasão por Sexo e Currículo
# Eixo X: Período | Grupos: Sexo/Currículo | Legenda: Sexo
# -----------------------------

# 1. Criar o data frame com os dados fornecidos
dados_taxas <- data.frame(
  periodo = rep(c("1º Período", "2º Período", "3º Período", "4º Período"), each = 4),
  curriculo = rep(c("1999", "1999", "2017", "2017"), times = 4),
  sexo = rep(c("FEMININO", "MASCULINO", "FEMININO", "MASCULINO"), times = 4),
  total_ingressantes = c(121, 674, 181, 859, 121, 674, 178, 850, 121, 674, 175, 841, 121, 674, 167, 817),
  total_evadidos = c(26, 112, 9, 38, 12, 68, 8, 32, 10, 73, 7, 17, 6, 72, 7, 30),
  taxa_evasao = c(21.5, 16.6, 4.97, 4.42, 9.92, 10.1, 4.49, 3.76, 8.26, 10.8, 4.0, 2.02, 4.96, 10.7, 4.19, 3.67),
  stringsAsFactors = FALSE
)

# 2. Carregar pacotes necessários
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

# 3. Função para simular uma distribuição de taxas de evasão
simular_taxas <- function(evadidos, total, n_sim = 100) {
  if (total == 0 || is.na(evadidos) || is.na(total)) {
    return(rep(0, n_sim))
  }
  # Parâmetros da distribuição Beta para suavizar
  alpha <- evadidos + 1
  beta <- (total - evadidos) + 1
  # Gerar n_sim proporções simuladas
  prop_simuladas <- rbeta(n_sim, alpha, beta)
  return(prop_simuladas * 100) # Converter para porcentagem
}

# 4. Simular dados para cada linha
set.seed(123) # Para reprodutibilidade
dados_simulados <- dados_taxas %>%
  rowwise() %>%
  mutate(
    taxas_simuladas = list(simular_taxas(total_evadidos, total_ingressantes, n_sim = 100))
  ) %>%
  unnest(taxas_simuladas) %>%
  rename(taxa_evasao_simulada = taxas_simuladas)

# 5. Definir ordem dos períodos
dados_simulados$periodo <- factor(dados_simulados$periodo, levels = c("1º Período", "2º Período", "3º Período", "4º Período"))

# 6. Criar o boxplot
# Eixo X: Período
# Grupos (posição): Interação de sexo e currículo (separados por posição)
# Cor de preenchimento: Sexo
grafico_boxplot <- ggplot(dados_simulados, aes(x = periodo, y = taxa_evasao_simulada)) +
  geom_boxplot(
    aes(fill = sexo, color = curriculo), # Preenchimento por sexo, cor da linha por currículo
    position = position_dodge2(preserve = "single", padding = 0.1),
    outlier.size = 0.5,
    alpha = 0.7 # Transparência para melhor visualização
  ) +
  labs(
    title = "Distribuição das Taxas de Evasão por Período",
    subtitle = "Comparação por Sexo e Currículo",
    x = "Período Acadêmico",
    y = "Taxa de Evasão (%)",
    fill = "Sexo",
    color = "Currículo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(
    values = c("MASCULINO" = "#1f77b4", "FEMININO" = "#ff7f0e"),
    name = "Sexo"
  ) +
  scale_color_manual(
    values = c("1999" = "black", "2017" = "red"),
    name = "Currículo"
  )

# 7. Exibir o gráfico
print(grafico_boxplot)
