# -----------------------------
# Script R: Boxplot das Taxas de Evasão por Sexo e Currículo
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

# 2. Carregar pacote ggplot2
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# 3. Criar o boxplot das taxas de evasão
# (Este boxplot mostra a distribuição das taxas de evasão para cada combinação de grupo)
p1 <- ggplot(dados_taxas, aes(x = interaction(sexo, curriculo), y = taxa_evasao, fill = sexo)) +
  geom_boxplot() +
  facet_wrap(~ periodo, scales = "free_x") + # Facetas por período
  labs(
    title = "Distribuição das Taxas de Evasão por Sexo e Currículo",
    x = "Grupo (Sexo e Currículo)",
    y = "Taxa de Evasão (%)",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("MASCULINO" = "#1f77b4", "FEMININO" = "#ff7f0e"))

# 4. Exibir o gráfico
print(p1)

# --- Opção Alternativa: Simular dados brutos e plotar ---
# Esta parte simula os dados individuais de alunos (evadiu/não evadiu)
# e calcula a taxa de evasão para cada simulação, criando uma distribuição.

# 5. Função para simular uma taxa de evasão com base em uma binomial
simular_taxa <- function(evadidos, total) {
  if (total == 0) return(0)
  # Simula uma proporção ligeiramente variável em torno da taxa observada
  # Usando uma distribuição Beta para suavizar
  alpha <- evadidos + 1
  beta <- (total - evadidos) + 1
  prop_simulada <- rbeta(100, alpha, beta) # 100 simulações
  return(prop_simulada * 100) # Converter para porcentagem
}

# 6. Simular dados para cada linha
set.seed(123) # Para reprodutibilidade
dados_simulados <- dados_taxas %>%
  rowwise() %>%
  summarise(
    periodo = periodo,
    curriculo = curriculo,
    sexo = sexo,
    taxas_simuladas = list(simular_taxa(total_evadidos, total_ingressantes)),
    .groups = 'drop'
  ) %>%
  tidyr::unnest(taxas_simuladas) # Desembrulhar a lista de simulações

# 7. Criar boxplot com os dados simulados
p2 <- ggplot(dados_simulados, aes(x = interaction(sexo, curriculo), y = taxas_simuladas, fill = sexo)) +
  geom_boxplot(outlier.size = 0.8) + # Menor tamanho dos outliers
  facet_wrap(~ periodo, scales = "free_x") +
  labs(
    title = "Distribuição Simulada das Taxas de Evasão por Sexo e Currículo",
    x = "Grupo (Sexo e Currículo)",
    y = "Taxa de Evasão Simulada (%)",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("MASCULINO" = "#1f77b4", "FEMININO" = "#ff7f0e"))

# 8. Exibir o gráfico com dados simulados
print(p2)

# Nota: O segundo gráfico (p2) é mais representativo de uma distribuição,
#       mas requer simulação. O primeiro (p1) plota diretamente os pontos das taxas observadas.