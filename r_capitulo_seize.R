# ===========================================================================
# 1. Definir caminho e arquivo de entrada
# ===========================================================================
caminho_base <- "D:/Documents/Mestrado UFCG/Semestre 2025.2/Tabelas"
arquivo_alunos <- file.path(caminho_base, "alunos-final.csv")

# Verificação de existência do arquivo
if (!file.exists(arquivo_alunos)) {
  stop("❌ Arquivo não encontrado: ", arquivo_alunos)
}

# ===========================================================================
# 2. Carregar e padronizar dados
# ===========================================================================
library(readr)
library(janitor)

dados <- read_delim(
  arquivo_alunos,
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) |> 
  janitor::clean_names()

# Mensagem de sucesso
cat("✅ Dados carregados com sucesso!\n")
cat("📊 Linhas:", nrow(dados), "| Colunas:", ncol(dados), "\n")

# ===========================================================================
# 3. Limpeza e padronização dos dados
# ===========================================================================

# Remover colunas completamente vazias
dados <- dados |> janitor::remove_empty(which = "cols")

# Remover linhas completamente vazias
dados <- dados |> janitor::remove_empty(which = "rows")

# Padronizar todas as colunas de texto:
# - Trim de espaços
# - Converter "" em NA
# - Colocar em minúsculas
dados <- dados |> 
  mutate(across(where(is.character), ~ na_if(trimws(.), ""))) |> 
  mutate(across(where(is.character), tolower))

# Remover duplicatas exatas
dados <- dados |> distinct()

# ===========================================================================
# 4. Resumo pós-limpeza
# ===========================================================================
cat("✅ Dados limpos e padronizados!\n")
cat("📊 Linhas:", nrow(dados), "| Colunas:", ncol(dados), "\n")

# Visualizar primeiras linhas
print(head(dados, 5))
