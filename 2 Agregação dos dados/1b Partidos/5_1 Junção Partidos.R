### 5_1 Junção Partidos ---------------------------------------------------------
# Bruno Schaefer (IESP-UERJ)
#
# Objetivo: acrescentar aos bancos do código 4 (direção + financiamento
# partidário, por nível) as características nacionais dos partidos do código 5
# (ideologia BLS e Bolognesi, democracia interna de Salgado e PSI/IPD de
# Ribeiro et al.).
#
# Entradas:
#   - 2 Agregação dos dados/1b Partidos/bancos (saídas do código 4):
#     banco_nacional_comissao_executiva.xlsx,
#     banco_nacional_orgao_direcao.xlsx, banco_estadual.xlsx,
#     banco_municipal.csv
#   - 1 Pesquisas individuais/4a Produtos/Partidos (código 5):
#     banco_partidos_ano.xlsx (partido-ano, 2015–2025)
#
# Saída: 2 Agregação dos dados/1b Partidos/bancos (banco_partidos_*), um banco por nível, com as mesmas linhas do código 4
# (unidade partidária-ano) e as variáveis do partido no nível nacional.
#
# Chave de junção: ano e sigla padronizada (SG_PARTIDO_PAD), a mesma usada no
# código 4. As características do partido são as mesmas para todas as
# unidades (estados e municípios) do partido no ano.

library(here)
source(here("0 Funções, estrutura e passo-a-passo", "0_pastas.R"))
library(tidyverse)
library(openxlsx)
library(data.table)


### Dados ---------------------------------------------------------------------

pasta_juncao  <- pasta$juncao_partidos
pasta_saida   <- pasta$juncao_partidos
dir.create(pasta_saida, showWarnings = FALSE)

partidos <- read.xlsx(
  file.path(pasta$prod_partidos, "banco_partidos_ano.xlsx")
)

# Uma linha por partido-ano
stopifnot(nrow(count(partidos, SG_PARTIDO_PAD, ANO) |> filter(n > 1)) == 0)

bancos <- list(
  nacional_comissao_executiva = read.xlsx(file.path(pasta_juncao, "banco_nacional_comissao_executiva.xlsx")),
  nacional_orgao_direcao      = read.xlsx(file.path(pasta_juncao, "banco_nacional_orgao_direcao.xlsx")),
  estadual                    = read.xlsx(file.path(pasta_juncao, "banco_estadual.xlsx")),
  municipal                   = fread(file.path(pasta_juncao, "banco_municipal.csv"), sep = ";",
                                      colClasses = list(character = "CD_MUNICIPIO"))
)


### Junção --------------------------------------------------------------------

juntar_partidos <- function(df, rotulo) {

  base <- df |>
    as_tibble() |>
    left_join(partidos, by = c("SG_PARTIDO_PAD", "ANO"), relationship = "many-to-one")

  # A junção não pode criar linhas
  stopifnot(nrow(base) == nrow(df))

  cat("\n===", rotulo, "===\n")
  cat("Linhas:", nrow(base), "\n")

  # Cobertura das variáveis do partido
  base |>
    summarise(
      bls       = mean(!is.na(bls_ideologia)),
      bolognesi = mean(!is.na(bolognesi_ideologia)),
      salgado   = mean(!is.na(salgado_democracia_interna)),
      psi       = mean(!is.na(ribeiro_psi))
    ) |>
    mutate(across(everything(), ~ round(100 * .x, 1))) |>
    print()

  # Partidos sem nenhuma medida de ideologia
  base |>
    filter(is.na(bls_ideologia), is.na(bolognesi_ideologia)) |>
    count(SG_PARTIDO_PAD, sort = TRUE) |>
    print(n = 20)

  base
}

resultado <- imap(bancos, juntar_partidos)


### Salvar bancos -------------------------------------------------------------
# O banco municipal é salvo em .csv (separador ";", UTF-8) por causa do tamanho.

write.xlsx(resultado$nacional_comissao_executiva,
           file.path(pasta_saida, "banco_partidos_nacional_comissao_executiva.xlsx"), overwrite = TRUE)
write.xlsx(resultado$nacional_orgao_direcao,
           file.path(pasta_saida, "banco_partidos_nacional_orgao_direcao.xlsx"), overwrite = TRUE)
write.xlsx(resultado$estadual,
           file.path(pasta_saida, "banco_partidos_estadual.xlsx"), overwrite = TRUE)
fwrite(resultado$municipal,
       file.path(pasta_saida, "banco_partidos_municipal.csv"), sep = ";")

cat("Bancos salvos em:", pasta_saida, "\n")
