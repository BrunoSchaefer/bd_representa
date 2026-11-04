### 15 Produtos finais — Candidatos (Etapa 3, 2c) ------------------------------
# Bruno Schaefer (IESP-UERJ)
#
# Objetivo: os bancos finais de candidatos, um por cargo (casa legislativa ou
# executivo), com tudo o que a Etapa 2b (código 12) reuniu sobre a
# candidatura, o partido e o distrito.
#
#   bd_candidatos_governadoria  governador
#   bd_candidatos_senado        senador
#   bd_candidatos_camara        deputado federal
#   bd_candidatos_assembleia    deputado estadual e deputado distrital (DF)
#   bd_candidatos_prefeitura    prefeito
#   bd_candidatos_vereanca      vereador (.csv, acima do limite do Excel)
#
# Unidade: ano-candidato-cargo-partido-distrito. Eleições ordinárias; votos e
# resultado do 1º turno (e desfecho final para os cargos com 2º turno).
#
# Entrada: 2 Agregação dos dados/2b Eleições/bancos
# Saída:   3 Produtos finais/bancos
#
# Variáveis acrescentadas aqui: mulher (gênero feminino) e negro (cor/raça
# preta ou parda; NA quando a cor/raça não foi informada — antes de 2014 o TSE
# não registrava cor/raça).
#
# O banco de presidente fica na Etapa 2 (candidaturas_presidente.xlsx).

library(here)
library(tidyverse)
library(openxlsx)
library(data.table)

source(here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))

saida <- pasta$produtos_finais
dir.create(saida, showWarnings = FALSE, recursive = TRUE)

produtos <- list(
  bd_candidatos_governadoria = "candidaturas_governador.xlsx",
  bd_candidatos_senado       = "candidaturas_senador.xlsx",
  bd_candidatos_camara       = "candidaturas_deputado_federal.xlsx",
  bd_candidatos_assembleia   = c("candidaturas_deputado_estadual.xlsx", "candidaturas_deputado_distrital.xlsx"),
  bd_candidatos_prefeitura   = "candidaturas_prefeito.xlsx",
  bd_candidatos_vereanca     = "candidaturas_vereador.csv"
)

for (nome in names(produtos)) {
  df <- rbindlist(lapply(produtos[[nome]], \(a) ler_banco(file.path(pasta$juncao_eleicoes, a))),
                  use.names = TRUE, fill = TRUE)
  df[, `:=`(
    mulher = as.integer(genero == "feminino"),
    negro  = fifelse(is.na(raca), NA_integer_, as.integer(raca %in% c("preta", "parda")))
  )]
  setcolorder(df, c("ano", "cargo", "sigla_uf", intersect("id_municipio", names(df)),
                    "sigla_partido", "numero_candidato", "nome", "mulher", "negro"))

  if (nrow(df) > 1e6) {
    fwrite(df, file.path(saida, paste0(nome, ".csv")), sep = ";")
  } else {
    write.xlsx(df, file.path(saida, paste0(nome, ".xlsx")), overwrite = TRUE)
  }
  cat(nome, ":", nrow(df), "candidaturas |",
      paste(names(table(df$ano)), table(df$ano), sep = "=", collapse = " "), "\n")
}
