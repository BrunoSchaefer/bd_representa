### 12 Junção Candidaturas (Etapa 2b) -------------------------------------------
# Bruno Schaefer (IESP-UERJ)
#
# Objetivo: um banco por cargo, com uma linha por candidatura, juntando tudo o
# que se sabe sobre a candidatura, o partido e o distrito:
#   - características dos candidatos (código 7), resultados e incumbência
#     (código 9), bens (código 8) e financiamento eleitoral (código 6);
#   - participação do candidato nos votos do cargo-distrito e nos votos do
#     partido no cargo-distrito;
#   - características nacionais do partido (código 5: ideologia, democracia
#     interna, PSI);
#   - direção do partido no distrito (código 5_1: diretório estadual, municipal
#     ou, para presidente, nacional, com financiamento partidário);
#   - vagas, comparecimento e perfil do eleitorado do distrito (código 10);
#   - informações socioeconômicas do distrito no ano da eleição (código 11).
#
# Entradas: 1 Pesquisas individuais/4a Produtos (Candidatos, Partidos,
#           Distritos, Socioeconômico) e 2 Agregação dos dados/1b Partidos/bancos
# Saída:    2 Agregação dos dados/2b Eleições/bancos, candidaturas_<cargo>
#           (.xlsx; vereador em .csv, acima do limite de linhas do Excel)
#
# Decisões:
#   - Somente eleições ordinárias; votos e resultado do 1º turno. Para
#     presidente, governador e prefeito, disputou_2turno, votos_2turno e
#     eleito_final informam o desfecho após o 2º turno.
#   - A base é o banco de candidaturas deferidas do código 7 (junção à
#     esquerda). Vices e suplentes não recebem votos e ficam de fora.
#   - Chaves: ano, cargo, UF (e município nas eleições municipais) e número do
#     candidato; bens por ano e sequencial. Presidente: UF padronizada "BR".
#   - Participação nos votos: votos do candidato sobre os votos válidos do
#     cargo-distrito (código 10; para presidente, soma dos votos nominais) e
#     sobre a soma dos votos nominais dos candidatos do mesmo partido no
#     cargo-distrito (os votos de legenda não entram, por não estarem
#     disponíveis por partido).
#   - Partido: sigla da época convertida para a sigla atual (sigla_atual()).
#     Os diretórios existem a partir de 2015: o diretório do partido no
#     distrito só é informado a partir das eleições de 2016 (municipais) e
#     2018 (gerais).

library(here)
library(tidyverse)
library(openxlsx)
library(data.table)

source(here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))

pasta_saida <- pasta$juncao_eleicoes
dir.create(pasta_saida, showWarnings = FALSE, recursive = TRUE)

cargos_gerais     <- c("presidente", "governador", "senador", "deputado federal",
                       "deputado estadual", "deputado distrital")
cargos_municipais <- c("prefeito", "vereador")

# Eleições ordinárias (Macapá 2020 foi adiada e tem rótulo próprio)
tipos_ordinarios  <- c("eleicao ordinaria", "eleicoes municipais 2020 - ap")


### Funções -------------------------------------------------------------------

ler <- function(pasta, arquivo) {
  caminho <- file.path(pasta, arquivo)
  if (!file.exists(caminho)) {
    message("Arquivo não encontrado (etapa ignorada): ", caminho)
    return(NULL)
  }
  ler_banco(caminho) |> as_tibble()
}

# UF "BR" para os cargos nacionais e códigos como texto
padronizar <- function(df) {
  if (is.null(df)) return(NULL)
  df |>
    mutate(
      ano      = as.integer(ano),
      sigla_uf = if ("cargo" %in% names(df))
                   if_else(cargo %in% c("presidente", "vice-presidente"), "BR", sigla_uf)
                 else sigla_uf,
      across(any_of(c("id_municipio", "id_municipio_tse", "numero", "numero_candidato",
                      "sequencial", "sequencial_candidato")), as.character),
      # read.xlsx lê datas como número de série do Excel
      across(any_of("data_nascimento"), ~ if (is.numeric(.x)) convertToDate(.x) else as.Date(.x))
    )
}

# Relata e remove chaves repetidas (mantém a primeira ocorrência)
unicos <- function(df, chaves, rotulo) {
  dup <- df |> count(across(all_of(chaves))) |> filter(n > 1)
  if (nrow(dup) > 0) {
    cat(rotulo, ": ", nrow(dup), " chaves repetidas removidas\n", sep = "")
    print(head(dup))
  }
  distinct(df, across(all_of(chaves)), .keep_all = TRUE)
}


### Bases auxiliares (partido e distrito) --------------------------------------

# Características nacionais do partido (código 5)
partidos_ano <- ler(pasta$prod_partidos, "banco_partidos_ano.xlsx") |>
  rename(ano = ANO) |>
  mutate(ano = as.integer(ano)) |>
  rename_with(~ paste0("partido_", .x), -c(ano, SG_PARTIDO_PAD))

# Direção do partido no distrito (código 5_1): variáveis selecionadas
vars_diretorio <- c("NM_TIPO_ORGAO_PARTIDARIO", "total_membros", "total_mulheres",
                    "proporcao_mulheres", "total_decisao", "mulher_decisao",
                    "proporcao_mulheres_decisao", "tem_mulher_presidente",
                    "tem_mulher_tesoureiro", "turnover_prop", "turnover_prop_decisao",
                    "desp_valor_total_bruto", "rec_valor_total_bruto",
                    "rec_valor_recursos_publicos")

preparar_diretorio <- function(df, chaves) {
  if (is.null(df)) return(NULL)
  df |>
    rename(ano = ANO) |>
    mutate(ano = as.integer(ano)) |>
    select(all_of(chaves), any_of(vars_diretorio)) |>
    rename_with(~ paste0("diretorio_", .x), -all_of(chaves))
}

dir_nacional  <- preparar_diretorio(ler(pasta$juncao_partidos, "banco_partidos_nacional_orgao_direcao.xlsx"),
                                    c("ano", "SG_PARTIDO_PAD"))
dir_estadual  <- preparar_diretorio(ler(pasta$juncao_partidos, "banco_partidos_estadual.xlsx"),
                                    c("ano", "SG_PARTIDO_PAD", "SG_UF"))
dir_municipal <- preparar_diretorio(
  ler(pasta$juncao_partidos, "banco_partidos_municipal.csv") |>
    mutate(CD_MUNICIPIO = as.integer(CD_MUNICIPIO)),
  c("ano", "SG_PARTIDO_PAD", "SG_UF", "CD_MUNICIPIO"))

# Vagas, comparecimento e eleitorado do distrito (código 10)
vars_distrito <- c("aptos", "comparecimento", "votos_validos", "vagas",
                   "eleitores_por_vaga", "pct_comparecimento", "pct_abstencao",
                   "pct_validos", "pct_brancos", "pct_nulos", "total_eleitores",
                   "prop_mulheres", "prop_analfabeto", "prop_sup_completo")

distrito_uf <- ler(pasta$prod_distritos, "base_uf.xlsx") |>
  mutate(ano = as.integer(ano)) |>
  select(ano, sigla_uf, cargo, any_of(vars_distrito)) |>
  rename_with(~ paste0("distrito_", .x), -c(ano, sigla_uf, cargo))

distrito_mun <- readRDS(file.path(pasta$prod_distritos, "base_mun.rds")) |>
  mutate(ano = as.integer(ano), id_municipio = as.character(id_municipio)) |>
  select(ano, id_municipio, cargo, any_of(vars_distrito)) |>
  rename_with(~ paste0("distrito_", .x), -c(ano, id_municipio, cargo))

# Socioeconômico (código 11)
socio_uf  <- ler(pasta$prod_socioeconomico, "socioeconomico_uf.csv")
socio_mun <- ler(pasta$prod_socioeconomico, "socioeconomico_municipio.csv")


### Montagem ------------------------------------------------------------------

montar <- function(candidaturas, resultados, bens, financiamento, cargos, nivel) {

  chave_local <- if (nivel == "municipais") "id_municipio" else NULL
  chaves      <- c("ano", "cargo", "sigla_uf", chave_local, "numero_candidato")
  distrito    <- c("ano", "cargo", "sigla_uf", chave_local)

  # 1. Candidaturas: eleição ordinária e cargos com votação
  cand <- candidaturas |>
    padronizar() |>
    filter(tipo_eleicao %in% tipos_ordinarios, cargo %in% cargos) |>
    rename(numero_candidato = numero) |>
    unicos(chaves, "Candidaturas")

  # 2. Resultados: eleição ordinária
  res <- resultados |>
    padronizar() |>
    filter(tipo_eleicao %in% tipos_ordinarios, cargo %in% cargos)

  res_t1 <- res |>
    filter(turno == 1) |>
    select(all_of(chaves), sequencial_candidato, sigla_partido_res = sigla_partido,
           votos_1turno = votos, resultado_1turno = resultado, eleito_1turno = eleito,
           any_of("incumbente")) |>
    unicos(chaves, "Resultados 1º turno")

  # No 2º turno, quando o resultado não vem preenchido (ex.: presidente 2010),
  # o eleito é o mais votado da disputa
  res_t2 <- res |>
    filter(turno == 2) |>
    group_by(across(all_of(distrito))) |>
    mutate(mais_votado = votos == max(votos)) |>
    ungroup() |>
    mutate(eleito = if_else(is.na(resultado),
                            if_else(mais_votado, "eleito", "nao eleito"),
                            eleito)) |>
    select(all_of(chaves), votos_2turno = votos, eleito_2turno = eleito) |>
    unicos(chaves, "Resultados 2º turno")

  # Denominadores dos votos: todos os candidatos do cargo-distrito no 1º turno
  tot_distrito <- res_t1 |>
    group_by(across(all_of(distrito))) |>
    summarise(votos_nominais_distrito = sum(votos_1turno, na.rm = TRUE), .groups = "drop")
  tot_partido <- res_t1 |>
    group_by(across(all_of(distrito)), sigla_partido_res) |>
    summarise(votos_partido_distrito = sum(votos_1turno, na.rm = TRUE), .groups = "drop")

  # 3. Junção candidaturas + resultados
  base <- cand |>
    left_join(res_t1, by = chaves, relationship = "one-to-one") |>
    left_join(res_t2, by = chaves, relationship = "one-to-one") |>
    mutate(
      disputou_2turno = as.integer(!is.na(votos_2turno)),
      eleito_final    = if_else(disputou_2turno == 1, eleito_2turno, eleito_1turno)
    )

  # Sequencial: do banco de candidaturas quando existir; senão, dos resultados
  if ("sequencial" %in% names(base)) {
    base <- base |> mutate(sequencial_candidato = coalesce(sequencial, sequencial_candidato)) |>
      select(-sequencial)
  }

  # 4. Bens (chave: ano e sequencial, que é único no ano)
  if (!is.null(bens)) {
    b <- bens |>
      padronizar() |>
      select(-any_of("sigla_uf")) |>
      unicos(c("ano", "sequencial_candidato"), "Bens")
    base <- base |>
      left_join(b, by = c("ano", "sequencial_candidato"), relationship = "many-to-one") |>
      mutate(tem_declaracao_bens = as.integer(!is.na(n_bens)))
  }

  # 5. Financiamento eleitoral
  fin <- financiamento |>
    padronizar() |>
    filter(cargo %in% cargos) |>
    select(-any_of(c("numero_partido", "id_municipio_tse", "sequencial_candidato"))) |>
    unicos(chaves, "Financiamento")

  base <- base |>
    left_join(fin, by = chaves, relationship = "one-to-one") |>
    mutate(tem_financiamento = as.integer(!is.na(soma_receitas) | !is.na(despesas_total_can)))

  # 6. Distrito: vagas, comparecimento e eleitorado (mesmo ano e cargo)
  if (nivel == "gerais") {
    base <- base |> left_join(distrito_uf, by = c("ano", "sigla_uf", "cargo"), relationship = "many-to-one")
  } else {
    base <- base |> left_join(distrito_mun, by = c("ano", "id_municipio", "cargo"), relationship = "many-to-one")
  }

  # 7. Participação nos votos
  base <- base |>
    left_join(tot_distrito, by = distrito, relationship = "many-to-one") |>
    left_join(tot_partido, by = c(distrito, "sigla_partido_res"), relationship = "many-to-one") |>
    mutate(
      pct_votos_distrito = 100 * votos_1turno / coalesce(distrito_votos_validos, votos_nominais_distrito),
      pct_votos_partido  = 100 * votos_1turno / votos_partido_distrito
    ) |>
    select(-sigla_partido_res)

  # 8. Partido: características nacionais e direção no distrito
  base <- base |>
    mutate(SG_PARTIDO_PAD = sigla_atual(sigla_partido)) |>
    left_join(partidos_ano, by = c("ano", "SG_PARTIDO_PAD"), relationship = "many-to-one")

  if (nivel == "gerais") {
    base <- bind_rows(
      base |> filter(cargo != "presidente") |>
        left_join(dir_estadual, by = c("ano", "SG_PARTIDO_PAD", "sigla_uf" = "SG_UF"),
                  relationship = "many-to-one"),
      base |> filter(cargo == "presidente") |>
        left_join(dir_nacional, by = c("ano", "SG_PARTIDO_PAD"), relationship = "many-to-one")
    )
  } else {
    base <- base |>
      mutate(CD_MUNICIPIO = as.integer(id_municipio_tse)) |>
      left_join(dir_municipal, by = c("ano", "SG_PARTIDO_PAD", "sigla_uf" = "SG_UF", "CD_MUNICIPIO"),
                relationship = "many-to-one") |>
      select(-CD_MUNICIPIO)
  }

  # 9. Socioeconômico do distrito (quando existir)
  if (nivel == "gerais" && !is.null(socio_uf)) {
    base <- base |> left_join(socio_uf |> mutate(ano = as.integer(ano)), by = c("ano", "sigla_uf"))
  }
  if (nivel == "municipais" && !is.null(socio_mun)) {
    base <- base |> left_join(socio_mun |> mutate(ano = as.integer(ano), id_municipio = as.character(id_municipio)),
                              by = c("ano", "id_municipio"))
  }

  stopifnot(nrow(base) == nrow(cand))
  base
}

diagnostico <- function(base, rotulo) {
  cat("\n===", rotulo, "===\n")
  base |>
    group_by(ano, cargo) |>
    summarise(
      candidaturas   = n(),
      com_votos      = mean(!is.na(votos_1turno)),
      com_bens       = mean(tem_declaracao_bens),
      com_financ     = mean(tem_financiamento),
      com_ideologia  = mean(!is.na(partido_bls_ideologia) | !is.na(partido_bolognesi_ideologia)),
      com_diretorio  = mean(!is.na(diretorio_total_membros)),
      com_distrito   = mean(!is.na(distrito_aptos)),
      eleitos        = sum(eleito_final == "eleito", na.rm = TRUE),
      incumbentes    = sum(incumbente, na.rm = TRUE),
      soma_pct_votos = sum(pct_votos_distrito, na.rm = TRUE) / n_distinct(sigla_uf),
      .groups = "drop"
    ) |>
    mutate(across(starts_with("com_"), ~ round(100 * .x, 1))) |>
    print(n = Inf, width = Inf)
}

exportar <- function(base, cargos) {
  for (cg in cargos) {
    df   <- base |> filter(cargo == cg)
    nome <- paste0("candidaturas_", str_replace_all(cg, " ", "_"))
    if (nrow(df) > 1e6) {
      fwrite(df, file.path(pasta_saida, paste0(nome, ".csv")), sep = ";")
    } else {
      write.xlsx(df, file.path(pasta_saida, paste0(nome, ".xlsx")), overwrite = TRUE)
    }
    cat("Salvo:", nome, "-", nrow(df), "linhas\n")
  }
}


### Eleições gerais -----------------------------------------------------------

gerais <- montar(
  candidaturas  = ler(pasta$prod_candidatos, "candidaturas_gerais.xlsx"),
  resultados    = ler(pasta$prod_candidatos, "resultados_gerais.xlsx"),
  bens          = ler(pasta$prod_candidatos, "bens_candidatos_gerais.xlsx"),
  financiamento = ler(pasta$prod_candidatos, "financiamento_gerais.xlsx"),
  cargos        = cargos_gerais,
  nivel         = "gerais"
)

diagnostico(gerais, "Eleições gerais")
exportar(gerais, cargos_gerais)
rm(gerais); gc()


### Eleições municipais -------------------------------------------------------

municipais <- montar(
  candidaturas  = ler(pasta$prod_candidatos, "candidaturas_municipais.csv"),
  resultados    = ler(pasta$prod_candidatos, "resultados_municipais.csv"),
  bens          = ler(pasta$prod_candidatos, "bens_candidatos_municipais.csv"),
  financiamento = ler(pasta$prod_candidatos, "financiamento_municipais.csv"),
  cargos        = cargos_municipais,
  nivel         = "municipais"
)

diagnostico(municipais, "Eleições municipais")
exportar(municipais, cargos_municipais)
