### 17 Produtos finais — Listas partidárias (Etapa 3, 2c) ------------------------
# Bruno Schaefer (IESP-UERJ)
#
# Objetivo: bancos por partido-distrito-eleição nos cargos proporcionais, que
# agregam a lista de candidaturas de cada partido e calculam os índices de
# equidade (DRI) da lista:
#   bd_partidos_camara       ano-partido-UF, deputado federal (2010–2026)
#   bd_partidos_assembleias  ano-partido-UF, deputado estadual e distrital (2010–2026)
#   bd_partidos_vereanca     ano-partido-município, vereador (2012–2024), .csv
#
# Entradas: 2 Agregação dos dados/2b Eleições/bancos (candidaturas_*, código 12),
#           1 Pesquisas individuais/4a Produtos/Dirigentes e diretórios
#           (banco_dirigentes_*) e 4a Produtos/Socioeconômico (código 11)
# Saída:    3 Produtos finais/bancos
#
# Conteúdo de cada linha (lista do partido no distrito):
#   - composição: número de candidaturas, de mulheres, de pessoas negras
#     (pretas e pardas), de eleitos, de incumbentes; idade média; proporção
#     com ensino superior completo; cota de gênero (30% de candidaturas);
#   - dirigentes candidatos: candidaturas de dirigentes do próprio partido no
#     distrito (diretório estadual para câmara e assembleias; municipal para
#     vereança) e, nas eleições gerais, da direção nacional; cruzamento pelo
#     título eleitoral; NA antes de 2016 (os dados de dirigentes começam em 2015);
#   - votos: votos nominais da lista (1º turno), participação nos votos
#     nominais do distrito, candidaturas por vaga e cadeiras obtidas;
#   - financiamento: somas de receitas por origem, despesas e patrimônio
#     declarado dos candidatos da lista;
#   - participação das mulheres e das pessoas negras (prop_<medida>_<grupo>)
#     nas candidaturas, votos, receitas, recursos públicos (FEFC e Fundo
#     Partidário), recursos de partidos, recursos de direções partidárias e
#     eleitos;
#   - índices de equidade (DRI, função dri() em 0_funcoes_auxiliares.R):
#       DRI(b, p) = (b - p) / sqrt((b - p)^2 + b (1 - b))
#     b = participação do grupo na medida; p = mulheres no eleitorado do
#     distrito (gênero, código 10) ou pessoas negras na população do distrito
#     (raça, código 11). Varia de -1 a 1; 0 = paridade. NA quando a medida é
#     nula na lista (ex.: lista sem eleitos) ou a cor/raça não existe (2010 e
#     2012);
#   - candidaturas majoritárias próprias do partido no mesmo distrito e
#     eleição, com gênero, raça e resultado: governo do estado (governador_*),
#     Senado (senado_*) e Presidência (presidente_*) para câmara e
#     assembleias; prefeitura (prefeitura_*) para vereança. <cargo>_tem_candidato
#     = 1 quando o partido lançou candidatura própria (cabeça de chapa); apoio
#     a candidatura de outro partido (coligação majoritária) não é identificado;
#   - diretório do partido no distrito no ano da eleição (prefixo diretorio_):
#     todas as variáveis de bd_diretorios_estadual (câmara e assembleias) ou
#     bd_diretorios_municipal (vereança), código 14 — composição, turnover,
#     financiamento partidário e cruzamento dirigente-candidato; existem a
#     partir de 2016 (municipal) e 2018 (estadual);
#   - características nacionais do partido (partido_*), distrito (distrito_*)
#     e socioeconômico (código 11).
#
# Decisões:
#   - Unidade: partido (número do partido na eleição). Até 2018 os partidos
#     podiam se coligar nas eleições proporcionais e, a partir de 2022, formar
#     federações: nesses casos a lista efetiva é a da coligação/federação, mas
#     aqui cada partido é uma linha.
#   - Base: candidaturas deferidas em eleições ordinárias (código 12); a cota de
#     gênero é verificada sobre elas (a lei a aplica aos registros).
#   - Votos de legenda não entram nos votos da lista (ver código 0).

library(here)
library(tidyverse)
library(openxlsx)
library(data.table)

source(here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))

saida <- pasta$produtos_finais
dir.create(saida, showWarnings = FALSE, recursive = TRUE)

titulo_norm <- function(x) {
  x <- sub("^0+", "", as.character(x))
  fifelse(grepl("^[0-9]+$", x), x, NA_character_)
}

# Medidas em que se calcula a participação dos grupos e o DRI
medidas <- c("candidatos", "votos", "receitas", "receitas_publicas", "receitas_partidarias",
             "receitas_direcao", "eleitos")


### 1. Leitura -------------------------------------------------------------------

cols_base <- c("ano", "cargo", "sigla_uf", "id_municipio", "id_municipio_tse", "numero_partido",
               "sigla_partido", "SG_PARTIDO_PAD", "titulo_eleitoral", "genero", "raca", "idade",
               "instrucao", "incumbente", "eleito_final", "votos_1turno", "votos_nominais_distrito",
               "tem_financiamento", "patrimonio_total", "despesas_total_can")

# (as colunas diretorio_* das candidaturas são substituídas pelo banco completo
# de diretórios do código 14)
colunas_usadas <- function(cols) {
  c(intersect(cols_base, cols), grep("^soma_(receitas|recursos_)", cols, value = TRUE),
    grep("^(distrito_|partido_)", cols, value = TRUE))
}

ler_cands <- function(arquivos) {
  x <- rbindlist(lapply(arquivos, function(a) {
    caminho <- file.path(pasta$juncao_eleicoes, a)
    if (grepl("\\.csv$", a)) {
      cols <- names(fread(caminho, sep = ";", nrows = 0))
      fread(caminho, sep = ";", select = colunas_usadas(cols),
            colClasses = list(character = intersect(c("id_municipio", "id_municipio_tse", "numero_partido",
                                                      "titulo_eleitoral"), cols)),
            na.strings = c("", "NA"), showProgress = FALSE)
    } else {
      x <- as.data.table(read.xlsx(caminho))
      x[, colunas_usadas(names(x)), with = FALSE]
    }
  }), use.names = TRUE, fill = TRUE)
  x[, `:=`(ano = as.integer(ano), numero_partido = as.character(numero_partido),
           titulo = titulo_norm(titulo_eleitoral))]
  x
}

# Dirigentes por partido-distrito-ano (uma linha por pessoa)
dirigentes <- function(d, chave) {
  d <- as.data.table(d)[, c("ANO", "SG_PARTIDO", chave, "NR_TITULO_ELEITORAL_MEMBRO", "cargo_decisao"), with = FALSE]
  d[, `:=`(ano = as.integer(ANO), SG_PARTIDO_PAD = sigla_atual(SG_PARTIDO),
           titulo = titulo_norm(NR_TITULO_ELEITORAL_MEMBRO),
           decisao = fcoalesce(as.integer(cargo_decisao), 0L))]
  d[!is.na(titulo), .(decisao = max(decisao)), by = c("ano", "SG_PARTIDO_PAD", chave, "titulo")]
}

# Marca as candidaturas de dirigentes; NA nos anos sem dados de dirigentes
marcar <- function(cand, dir, on, var) {
  cand[, (var) := fifelse(ano %in% unique(dir$ano), 0L, NA_integer_)]
  cand[dir, on = on, (var) := 1L]
  if (!is.null(dir$decisao)) {
    vd <- paste0(var, "_decisao")
    cand[, (vd) := fifelse(ano %in% unique(dir$ano), 0L, NA_integer_)]
    cand[dir[decisao == 1L], on = on, (vd) := 1L]
  }
  cand
}

socio_uf  <- ler_banco(file.path(pasta$prod_socioeconomico, "socioeconomico_uf.csv"))[, ano := as.integer(ano)]
socio_mun <- ler_banco(file.path(pasta$prod_socioeconomico, "socioeconomico_municipio.csv"))[, ano := as.integer(ano)]
cols_socio <- setdiff(union(names(socio_uf), names(socio_mun)), c("ano", "sigla_uf", "id_municipio"))

# Candidaturas majoritárias próprias do partido, por partido-distrito-eleição
majoritario <- function(arquivo, chave, prefixo) {
  m <- ler_banco(file.path(pasta$juncao_eleicoes, arquivo))
  m[, `:=`(ano = as.integer(ano),
           negro = fifelse(is.na(raca) | raca == "", NA_integer_, as.integer(raca %in% c("preta", "parda"))))]
  if ("id_municipio" %in% chave) m[, id_municipio := as.character(id_municipio)]
  x <- m[, .(
    tem_candidato   = 1L,
    n_candidatos    = .N,
    nome_urna       = paste(nome_urna, collapse = "; "),
    genero          = paste(fcoalesce(genero, "nao informado"), collapse = "; "),
    raca            = paste(fcoalesce(fifelse(raca == "", NA_character_, raca), "nao informada"), collapse = "; "),
    n_mulheres      = sum(genero %in% "feminino"),
    n_negros        = conta(negro %in% 1L, negro),
    n_eleitos       = sum(eleito_final %in% "eleito"),
    eleita_mulher   = as.integer(any(eleito_final %in% "eleito" & genero %in% "feminino")),
    eleito_negro    = conta(any(eleito_final %in% "eleito" & negro %in% 1L), negro),
    incumbente      = conta(any(incumbente %in% 1), incumbente),
    disputou_2turno = as.integer(any(disputou_2turno %in% 1)),
    pct_votos       = sum(as.numeric(pct_votos_distrito), na.rm = TRUE)
  ), by = chave]
  setnames(x, setdiff(names(x), chave), paste0(prefixo, "_", setdiff(names(x), chave)))
  x
}

juntar_majoritario <- function(x, m, chave) {
  x <- merge(x, m, by = chave, all.x = TRUE, sort = FALSE)
  v <- grep("_tem_candidato$", names(m), value = TRUE)
  x[, (v) := lapply(.SD, \(z) fcoalesce(z, 0L)), .SDcols = v]
  x
}

# Diretório do partido no distrito (banco do código 14), prefixo diretorio_
juntar_diretorio <- function(x, d, chave_x, chave_d) {
  fora <- c("SG_PARTIDO", "SG_UF", "NR_PARTIDO", "CD_MUNICIPIO", "id_municipio", "NM_MUNICIPIO", "SG_UE", "NM_UE",
            grep("^(distrito_|bls_|bolognesi_|salgado_|ribeiro_)", names(d), value = TRUE), cols_socio)
  d <- d[, c(chave_d, setdiff(names(d), c(chave_d, fora))), with = FALSE]
  v <- setdiff(names(d), chave_d)
  setnames(d, v, paste0("diretorio_", v))
  merge(x, d, by.x = chave_x, by.y = chave_d, all.x = TRUE, sort = FALSE)
}

# Socioeconômico no fim do banco
finalizar <- function(x, chave) {
  s <- intersect(cols_socio, names(x))
  setcolorder(x, c(setdiff(names(x), s), s))
  setorderv(x, chave)
  x
}


### 2. Agregação por lista ----------------------------------------------------------

# Participação do grupo (g = 1) no total da medida v, entre quem tem o grupo informado
prop_grupo <- function(v, g) {
  ok <- !is.na(g)
  if (!any(ok)) return(NA_real_)
  tot <- sum(v[ok])
  if (tot > 0) sum(v[ok & g == 1L]) / tot else NA_real_
}

conta <- function(cond, g) if (all(is.na(g))) NA_integer_ else sum(cond, na.rm = TRUE)

agregar_listas <- function(cand, chave, chave_distrito, socio, chave_socio) {
  cand[, `:=`(
    mulher   = fifelse(genero %in% c("feminino", "masculino"), as.integer(genero == "feminino"), NA_integer_),
    negro    = fifelse(is.na(raca) | raca == "", NA_integer_, as.integer(raca %in% c("preta", "parda"))),
    eleito   = as.integer(eleito_final %in% "eleito"),
    um       = 1,
    votos    = fcoalesce(as.numeric(votos_1turno), 0),
    rec      = fcoalesce(as.numeric(soma_receitas), 0),
    rec_pub  = fcoalesce(as.numeric(soma_recursos_publicos), 0),
    rec_p    = fcoalesce(as.numeric(soma_recursos_partidos), 0),
    rec_d    = fcoalesce(as.numeric(soma_recursos_direcao_partidaria), 0),
    superior = fifelse(is.na(instrucao) | instrucao == "", NA_integer_,
                       as.integer(instrucao == "ensino superior completo")),
    incumbente = as.integer(incumbente)
  )]
  if (!"dirigente_nacional" %in% names(cand)) cand[, dirigente_nacional := NA_integer_]

  x <- cand[, .(
    n_candidatos                      = .N,
    n_mulheres                        = sum(mulher %in% 1L),
    n_negros                          = conta(negro %in% 1L, negro),
    n_raca_informada                  = sum(!is.na(negro)),
    n_eleitos                         = sum(eleito),
    n_eleitas_mulheres                = sum(eleito == 1L & mulher %in% 1L),
    n_eleitos_negros                  = conta(eleito == 1L & negro %in% 1L, negro),
    n_incumbentes                     = conta(incumbente %in% 1L, incumbente),
    n_mulheres_incumbentes            = conta(incumbente %in% 1L & mulher %in% 1L, incumbente),
    n_dirigentes_candidatos           = conta(dirigente %in% 1L, dirigente),
    n_mulheres_dirigentes_candidatas  = conta(dirigente %in% 1L & mulher %in% 1L, dirigente),
    n_dirigentes_decisao_candidatos   = conta(dirigente_decisao %in% 1L, dirigente_decisao),
    n_dirigentes_eleitos              = conta(dirigente %in% 1L & eleito == 1L, dirigente),
    n_dirigentes_nacionais_candidatos = conta(dirigente_nacional %in% 1L, dirigente_nacional),
    idade_media                       = mean(as.numeric(idade), na.rm = TRUE),
    prop_superior_completo            = mean(superior, na.rm = TRUE),
    n_com_financiamento               = sum(tem_financiamento %in% 1),
    votos_nominais                    = sum(votos),
    patrimonio_total                  = sum(as.numeric(patrimonio_total), na.rm = TRUE),
    patrimonio_mediano                = as.numeric(median(as.numeric(patrimonio_total), na.rm = TRUE)),
    despesas_total                    = sum(as.numeric(despesas_total_can), na.rm = TRUE),
    prop_candidatos_mulheres          = prop_grupo(um, mulher),
    prop_votos_mulheres               = prop_grupo(votos, mulher),
    prop_receitas_mulheres            = prop_grupo(rec, mulher),
    prop_receitas_publicas_mulheres   = prop_grupo(rec_pub, mulher),
    prop_receitas_partidarias_mulheres = prop_grupo(rec_p, mulher),
    prop_receitas_direcao_mulheres    = prop_grupo(rec_d, mulher),
    prop_eleitos_mulheres             = prop_grupo(eleito, mulher),
    prop_candidatos_negros            = prop_grupo(um, negro),
    prop_votos_negros                 = prop_grupo(votos, negro),
    prop_receitas_negros              = prop_grupo(rec, negro),
    prop_receitas_publicas_negros     = prop_grupo(rec_pub, negro),
    prop_receitas_partidarias_negros  = prop_grupo(rec_p, negro),
    prop_receitas_direcao_negros      = prop_grupo(rec_d, negro),
    prop_eleitos_negros               = prop_grupo(eleito, negro)
  ), by = chave]
  for (v in c("idade_media", "prop_superior_completo")) set(x, which(is.nan(x[[v]])), v, NA_real_)

  # Financiamento: somas por origem (soma_* dos candidatos)
  cols_soma <- grep("^soma_(receitas|recursos_)", names(cand), value = TRUE)
  f <- cand[, lapply(.SD, \(v) sum(as.numeric(v), na.rm = TRUE)), by = chave, .SDcols = cols_soma]
  setnames(f, cols_soma, sub("^soma_", "", cols_soma))

  # Constantes da lista: partido, diretório e distrito
  cols_const <- c("votos_nominais_distrito", grep("^(distrito_|partido_|diretorio_)", names(cand), value = TRUE))
  k <- cand[, lapply(.SD, first), by = chave, .SDcols = cols_const]

  x <- x[f, on = chave][k, on = chave]
  stopifnot(sum(x$n_candidatos) == nrow(cand))

  # Votos, vagas e cota
  x[, `:=`(
    pct_votos_nominais_distrito = 100 * votos_nominais / votos_nominais_distrito,
    candidatos_por_vaga         = n_candidatos / distrito_vagas,
    pct_cadeiras                = 100 * n_eleitos / distrito_vagas,
    cumpre_cota_genero          = as.integer(prop_candidatos_mulheres >= 0.3)
  )]
  x[, `:=`(n_partidos_distrito     = .N,
           pct_candidatos_distrito = 100 * n_candidatos / sum(n_candidatos)), by = chave_distrito]

  # Socioeconômico e parâmetros de referência (p) do DRI
  x <- merge(x, socio, by = chave_socio, all.x = TRUE, sort = FALSE)
  x[, `:=`(denominador_mulheres = distrito_prop_mulheres, denominador_negros = prop_negros_populacao)]

  # Índices de equidade (DRI)
  for (m in medidas) {
    x[, paste0("indice_genero_", m) := dri(get(paste0("prop_", m, "_mulheres")), denominador_mulheres)]
    x[, paste0("indice_racial_", m) := dri(get(paste0("prop_", m, "_negros")), denominador_negros)]
  }

  # Ordem das colunas: chave, composição, votos, financiamento, proporções,
  # índices, partido, diretório, distrito, socioeconômico
  primeiras <- c(chave, grep("^n_", names(x), value = TRUE), "idade_media", "prop_superior_completo",
                 "cumpre_cota_genero", "votos_nominais", "votos_nominais_distrito", "pct_votos_nominais_distrito",
                 "candidatos_por_vaga", "pct_cadeiras", "pct_candidatos_distrito",
                 sub("^soma_", "", cols_soma), "despesas_total", "patrimonio_total", "patrimonio_mediano",
                 grep("^prop_.*_(mulheres|negros)$", names(x), value = TRUE),
                 "denominador_mulheres", "denominador_negros", grep("^indice_", names(x), value = TRUE),
                 grep("^partido_", names(x), value = TRUE), grep("^diretorio_", names(x), value = TRUE),
                 grep("^distrito_", names(x), value = TRUE))
  setcolorder(x, unique(intersect(primeiras, names(x))))
  setorderv(x, chave)
  x
}

resumo <- function(x, rotulo) {
  cat("\n===", rotulo, ":", nrow(x), "listas ===\n")
  print(x[, .(listas = .N,
              candidatos = sum(n_candidatos),
              pct_cota = round(100 * mean(cumpre_cota_genero, na.rm = TRUE), 1),
              dri_genero_candidatos = round(mean(indice_genero_candidatos, na.rm = TRUE), 3),
              dri_genero_receitas   = round(mean(indice_genero_receitas, na.rm = TRUE), 3),
              dri_genero_eleitos    = round(mean(indice_genero_eleitos, na.rm = TRUE), 3),
              dri_racial_candidatos = round(mean(indice_racial_candidatos, na.rm = TRUE), 3),
              dri_racial_eleitos    = round(mean(indice_racial_eleitos, na.rm = TRUE), 3),
              dirigentes_candidatos = sum(n_dirigentes_candidatos, na.rm = TRUE),
              com_diretorio         = sum(!is.na(diretorio_total_membros))), by = ano])
}


### 3. Câmara e assembleias (eleições gerais) --------------------------------------

d_est <- dirigentes(read.xlsx(file.path(pasta$prod_dirigentes, "banco_dirigentes_estadual.xlsx")), "SG_UF")
d_nac <- dirigentes(read.xlsx(file.path(pasta$prod_dirigentes, "banco_dirigentes_nacional.xlsx")), character(0))

maj_governador <- majoritario("candidaturas_governador.xlsx", c("ano", "sigla_uf", "SG_PARTIDO_PAD"), "governador")
maj_senado     <- majoritario("candidaturas_senador.xlsx", c("ano", "sigla_uf", "SG_PARTIDO_PAD"), "senado")
maj_presidente <- majoritario("candidaturas_presidente.xlsx", c("ano", "SG_PARTIDO_PAD"), "presidente")

dir_est <- as.data.table(read.xlsx(file.path(saida, "bd_diretorios_estadual.xlsx")))
dir_est[, ANO := as.integer(ANO)]

chave_gerais <- c("ano", "cargo", "sigla_uf", "numero_partido", "sigla_partido", "SG_PARTIDO_PAD")

gerais <- list(
  camara      = "candidaturas_deputado_federal.xlsx",
  assembleias = c("candidaturas_deputado_estadual.xlsx", "candidaturas_deputado_distrital.xlsx")
)

for (nome in names(gerais)) {
  cand <- ler_cands(gerais[[nome]])
  cand <- marcar(cand, d_est, c("ano", "SG_PARTIDO_PAD", sigla_uf = "SG_UF", "titulo"), "dirigente")
  cand <- marcar(cand, d_nac[, .(ano, SG_PARTIDO_PAD, titulo)], c("ano", "SG_PARTIDO_PAD", "titulo"), "dirigente_nacional")
  x <- agregar_listas(cand, chave_gerais, c("ano", "cargo", "sigla_uf"), socio_uf, c("ano", "sigla_uf"))
  x <- juntar_majoritario(x, maj_governador, c("ano", "sigla_uf", "SG_PARTIDO_PAD"))
  x <- juntar_majoritario(x, maj_senado, c("ano", "sigla_uf", "SG_PARTIDO_PAD"))
  x <- juntar_majoritario(x, maj_presidente, c("ano", "SG_PARTIDO_PAD"))
  x <- juntar_diretorio(x, dir_est, c("ano", "SG_PARTIDO_PAD", "sigla_uf"), c("ANO", "SG_PARTIDO_PAD", "SG_UF"))
  x <- finalizar(x, chave_gerais)
  stopifnot(nrow(x[, .N, by = .(ano, cargo, sigla_uf, numero_partido)][N > 1]) == 0)
  stopifnot(sum(x$n_candidatos) == nrow(cand))
  resumo(x, paste0("bd_partidos_", nome))
  print(x[, .(listas = .N, com_governador = sum(governador_tem_candidato), com_senado = sum(senado_tem_candidato),
              com_presidente = sum(presidente_tem_candidato)), by = ano])
  write.xlsx(x, file.path(saida, paste0("bd_partidos_", nome, ".xlsx")), overwrite = TRUE)
}
rm(cand, x, d_est, d_nac, dir_est); gc()


### 4. Vereança (eleições municipais) ---------------------------------------------

anos_municipais <- c(2016L, 2020L, 2024L)   # anos com dirigentes (a partir de 2015)
d_mun <- fread(file.path(pasta$prod_dirigentes, "banco_dirigentes_mun2.csv"), sep = ";",
               select = c("ANO", "SG_PARTIDO", "CD_MUNICIPIO", "NR_TITULO_ELEITORAL_MEMBRO", "cargo_decisao"),
               colClasses = list(character = c("NR_TITULO_ELEITORAL_MEMBRO", "CD_MUNICIPIO")),
               na.strings = c("", "NA"))[ANO %in% anos_municipais]
d_mun <- dirigentes(d_mun, "CD_MUNICIPIO")
d_mun[, CD_MUNICIPIO := as.integer(CD_MUNICIPIO)]

cand <- ler_cands("candidaturas_vereador.csv")
cand[, CD_MUNICIPIO := as.integer(id_municipio_tse)]
cand <- marcar(cand, d_mun, c("ano", "SG_PARTIDO_PAD", "CD_MUNICIPIO", "titulo"), "dirigente")
cand[, CD_MUNICIPIO := NULL]
rm(d_mun); gc()

socio_mun[, id_municipio := as.character(id_municipio)]
chave_mun <- c("ano", "cargo", "sigla_uf", "id_municipio", "id_municipio_tse", "numero_partido",
               "sigla_partido", "SG_PARTIDO_PAD")
x <- agregar_listas(cand, chave_mun, c("ano", "id_municipio"), socio_mun, c("ano", "id_municipio"))
n_cand <- nrow(cand)
rm(cand); gc()
x[, n_dirigentes_nacionais_candidatos := NULL]

x <- juntar_majoritario(x, majoritario("candidaturas_prefeito.xlsx", c("ano", "id_municipio", "SG_PARTIDO_PAD"), "prefeitura"),
                        c("ano", "id_municipio", "SG_PARTIDO_PAD"))

# Diretórios municipais: só os anos de eleição municipal
cols_dir <- names(fread(file.path(saida, "bd_diretorios_municipal.csv"), sep = ";", nrows = 0))
dir_mun <- fread(file.path(saida, "bd_diretorios_municipal.csv"), sep = ";", na.strings = c("", "NA"),
                 colClasses = list(character = intersect(c("id_municipio", "SG_UE"), cols_dir)))[ANO %in% anos_municipais]
dir_mun[, `:=`(ANO = as.integer(ANO), CD_MUNICIPIO = as.integer(CD_MUNICIPIO))]
x[, CD_MUNICIPIO := as.integer(id_municipio_tse)]
x <- juntar_diretorio(x, dir_mun, c("ano", "SG_PARTIDO_PAD", "CD_MUNICIPIO"), c("ANO", "SG_PARTIDO_PAD", "CD_MUNICIPIO"))
x[, CD_MUNICIPIO := NULL]
rm(dir_mun); gc()

x <- finalizar(x, chave_mun)
stopifnot(nrow(x[, .N, by = .(ano, id_municipio, numero_partido)][N > 1]) == 0)
stopifnot(sum(x$n_candidatos) == n_cand)
resumo(x, "bd_partidos_vereanca")
print(x[, .(listas = .N, com_prefeitura = sum(prefeitura_tem_candidato),
            eleitos_prefeitura = sum(prefeitura_n_eleitos, na.rm = TRUE)), by = ano])
fwrite(x, file.path(saida, "bd_partidos_vereanca.csv"), sep = ";")

cat("\nBancos salvos em:", saida, "\n")
