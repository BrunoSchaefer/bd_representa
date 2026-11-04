### 16 Produtos finais — Distritos (Etapa 3, 3c) -------------------------------
# Bruno Schaefer (IESP-UERJ)
#
# Objetivo: bancos por distrito e eleição com índices de equidade de gênero e
# racial nos cargos proporcionais, informações sobre os partidos no distrito
# e sobre as eleições majoritárias.
#
#   bd_distritos_estadual   ano-UF (eleições gerais 2010–2022), cargos
#                           proporcionais: câmara (deputado federal) e
#                           assembleia (deputado estadual ou distrital)
#   bd_distritos_municipal  ano-município (eleições municipais 2012–2024),
#                           cargo proporcional: vereança
#
# Entradas: 2 Agregação dos dados (2b Eleições/bancos e 1b Partidos/bancos) e
#           1 Pesquisas individuais/4a Produtos/Socioeconômico (código 11)
# Saída:    3 Produtos finais/bancos
#
# Índices de equidade (por cargo proporcional)
#   Numeradores — participação do grupo (mulheres; pessoas negras = pretas e
#   pardas) em:
#     candidatos                         prop_candidatos
#     receitas de campanha               prop_receitas
#     receitas vindas de partidos        prop_receitas_partidarias
#     receitas vindas de direções        prop_receitas_direcao
#     eleitos                            prop_eleitos
#   Parâmetro de referência p (colunas denominador_*):
#     gênero — proporção de mulheres no eleitorado (perfil do eleitorado, 10)
#     raça   — proporção de pessoas negras na população (código 11; Censo
#              2010 para 2010–2015 e Censo 2022 para 2016–2025)
#   Índice = DRI (Descriptive Representation Index), modificação de Ransom e
#   Sutch (2001); função dri() em 0_funcoes_auxiliares.R:
#     DRI(b, p) = (b - p) / sqrt((b - p)^2 + b (1 - b))
#   em que b é a participação do grupo (numeradores acima). Varia de -1 a 1:
#   0 = paridade (b = p); negativo = sub-representação (-1 = grupo ausente);
#   positivo = sobrerrepresentação.
#   As demais variáveis socioeconômicas do distrito (código 11) também são
#   acrescentadas aos bancos.
#   A cor/raça dos candidatos só existe a partir de 2014: em 2010 e 2012 as
#   medidas raciais ficam NA.
#
# Partidos no distrito (ano da eleição; diretórios existem a partir de 2015,
# portanto só a partir das eleições de 2016 e 2018): número de partidos com
# direção, total de dirigentes, de cargos de decisão, de mulheres em cargos de
# decisão e proporção de mulheres em cargos de decisão.
#
# Eleições majoritárias: tem mulher eleita e tem pessoa negra eleita para o
# governo do estado (estadual) ou para a prefeitura (municipal).

library(here)
library(tidyverse)
library(openxlsx)
library(data.table)

source(here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))

saida <- pasta$produtos_finais
dir.create(saida, showWarnings = FALSE, recursive = TRUE)


### Funções -------------------------------------------------------------------

ler_cands <- function(arquivos) {
  rbindlist(lapply(arquivos, \(a) ler_banco(file.path(pasta$juncao_eleicoes, a))), use.names = TRUE, fill = TRUE)
}

# Proporções por grupo e cargo proporcional em cada distrito
proporcoes <- function(cand, chave, sufixo) {
  cand[, `:=`(
    mulher = as.integer(genero == "feminino"),
    negro  = fifelse(is.na(raca), NA_integer_, as.integer(raca %in% c("preta", "parda"))),
    eleito = as.integer(eleito_final %in% "eleito"),
    rec    = fcoalesce(as.numeric(soma_receitas), 0),
    rec_p  = fcoalesce(as.numeric(soma_recursos_partidos), 0),
    rec_d  = fcoalesce(as.numeric(soma_recursos_direcao_partidaria), 0)
  )]
  prop <- function(v, g) if (all(is.na(g))) NA_real_ else {
    ok <- !is.na(g); tot <- sum(v[ok]); if (tot > 0) sum(v[ok & g == 1]) / tot else NA_real_
  }
  x <- cand[, .(
    n_candidatos                       = .N,
    n_eleitos                          = sum(eleito),
    prop_candidatos_mulheres           = mean(mulher, na.rm = TRUE),
    prop_receitas_mulheres             = prop(rec, mulher),
    prop_receitas_partidarias_mulheres = prop(rec_p, mulher),
    prop_receitas_direcao_mulheres     = prop(rec_d, mulher),
    prop_eleitos_mulheres              = prop(eleito, mulher),
    prop_candidatos_negros             = if (all(is.na(negro))) NA_real_ else mean(negro, na.rm = TRUE),
    prop_receitas_negros               = prop(rec, negro),
    prop_receitas_partidarias_negros   = prop(rec_p, negro),
    prop_receitas_direcao_negros       = prop(rec_d, negro),
    prop_eleitos_negros                = prop(eleito, negro)
  ), by = chave]
  setnames(x, setdiff(names(x), chave), paste0(setdiff(names(x), chave), "_", sufixo))
  x
}

# Índices de equidade: DRI(participação do grupo, referência)
indices <- function(x, sufixo) {
  for (v in c("candidatos", "receitas", "receitas_partidarias", "receitas_direcao", "eleitos")) {
    x[, paste0("indice_genero_", v, "_", sufixo) :=
        dri(get(paste0("prop_", v, "_mulheres_", sufixo)), denominador_mulheres)]
    x[, paste0("indice_racial_", v, "_", sufixo) :=
        dri(get(paste0("prop_", v, "_negros_", sufixo)), denominador_negros)]
  }
  x
}

# Eleições majoritárias: mulher / pessoa negra eleita
majoritario <- function(cand, chave, sufixo) {
  x <- cand[eleito_final %in% "eleito", .(
    tem_mulher = as.integer(any(genero == "feminino")),
    tem_negro  = if (all(is.na(raca))) NA_integer_ else as.integer(any(raca %in% c("preta", "parda")))
  ), by = chave]
  setnames(x, c("tem_mulher", "tem_negro"), paste0(c("tem_mulher_eleita_", "tem_negro_eleito_"), sufixo))
  x
}

# Partidos no distrito (diretórios no ano da eleição)
partidos_distrito <- function(dir, chave) {
  dir[, .(
    partidos_n_diretorios          = .N,
    partidos_total_dirigentes      = sum(total_membros, na.rm = TRUE),
    partidos_total_decisao         = sum(total_decisao, na.rm = TRUE),
    partidos_total_mulheres        = sum(total_mulheres, na.rm = TRUE),
    partidos_mulheres_decisao      = sum(mulher_decisao, na.rm = TRUE)
  ), by = chave][, partidos_prop_mulheres_decisao := fifelse(partidos_total_decisao > 0,
                                                            partidos_mulheres_decisao / partidos_total_decisao, NA_real_)]
}

socio_uf  <- file.path(pasta$prod_socioeconomico, "socioeconomico_uf.csv")
socio_mun <- file.path(pasta$prod_socioeconomico, "socioeconomico_municipio.csv")


### Estadual ---------------------------------------------------------------------

dist_uf <- as.data.table(read.xlsx(file.path(pasta$juncao_partidos, "distritos_uf_eleicao.xlsx")))
dist_uf[, ano := as.integer(ano)]

camara     <- ler_cands("candidaturas_deputado_federal.xlsx")
assembleia <- ler_cands(c("candidaturas_deputado_estadual.xlsx", "candidaturas_deputado_distrital.xlsx"))
governo    <- ler_cands("candidaturas_governador.xlsx")
camara[, ano := as.integer(ano)]; assembleia[, ano := as.integer(ano)]; governo[, ano := as.integer(ano)]

bd_uf <- dist_uf |>
  merge(proporcoes(camara, c("ano", "sigla_uf"), "camara"), by = c("ano", "sigla_uf"), all.x = TRUE) |>
  merge(proporcoes(assembleia, c("ano", "sigla_uf"), "assembleia"), by = c("ano", "sigla_uf"), all.x = TRUE) |>
  merge(majoritario(governo, c("ano", "sigla_uf"), "governo"), by = c("ano", "sigla_uf"), all.x = TRUE)

dir_est <- as.data.table(read.xlsx(file.path(pasta$juncao_partidos, "diretorios_estadual.xlsx")))
dir_est[, ano := as.integer(ANO)]
bd_uf <- merge(bd_uf, partidos_distrito(dir_est, c("ano", "SG_UF")),
               by.x = c("ano", "sigla_uf"), by.y = c("ano", "SG_UF"), all.x = TRUE)

# Denominadores
bd_uf[, denominador_mulheres := distrito_prop_mulheres]
if (file.exists(socio_uf)) {
  s <- ler_banco(socio_uf)[, `:=`(ano = as.integer(ano), denominador_negros = prop_negros_populacao)]
  bd_uf <- merge(bd_uf, s, by = c("ano", "sigla_uf"), all.x = TRUE)
} else {
  message("Socioeconômico por UF ainda não disponível: índices raciais ficam NA.")
  bd_uf[, denominador_negros := NA_real_]
}
bd_uf <- indices(bd_uf, "camara")
bd_uf <- indices(bd_uf, "assembleia")
setorder(bd_uf, ano, sigla_uf)

cat("\n=== bd_distritos_estadual ===\n")
print(bd_uf[, .(ufs = .N,
                indice_genero_candidatos_camara = round(mean(indice_genero_candidatos_camara, na.rm = TRUE), 3),
                indice_genero_eleitos_camara    = round(mean(indice_genero_eleitos_camara, na.rm = TRUE), 3),
                prop_candidatos_negros_camara   = round(mean(prop_candidatos_negros_camara, na.rm = TRUE), 3),
                tem_mulher_governo              = sum(tem_mulher_eleita_governo, na.rm = TRUE),
                com_partidos                    = sum(!is.na(partidos_n_diretorios))), by = ano])
write.xlsx(bd_uf, file.path(saida, "bd_distritos_estadual.xlsx"), overwrite = TRUE)


### Municipal --------------------------------------------------------------------

dist_mun <- ler_banco(file.path(pasta$juncao_partidos, "distritos_municipio_eleicao.csv"))
dist_mun[, `:=`(ano = as.integer(ano), CD_MUNICIPIO = as.integer(CD_MUNICIPIO))]

vereanca   <- ler_cands("candidaturas_vereador.csv")
prefeitura <- ler_cands("candidaturas_prefeito.xlsx")
vereanca[, `:=`(ano = as.integer(ano), id_municipio = as.character(id_municipio))]
prefeitura[, `:=`(ano = as.integer(ano), id_municipio = as.character(id_municipio))]

bd_mun <- dist_mun |>
  merge(proporcoes(vereanca, c("ano", "id_municipio"), "vereanca"), by = c("ano", "id_municipio"), all.x = TRUE) |>
  merge(majoritario(prefeitura, c("ano", "id_municipio"), "prefeitura"), by = c("ano", "id_municipio"), all.x = TRUE)

dir_mun <- ler_banco(file.path(pasta$juncao_partidos, "diretorios_municipal.csv"))
dir_mun[, `:=`(ano = as.integer(ANO), CD_MUNICIPIO = as.integer(CD_MUNICIPIO))]
bd_mun <- merge(bd_mun, partidos_distrito(dir_mun, c("ano", "CD_MUNICIPIO")),
                by = c("ano", "CD_MUNICIPIO"), all.x = TRUE)

bd_mun[, denominador_mulheres := distrito_prop_mulheres]
if (file.exists(socio_mun)) {
  s <- ler_banco(socio_mun)[, `:=`(ano = as.integer(ano), id_municipio = as.character(id_municipio),
                                  denominador_negros = prop_negros_populacao)]
  bd_mun <- merge(bd_mun, s, by = c("ano", "id_municipio"), all.x = TRUE)
} else {
  message("Socioeconômico por município ainda não disponível: índices raciais ficam NA.")
  bd_mun[, denominador_negros := NA_real_]
}
bd_mun <- indices(bd_mun, "vereanca")
setorder(bd_mun, ano, id_municipio)

cat("\n=== bd_distritos_municipal ===\n")
print(bd_mun[, .(municipios = .N,
                 indice_genero_candidatos_vereanca = round(mean(indice_genero_candidatos_vereanca, na.rm = TRUE), 3),
                 indice_genero_eleitos_vereanca    = round(mean(indice_genero_eleitos_vereanca, na.rm = TRUE), 3),
                 prop_candidatos_negros_vereanca   = round(mean(prop_candidatos_negros_vereanca, na.rm = TRUE), 3),
                 tem_mulher_prefeitura             = sum(tem_mulher_eleita_prefeitura, na.rm = TRUE),
                 com_partidos                      = sum(!is.na(partidos_n_diretorios))), by = ano])
write.xlsx(bd_mun, file.path(saida, "bd_distritos_municipal.xlsx"), overwrite = TRUE)

cat("\nBancos salvos em:", saida, "\n")
