### 14 Produtos finais — Partidos (Etapa 3, 1c) ---------------------------------
# Bruno Schaefer (IESP-UERJ)
#
# Objetivo: os bancos finais da dimensão partidos.
#
#   bd_diretorios_nacional, bd_diretorios_estadual, bd_diretorios_municipal
#     Unidade: ano-partido-distrito. Composição da direção (gênero, cargos de
#     decisão, turnover geral, por gênero e dos cargos de decisão),
#     financiamento partidário, características nacionais do partido,
#     informações do distrito e quantos dirigentes são ou foram candidatos.
#     No nacional, a Comissão Executiva entra como colunas com prefixo ce_.
#
#   bd_dirigentes_nacional, bd_dirigentes_estadual,
#   bd_dirigentes_municipal (um arquivo por ano, pelo tamanho)
#     Unidade: ano-dirigente-partido-distrito (uma linha por cargo). Dados do
#     dirigente, trajetória no partido (estava na direção no ano anterior,
#     mesmo cargo, mudou de cargo, passou a cargo de decisão), candidaturas
#     (é/foi candidato, cargo, resultado), composição do diretório,
#     características nacionais do partido e informações do distrito.
#
# Entradas:
#   - 1 Pesquisas individuais/4a Produtos/Dirigentes e diretórios (1_x)
#   - 1 Pesquisas individuais/4a Produtos/Partidos (5)
#   - 2 Agregação dos dados/1b Partidos/bancos (13)
#   - 2 Agregação dos dados/2b Eleições/bancos (12)
# Saída: 3 Produtos finais/bancos
#
# Decisões:
#   - Versão dos dirigentes e diretórios: restritiva (>= 183 dias no ano) no
#     nacional e no estadual; ampla (qualquer dia) no municipal.
#   - Cruzamento dirigente-candidato pelo título eleitoral, exato (zeros à
#     esquerda removidos). Quando o dirigente não tem título, usa-se o nome
#     normalizado, só se o nome for único entre os candidatos da UF
#     (variável cruzamento_candidatura = "titulo" ou "nome").
#   - "Foi candidato" considera as eleições até o ano do registro
#     (2010–2024); "candidato no ano" indica candidatura na eleição daquele ano.
#   - Trajetória: comparação com o ano anterior no mesmo partido e distrito
#     (qualquer tipo de órgão). Fica NA em 2015, primeiro ano do banco.

library(here)
library(tidyverse)
library(openxlsx)
library(data.table)

source(here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))

saida <- pasta$produtos_finais
dir.create(saida, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(saida, "bd_dirigentes_municipal"), showWarnings = FALSE)

titulo_norm <- function(x) {
  x <- sub("^0+", "", as.character(x))
  fifelse(grepl("^[0-9]+$", x), x, NA_character_)
}


### 1. Candidaturas (Etapa 2b) ---------------------------------------------------

cols_cand <- c("ano", "cargo", "sigla_uf", "titulo_eleitoral", "nome", "eleito_final")

ler_cand <- function(arquivo) {
  caminho <- file.path(pasta$juncao_eleicoes, arquivo)
  if (grepl("\\.csv$", arquivo)) {
    fread(caminho, sep = ";", select = cols_cand, colClasses = list(character = "titulo_eleitoral"),
          na.strings = c("", "NA"))
  } else {
    as.data.table(read.xlsx(caminho))[, ..cols_cand]
  }
}

candidaturas <- rbindlist(lapply(
  c("candidaturas_presidente.xlsx", "candidaturas_governador.xlsx", "candidaturas_senador.xlsx",
    "candidaturas_deputado_federal.xlsx", "candidaturas_deputado_estadual.xlsx",
    "candidaturas_deputado_distrital.xlsx", "candidaturas_prefeito.xlsx", "candidaturas_vereador.csv"),
  ler_cand
))
candidaturas[, `:=`(ano = as.integer(ano), titulo = titulo_norm(titulo_eleitoral),
                    nome_chave = normalizar_nome(nome), eleito = eleito_final %in% "eleito")]
cat("Candidaturas:", nrow(candidaturas), "| com título:", round(100 * mean(!is.na(candidaturas$titulo)), 1), "%\n")

cand_titulo <- unique(candidaturas[!is.na(titulo), .(titulo, ano_eleicao = ano, cargo, eleito)])

# Nomes únicos por UF (para dirigentes sem título)
nomes_unicos <- candidaturas[!is.na(titulo) & !is.na(nome_chave),
                             .(n = uniqueN(titulo), titulo = first(titulo)),
                             by = .(sigla_uf, nome_chave)][n == 1, .(sigla_uf, nome_chave, titulo)]

rm(candidaturas); gc()

# Características de candidatura por pessoa-ano:
#   pessoas: data.table com id_membro, ANO, titulo, nome_chave, SG_UF
cruzar_candidaturas <- function(pessoas) {
  p <- unique(pessoas[, .(id_membro, ANO, titulo, nome_chave, SG_UF)])
  p[, cruzamento_candidatura := fifelse(!is.na(titulo), "titulo", NA_character_)]
  # Sem título: nome único na UF
  sem <- p[is.na(titulo)]
  if (nrow(sem) > 0) {
    sem[nomes_unicos, on = .(SG_UF = sigla_uf, nome_chave), `:=`(titulo = i.titulo, cruzamento_candidatura = "nome")]
    p <- rbind(p[!is.na(titulo)], sem)
  }
  lig <- cand_titulo[p[!is.na(titulo), .(id_membro, ANO, titulo, cruzamento_candidatura)],
                     on = "titulo", nomatch = 0, allow.cartesian = TRUE][ano_eleicao <= ANO]
  setorder(lig, id_membro, ANO, ano_eleicao)
  res <- lig[, .(
    foi_candidato               = 1L,
    n_candidaturas              = .N,
    ja_eleito                   = as.integer(any(eleito)),
    candidato_no_ano            = as.integer(any(ano_eleicao == ANO[1])),
    ultimo_ano_candidatura      = ano_eleicao[.N],
    ultimo_cargo_candidatura    = cargo[.N],
    eleito_ultima_candidatura   = as.integer(eleito[.N]),
    cruzamento_candidatura      = cruzamento_candidatura[1]
  ), by = .(id_membro, ANO)]
  res
}

aplicar_candidaturas <- function(d) {
  cand <- cruzar_candidaturas(d[, .(id_membro, ANO,
                                    titulo = titulo_norm(NR_TITULO_ELEITORAL_MEMBRO),
                                    nome_chave = normalizar_nome(NM_MEMBRO), SG_UF)])
  d <- merge(d, cand, by = c("id_membro", "ANO"), all.x = TRUE, sort = FALSE)
  d[is.na(foi_candidato), `:=`(foi_candidato = 0L, n_candidaturas = 0L, ja_eleito = 0L, candidato_no_ano = 0L)]
  d
}


### 2. Trajetória no partido ----------------------------------------------------

aplicar_trajetoria <- function(d, unidade) {
  k <- c(unidade, "ANO", "id_membro")
  pessoa  <- d[, .(decisao = max(cargo_decisao)), by = k]
  ant     <- copy(pessoa)[, ANO := ANO + 1L]
  cargos  <- unique(d[, c(k, "DS_CARGO_MEMBRO"), with = FALSE])
  ant_c   <- copy(cargos)[, ANO := ANO + 1L]

  d[pessoa, on = k, decisao_pessoa := i.decisao]
  d[, presente_ano_anterior := 0L][ant, on = k, `:=`(presente_ano_anterior = 1L, decisao_ano_anterior = i.decisao)]
  d[, mesmo_cargo_ano_anterior := 0L][ant_c, on = c(k, "DS_CARGO_MEMBRO"), mesmo_cargo_ano_anterior := 1L]
  d[, `:=`(
    mudou_cargo      = fifelse(presente_ano_anterior == 1L & mesmo_cargo_ano_anterior == 0L, 1L, 0L),
    passou_a_decisao = fifelse(presente_ano_anterior == 1L & decisao_pessoa == 1L & decisao_ano_anterior %in% 0L, 1L, 0L)
  )]
  # Primeiro ano do banco: sem ano anterior para comparar
  primeiro <- min(d$ANO)
  d[ANO == primeiro, c("presente_ano_anterior", "mesmo_cargo_ano_anterior", "mudou_cargo", "passou_a_decisao") := NA_integer_]
  d[, c("decisao_pessoa", "decisao_ano_anterior") := NULL]
  d
}


### 3. Bases de junção -----------------------------------------------------------

partidos_ano <- as.data.table(read.xlsx(file.path(pasta$prod_partidos, "banco_partidos_ano.xlsx")))
setnames(partidos_ano, setdiff(names(partidos_ano), c("SG_PARTIDO_PAD", "ANO")),
         paste0("partido_", setdiff(names(partidos_ano), c("SG_PARTIDO_PAD", "ANO"))))

vars_dir <- c("total_membros", "total_mulheres", "proporcao_mulheres", "total_decisao",
              "mulher_decisao", "proporcao_mulheres_decisao", "tem_mulher_presidente",
              "tem_mulher_tesoureiro", "turnover_prop", "turnover_prop_mulheres",
              "turnover_prop_decisao")

diretorio_dirigentes <- function(arquivo, chave) {
  x <- as.data.table(read.xlsx(file.path(pasta$prod_dirigentes, arquivo)))
  x[, ANO := as.integer(ANO)]
  if ("CD_MUNICIPIO" %in% chave) x[, CD_MUNICIPIO := as.integer(CD_MUNICIPIO)]
  x <- x[, c(chave, intersect(vars_dir, names(x))), with = FALSE]
  setnames(x, setdiff(names(x), chave), paste0("diretorio_", setdiff(names(x), chave)))
  x
}

vars_distrito <- "^distrito_(ano_eleicao|aptos|total_eleitores|pct_comparecimento|prop_mulheres|prop_sup_completo|vagas_)"

distrito_tab <- function(arquivo, chave) {
  x <- ler_banco(file.path(pasta$juncao_partidos, arquivo))
  x[, ANO := as.integer(ANO)]
  if ("CD_MUNICIPIO" %in% chave) x[, CD_MUNICIPIO := as.integer(CD_MUNICIPIO)]
  x[, c(chave, grep(vars_distrito, names(x), value = TRUE)), with = FALSE]
}

# Colunas técnicas dos bancos de dirigentes que não vão para o produto final
cols_tecnicas <- c("arquivo_origem", "partido", "dt_inicio", "dt_fim", "dt_fim2", "join_inicio",
                   "join_fim", "inicio_no_ano", "fim_no_ano", "CD_TIPO_ABRANGENCIA",
                   "SQ_CARGO_MEMBRO", "CD_GENERO", "CD_TIPO_ORGAO_PARTIDARIO",
                   # data de geração do arquivo e contato do órgão
                   "DT_GERACAO", "HH_GERACAO", "DS_ENDERECO_ORGAO", "NR_CEP_ORGAO",
                   "NR_FONE_FIXO_ORGAO", "NR_FONE_CEL_ORGAO", "NR_FONE_COMERCIAL_ORGAO",
                   "NR_FAX_ORGAO", "TX_EMAIL_ORGAO")

# Trajetória e candidaturas (precisam de todos os anos juntos)
preparar_dirigentes <- function(d, unidade) {
  d[, ANO := as.integer(ANO)]
  if ("CD_MUNICIPIO" %in% names(d)) d[, CD_MUNICIPIO := as.integer(CD_MUNICIPIO)]
  d[, intersect(cols_tecnicas, names(d)) := NULL]
  d <- aplicar_trajetoria(d, unidade)
  d <- aplicar_candidaturas(d)
  d[, SG_PARTIDO_PAD := sigla_atual(SG_PARTIDO)]
  d
}

# Partido, diretório e distrito (podem ser feitos ano a ano)
enriquecer_dirigentes <- function(d, unidade, dir_tab, dir_chave, dist_tab, dist_chave) {
  d <- merge(d, partidos_ano, by = c("SG_PARTIDO_PAD", "ANO"), all.x = TRUE, sort = FALSE)
  d <- merge(d, dir_tab, by = dir_chave, all.x = TRUE, sort = FALSE)
  d <- merge(d, dist_tab, by = dist_chave, all.x = TRUE, sort = FALSE)
  setorderv(d, c("ANO", unidade, "NM_TIPO_ORGAO_PARTIDARIO", "id_membro"))
  d
}

montar_dirigentes <- function(d, unidade, dir_tab, dir_chave, dist_tab, dist_chave) {
  enriquecer_dirigentes(preparar_dirigentes(d, unidade), unidade, dir_tab, dir_chave, dist_tab, dist_chave)
}

# Candidaturas agregadas por diretório (tipo de órgão incluído)
candidaturas_diretorio <- function(d, chave) {
  # primeiro uma linha por pessoa no diretório (a pessoa pode ter mais de um cargo)
  p <- d[, .(
    foi    = max(foi_candidato),
    ja     = max(ja_eleito),
    no_ano = max(candidato_no_ano),
    mulher = max(DS_GENERO == "FEMININO"),
    dec    = max(cargo_decisao)
  ), by = c(chave, "id_membro")]
  p[, .(
    n_dirigentes_ja_candidatos      = sum(foi),
    n_dirigentes_ja_eleitos         = sum(ja),
    n_dirigentes_candidatos_no_ano  = sum(no_ano),
    n_mulheres_ja_candidatas        = sum(foi == 1L & mulher == 1L),
    n_decisao_ja_candidatos         = sum(foi == 1L & dec == 1L)
  ), by = chave]
}

resumo <- function(d, rotulo) {
  cat("\n===", rotulo, "===\n")
  print(d[, .(linhas = .N,
              pessoas = uniqueN(id_membro),
              pct_foi_candidato = round(100 * mean(foi_candidato), 1),
              pct_ja_eleito     = round(100 * mean(ja_eleito), 1),
              pct_presente_ant  = round(100 * mean(presente_ano_anterior, na.rm = TRUE), 1),
              pct_com_partido   = round(100 * mean(!is.na(partido_bls_ideologia) | !is.na(partido_bolognesi_ideologia)), 1),
              pct_com_diretorio = round(100 * mean(!is.na(diretorio_total_membros)), 1),
              pct_com_distrito  = round(100 * mean(!is.na(distrito_ano_eleicao)), 1)),
          by = ANO][order(ANO)])
}


### 4. Nacional -----------------------------------------------------------------

d_nac <- as.data.table(read.xlsx(file.path(pasta$prod_dirigentes, "banco_dirigentes_nacional.xlsx")))
d_nac <- montar_dirigentes(
  d_nac, unidade = "SG_PARTIDO",
  dir_tab   = diretorio_dirigentes("banco_diretorios_nacional.xlsx", c("ANO", "SG_PARTIDO", "NM_TIPO_ORGAO_PARTIDARIO")),
  dir_chave = c("ANO", "SG_PARTIDO", "NM_TIPO_ORGAO_PARTIDARIO"),
  dist_tab  = distrito_tab("distritos_brasil_ano.xlsx", "ANO"),
  dist_chave = "ANO"
)
resumo(d_nac, "Dirigentes nacionais")
write.xlsx(d_nac, file.path(saida, "bd_dirigentes_nacional.xlsx"), overwrite = TRUE)

# Diretórios nacionais: órgão de direção + Comissão Executiva (prefixo ce_)
cand_nac <- candidaturas_diretorio(d_nac, c("ANO", "SG_PARTIDO", "NM_TIPO_ORGAO_PARTIDARIO"))
od <- as.data.table(read.xlsx(file.path(pasta$juncao_partidos, "diretorios_nacional_orgao_direcao.xlsx")))
ce <- as.data.table(read.xlsx(file.path(pasta$juncao_partidos, "diretorios_nacional_comissao_executiva.xlsx")))
od <- merge(od, cand_nac, by = c("ANO", "SG_PARTIDO", "NM_TIPO_ORGAO_PARTIDARIO"), all.x = TRUE, sort = FALSE)
ce <- merge(ce, cand_nac, by = c("ANO", "SG_PARTIDO", "NM_TIPO_ORGAO_PARTIDARIO"), all.x = TRUE, sort = FALSE)
# Da Comissão Executiva entram só as variáveis de composição e turnover (as do
# banco de diretórios dos códigos 1_x) e as de candidaturas
vars_composicao <- setdiff(names(read.xlsx(file.path(pasta$prod_dirigentes, "banco_diretorios_nacional.xlsx"), rows = 1:2)),
                           c("ANO", "SG_PARTIDO", "NR_PARTIDO", "SG_UF", "NM_TIPO_ORGAO_PARTIDARIO"))
vars_orgao <- c(vars_composicao, setdiff(names(cand_nac), c("ANO", "SG_PARTIDO", "NM_TIPO_ORGAO_PARTIDARIO")),
                "NM_TIPO_ORGAO_PARTIDARIO", "n_tipos_orgao_ano", "tipos_orgao_ano")
chave_nac  <- c("ANO", "SG_PARTIDO_PAD")

# Variáveis do órgão: sem prefixo (órgão de direção) e com prefixo ce_
comp_od <- od[, c(chave_nac, intersect(names(od), vars_orgao)), with = FALSE]
cols_ce <- intersect(names(ce), vars_orgao)
comp_ce <- ce[, c(chave_nac, cols_ce), with = FALSE]
setnames(comp_ce, cols_ce, paste0("ce_", cols_ce))

# Variáveis comuns ao partido (sigla, financiamento, características, distrito):
# do órgão de direção e, quando o partido só tem Comissão Executiva no ano, dela
cols_comuns <- setdiff(names(od), vars_orgao)
comuns <- rbind(od[, cols_comuns, with = FALSE],
                ce[!od, on = chave_nac, intersect(cols_comuns, names(ce)), with = FALSE],
                fill = TRUE)

bd_dir_nac <- comuns |>
  merge(comp_od, by = chave_nac, all.x = TRUE) |>
  merge(comp_ce, by = chave_nac, all.x = TRUE)
setorderv(bd_dir_nac, chave_nac)
cat("\nbd_diretorios_nacional:", nrow(bd_dir_nac), "linhas\n")
write.xlsx(bd_dir_nac, file.path(saida, "bd_diretorios_nacional.xlsx"), overwrite = TRUE)
rm(d_nac, od, ce, ce_sel, bd_dir_nac); gc()


### 5. Estadual -----------------------------------------------------------------

d_est <- as.data.table(read.xlsx(file.path(pasta$prod_dirigentes, "banco_dirigentes_estadual.xlsx")))
d_est <- montar_dirigentes(
  d_est, unidade = c("SG_PARTIDO", "SG_UF"),
  dir_tab   = diretorio_dirigentes("banco_diretorios_estadual.xlsx", c("ANO", "SG_PARTIDO", "SG_UF", "NM_TIPO_ORGAO_PARTIDARIO")),
  dir_chave = c("ANO", "SG_PARTIDO", "SG_UF", "NM_TIPO_ORGAO_PARTIDARIO"),
  dist_tab  = distrito_tab("distritos_uf_ano.xlsx", c("ANO", "sigla_uf")) |> setnames("sigla_uf", "SG_UF"),
  dist_chave = c("ANO", "SG_UF")
)
resumo(d_est, "Dirigentes estaduais")
write.xlsx(d_est, file.path(saida, "bd_dirigentes_estadual.xlsx"), overwrite = TRUE)

cand_est <- candidaturas_diretorio(d_est, c("ANO", "SG_PARTIDO", "SG_UF", "NM_TIPO_ORGAO_PARTIDARIO"))
bd_dir_est <- as.data.table(read.xlsx(file.path(pasta$juncao_partidos, "diretorios_estadual.xlsx")))
bd_dir_est <- merge(bd_dir_est, cand_est, by = c("ANO", "SG_PARTIDO", "SG_UF", "NM_TIPO_ORGAO_PARTIDARIO"),
                    all.x = TRUE, sort = TRUE)
cat("\nbd_diretorios_estadual:", nrow(bd_dir_est), "linhas\n")
write.xlsx(bd_dir_est, file.path(saida, "bd_diretorios_estadual.xlsx"), overwrite = TRUE)
rm(d_est, bd_dir_est); gc()


### 6. Municipal (um arquivo de dirigentes por ano) -----------------------------

d_mun <- fread(file.path(pasta$prod_dirigentes, "banco_dirigentes_mun2.csv"), sep = ";",
               colClasses = list(character = c("NR_TITULO_ELEITORAL_MEMBRO", "CD_MUNICIPIO", "SG_UE", "id_membro")),
               na.strings = c("", "NA"))
d_mun <- preparar_dirigentes(d_mun, unidade = c("SG_PARTIDO", "SG_UF", "CD_MUNICIPIO"))
chave_dir_mun <- c("ANO", "SG_PARTIDO", "SG_UF", "CD_MUNICIPIO", "NM_TIPO_ORGAO_PARTIDARIO")
cand_mun <- candidaturas_diretorio(d_mun, chave_dir_mun)

dir_mun  <- diretorio_dirigentes("banco_diretorios_municipal2.xlsx", chave_dir_mun)
dist_mun <- distrito_tab("distritos_municipio_ano.csv", c("ANO", "CD_MUNICIPIO"))

cat("\n=== Dirigentes municipais ===\n")
for (a in sort(unique(d_mun$ANO))) {
  x <- enriquecer_dirigentes(d_mun[ANO == a], c("SG_PARTIDO", "SG_UF", "CD_MUNICIPIO"),
                             dir_mun[ANO == a], chave_dir_mun, dist_mun[ANO == a], c("ANO", "CD_MUNICIPIO"))
  resumo(x, paste("Dirigentes municipais", a))
  fwrite(x, file.path(saida, "bd_dirigentes_municipal", paste0("bd_dirigentes_municipal_", a, ".csv")), sep = ";")
  rm(x); gc()
}
cat("bd_dirigentes_municipal: um arquivo por ano em", file.path(saida, "bd_dirigentes_municipal"), "\n")
rm(d_mun, dir_mun, dist_mun); gc()

bd_dir_mun <- ler_banco(file.path(pasta$juncao_partidos, "diretorios_municipal.csv"))
bd_dir_mun[, `:=`(ANO = as.integer(ANO), CD_MUNICIPIO = as.integer(CD_MUNICIPIO))]
bd_dir_mun <- merge(bd_dir_mun, cand_mun,
                    by = c("ANO", "SG_PARTIDO", "SG_UF", "CD_MUNICIPIO", "NM_TIPO_ORGAO_PARTIDARIO"),
                    all.x = TRUE, sort = TRUE)
cat("\nbd_diretorios_municipal:", nrow(bd_dir_mun), "linhas\n")
fwrite(bd_dir_mun, file.path(saida, "bd_diretorios_municipal.csv"), sep = ";")

cat("\nBancos salvos em:", saida, "\n")
