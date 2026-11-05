### bd_representa_analise — Análises exploratórias (Pasta 4) ---------------------
# Bruno Schaefer (IESP-UERJ)
#
# Objetivo: análises exploratórias com os bancos finais do bd_representa
# (3 Produtos finais/bancos). Cada análise gera tabelas (.csv, separador ";") e
# figuras (.png) em "4 Análises exploratórias/Figuras e tabelas", com o prefixo
# da análise no nome do arquivo. Todas as tabelas também vão para um único
# arquivo "tabelas_bd_representa_analise.xlsx" (uma aba por tabela) e a lista de
# bancos e variáveis usados em cada análise vai para "0_bancos_e_variaveis.csv".
#
# Análises:
#   3.1 Tamanho eleitoral do partido no distrito e proporção de mulheres dirigentes
#   3.2 Dirigente mulher e financiamento de candidatas
#   3.3 Condições socioeconômicas do distrito e índices de igualdade de gênero e raça
#   F   Relação entre classificações de ideologia (BLS x Bolognesi)
#   G   Relação entre classificações de democracia partidária (Salgado x Ribeiro)
#   H   Tipos de despesas eleitorais por cargo e ano
#   I   Tipos de despesas partidárias por esfera e ano
#   J   Tipos de receitas eleitorais por cargo e ano
#   K   Tipos de receitas partidárias por esfera e ano
#   L   Despesas partidárias e ideologia
#   M   Dependência das direções estaduais de transferências intrapartidárias
#   N   Recursos da direção e bancada eleita (Assembleia Legislativa)
#   O   Recursos da direção e bancada eleita (Câmara de Vereadores)
#   P   Dirigentes candidatos
#   Q   Gasto com internet e votos
#   R   Ser dirigente e acesso a recursos de campanha
#   S   Rotatividade das direções partidárias após as eleições (três esferas)
#
# Regras de leitura: todo banco é lido com o pacote here, pelo nome do banco, e
# só com as variáveis necessárias (função ler_bd). Valores monetários estão em
# R$ de dezembro de 2025 (deflacionados nos bancos finais).
#
# Uso: abrir o projeto (Dirigentes Partidários.Rproj) e rodar este arquivo
# (source). Leva cerca de 15 a 25 minutos (os bancos de vereança e de
# dirigentes municipais são grandes).

library(here)
library(data.table)
library(openxlsx)
library(ggplot2)
library(scales)
library(ggrepel)
library(fixest)

# Pastas --------------------------------------------------------------------------

bancos <- function(...) here("3 Produtos finais", "bancos", ...)
saida  <- here("4 Análises exploratórias", "Figuras e tabelas")
dir.create(saida, showWarnings = FALSE, recursive = TRUE)

# Registro dos bancos e variáveis usados em cada análise ---------------------------

registro <- list()
analise  <- ""   # análise em curso (preenchida no início de cada seção)

# Lê um banco final pelo nome, só com as variáveis indicadas.
#   arquivo: nome do arquivo em 3 Produtos finais/bancos (ou subpasta/arquivo)
#   vars:    variáveis a manter
ler_bd <- function(arquivo, vars) {
  caminho <- bancos(arquivo)
  if (grepl("\\.csv$", arquivo)) {
    chr <- intersect(c("id_municipio", "id_municipio_tse", "titulo_eleitoral",
                       "NR_TITULO_ELEITORAL_MEMBRO"), vars)
    d <- fread(caminho, sep = ";", select = vars, na.strings = c("", "NA"),
               colClasses = list(character = chr), showProgress = FALSE)
  } else {
    cab  <- names(read.xlsx(caminho, rows = 1:2))
    falt <- setdiff(vars, cab)
    if (length(falt)) stop(arquivo, ": variáveis inexistentes: ", paste(falt, collapse = ", "))
    d <- as.data.table(read.xlsx(caminho, cols = match(vars, cab)))
    setcolorder(d, vars)
  }
  registro[[length(registro) + 1]] <<- data.table(
    analise = analise, banco = sub("\\.(csv|xlsx)$", "", basename(arquivo)),
    variaveis = paste(vars, collapse = ", "))
  d
}

# Registra o uso de um banco já lido em outra análise
usar_bd <- function(arquivo, vars) {
  registro[[length(registro) + 1]] <<- data.table(
    analise = analise, banco = sub("\\.(csv|xlsx)$", "", basename(arquivo)),
    variaveis = paste(vars, collapse = ", "))
}

# Saídas -----------------------------------------------------------------------------

tabelas <- list()

salvar_tab <- function(d, nome) {
  d <- as.data.table(d)
  num <- names(d)[vapply(d, is.double, logical(1))]
  for (v in num) set(d, j = v, value = round(d[[v]], 4))
  fwrite(d, file.path(saida, paste0(nome, ".csv")), sep = ";", bom = TRUE)
  tabelas[[nome]] <<- d
  invisible(d)
}

salvar_fig <- function(p, nome, largura = 10, altura = 6) {
  ggsave(file.path(saida, paste0(nome, ".png")), p, width = largura, height = altura,
         dpi = 300, bg = "white")
}

# Paleta categórica (ordem fixa) e tema
cores <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
        legend.position = "top", strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"), plot.caption = element_text(colour = "grey40"))
theme_set(tema)

# Funções auxiliares ----------------------------------------------------------------------

titulo_norm <- function(x) {
  if (is.numeric(x)) x <- format(x, scientific = FALSE, trim = TRUE)
  x <- sub("^0+", "", as.character(x))
  fifelse(grepl("^[0-9]+$", x), x, NA_character_)
}

razao <- function(a, b) {
  r <- a / b
  r[rep_len(is.na(b) | b <= 0, length(r))] <- NA_real_
  r
}

correlacoes <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 5) return(list(n = sum(ok), pearson = NA_real_, p_pearson = NA_real_, spearman = NA_real_))
  ct <- suppressWarnings(cor.test(x[ok], y[ok]))
  list(n = sum(ok), pearson = unname(ct$estimate), p_pearson = ct$p.value,
       spearman = suppressWarnings(cor(x[ok], y[ok], method = "spearman")))
}

# feols que devolve NULL quando o modelo não pode ser estimado (ex.: grupo sem variação)
feols_s <- function(...) tryCatch(feols(...), error = function(e) NULL)

coef_tab <- function(m, termo) {
  if (is.null(m)) return(data.table(coef = NA_real_, ep = NA_real_, p = NA_real_, n = NA_integer_))
  ct <- coeftable(m)
  if (!termo %in% rownames(ct)) return(data.table(coef = NA_real_, ep = NA_real_, p = NA_real_, n = nobs(m)))
  data.table(coef = ct[termo, 1], ep = ct[termo, 2], p = ct[termo, 4], n = nobs(m))
}

rotulo_cargo <- c(vereanca = "Vereador", prefeitura = "Prefeito", assembleia = "Dep. estadual/distrital",
                  camara = "Dep. federal", senado = "Senador", governadoria = "Governador")

fonte_txt <- "Fonte: bd_representa (Schaefer, 2026)."


### Leitura dos bancos compartilhados entre análises -----------------------------------

# Diretórios (composição, despesas e receitas) ----------------------------------------
vars_desp <- c("desp_valor_total_bruto", "desp_valor_aparato_pessoal", "desp_valor_aparato_sedes",
               "desp_valor_aparato_politica", "desp_valor_eleicoes_comunicacao",
               "desp_valor_eleicoes_publicidade", "desp_valor_transferencias",
               "desp_valor_financeiro_contabil", "desp_valor_nao_classificado")
vars_rec  <- c("rec_valor_total_bruto", "rec_valor_total_identificado", "rec_valor_origem_cota_fefc",
               "rec_valor_origem_cota_fundo_part", "rec_valor_origem_partidos_politicos",
               "rec_valor_origem_candidatos", "rec_valor_origem_pessoas_fisicas",
               "rec_valor_origem_pessoas_juridicas", "rec_valor_origem_nao_identificada",
               "rec_valor_recursos_publicos", "rec_valor_recursos_intrapartidarios",
               "rec_valor_recursos_privados")
vars_comp <- c("total_membros", "proporcao_mulheres", "proporcao_mulheres_decisao",
               "tem_mulher_presidente", "tem_mulher_tesoureiro", "tem_mulher_decisao")

vars_turn <- c("n_membros_t1", "turnover_prop", "turnover_prop_decisao", "turnover_prop_mulheres", "turnover_prop_homens")

analise <- "(leitura)"
dir_nac <- ler_bd("bd_diretorios_nacional.xlsx",
                  c("ANO", "SG_PARTIDO_PAD", "bls_ideologia", "bolognesi_ideologia", "bolognesi_categoria",
                    "salgado_democracia_interna", "ribeiro_ipd", vars_desp, vars_rec, vars_turn))
dir_est <- ler_bd("bd_diretorios_estadual.xlsx",
                  c("ANO", "SG_PARTIDO_PAD", "SG_UF", vars_comp, vars_desp, vars_rec, vars_turn))
dir_mun <- ler_bd("bd_diretorios_municipal.csv",
                  c("ANO", "SG_PARTIDO_PAD", "SG_UF", "CD_MUNICIPIO", vars_comp, vars_desp, vars_rec, vars_turn))
dir_nac[, ANO := as.integer(ANO)]
dir_est[, ANO := as.integer(ANO)]
dir_mun[, ANO := as.integer(ANO)]
dir_mun[, CD_MUNICIPIO := as.integer(CD_MUNICIPIO)]
# uma linha por partido-distrito-ano
dir_est <- unique(dir_est, by = c("ANO", "SG_PARTIDO_PAD", "SG_UF"))
dir_mun <- unique(dir_mun, by = c("ANO", "SG_PARTIDO_PAD", "CD_MUNICIPIO"))
dir_nac <- unique(dir_nac, by = c("ANO", "SG_PARTIDO_PAD"))

# Listas partidárias ---------------------------------------------------------------------
vars_lista <- c("ano", "SG_PARTIDO_PAD", "cargo", "n_candidatos", "n_eleitos", "pct_votos_nominais_distrito",
                "pct_cadeiras", "prop_candidatos_mulheres", "prop_receitas_mulheres",
                "prop_receitas_partidarias_mulheres", "prop_receitas_direcao_mulheres",
                "n_dirigentes_candidatos", "n_dirigentes_decisao_candidatos", "n_dirigentes_eleitos",
                "diretorio_rec_valor_total_bruto")
part_ver <- ler_bd("bd_partidos_vereanca.csv", c(vars_lista[1:2], "id_municipio_tse", vars_lista[-(1:2)]))
part_cam <- ler_bd("bd_partidos_camara.xlsx", c(vars_lista[1:2], "sigla_uf", vars_lista[-(1:2)],
                                               "n_dirigentes_nacionais_candidatos"))
part_ass <- ler_bd("bd_partidos_assembleias.xlsx", c(vars_lista[1:2], "sigla_uf", vars_lista[-(1:2)],
                                                    "n_dirigentes_nacionais_candidatos"))
part_ver[, `:=`(ano = as.integer(ano), CD_MUNICIPIO = as.integer(id_municipio_tse))]
part_cam[, ano := as.integer(ano)]
part_ass[, ano := as.integer(ano)]

# Candidaturas -----------------------------------------------------------------------------
vars_cand <- c("ano", "cargo", "sigla_uf", "SG_PARTIDO_PAD", "titulo_eleitoral", "mulher", "incumbente",
               "eleito_final", "pct_votos_distrito", "soma_receitas", "soma_recursos_publicos",
               "soma_recursos_pessoa_fisica", "soma_recursos_pessoa_juridica", "soma_recursos_proprios",
               "soma_recursos_partidos", "soma_recursos_outros_candidatos", "despesas_total_can",
               "despesas_internet_can", "despesas_tradicional_can", "despesas_moderno_can",
               "despesas_organizacao_can", "despesas_outras_can")
arq_cand <- c(vereanca = "bd_candidatos_vereanca.csv", prefeitura = "bd_candidatos_prefeitura.xlsx",
              assembleia = "bd_candidatos_assembleia.xlsx", camara = "bd_candidatos_camara.xlsx",
              senado = "bd_candidatos_senado.xlsx", governadoria = "bd_candidatos_governadoria.xlsx")
cand <- rbindlist(lapply(names(arq_cand), function(k) {
  municipal <- k %in% c("vereanca", "prefeitura")
  v <- if (municipal) c(vars_cand[1:3], "id_municipio", vars_cand[-(1:3)]) else vars_cand
  d <- ler_bd(arq_cand[[k]], v)
  d[, `:=`(banco = k, titulo_eleitoral = titulo_norm(titulo_eleitoral), ano = as.integer(ano))]
  d[, distrito := if (municipal) as.character(id_municipio) else sigla_uf]
  if (municipal) d[, id_municipio := NULL]
  d
}), use.names = TRUE)
cand[, cargo_rot := factor(rotulo_cargo[banco], levels = rotulo_cargo)]
cand[, eleito_final := as.integer(eleito_final == "eleito")]   # texto "eleito"/"nao eleito" -> 1/0
num_cand <- grep("^(soma_|despesas_)", names(cand), value = TRUE)
for (v in num_cand) set(cand, j = v, value = as.numeric(cand[[v]]))


### 3.1 Tamanho eleitoral do partido no distrito e mulheres na direção -----------------
# Por cargo eleitoral: vereança (diretórios municipais x votação da lista para
# vereador no município) e deputado estadual/distrital (diretórios estaduais x
# votação da lista para a Assembleia na UF). Em cada caso, a votação é a da
# eleição anterior ao ano da direção.
analise <- "3.1"
usar_bd("bd_diretorios_municipal.csv", c("ANO", "SG_PARTIDO_PAD", "CD_MUNICIPIO", vars_comp))
usar_bd("bd_partidos_vereanca.csv", c("ano", "SG_PARTIDO_PAD", "id_municipio_tse", "pct_votos_nominais_distrito"))
usar_bd("bd_diretorios_estadual.xlsx", c("ANO", "SG_PARTIDO_PAD", "SG_UF", vars_comp))
usar_bd("bd_partidos_assembleias.xlsx", c("ano", "SG_PARTIDO_PAD", "sigla_uf", "pct_votos_nominais_distrito"))

v31 <- c("ANO", "SG_PARTIDO_PAD", "total_membros", "proporcao_mulheres", "proporcao_mulheres_decisao",
         "tem_mulher_presidente", "tem_mulher_tesoureiro")
# Vereança: 2015-2016 -> eleição de 2012; 2017-2020 -> 2016; 2021-2024 -> 2020; 2025 -> 2024
m31 <- dir_mun[, c(v31, "CD_MUNICIPIO"), with = FALSE][, distrito := as.character(CD_MUNICIPIO)]
m31[, ano_eleicao := 2012L + 4L * ((ANO - 2013L) %/% 4L)]
m31 <- merge(m31, part_ver[, .(ano_eleicao = ano, SG_PARTIDO_PAD, CD_MUNICIPIO, pct_votos = pct_votos_nominais_distrito)],
             by = c("ano_eleicao", "SG_PARTIDO_PAD", "CD_MUNICIPIO"), all.x = TRUE)
m31[, `:=`(cargo_rot = "Vereador", CD_MUNICIPIO = NULL)]
# Assembleia: 2015-2018 -> eleição de 2014; 2019-2022 -> 2018; 2023-2025 -> 2022
e31 <- dir_est[, c(v31, "SG_UF"), with = FALSE][, distrito := SG_UF]
e31[, ano_eleicao := 2010L + 4L * ((ANO - 2011L) %/% 4L)]
e31 <- merge(e31, part_ass[, .(ano_eleicao = ano, SG_PARTIDO_PAD, SG_UF = sigla_uf, pct_votos = pct_votos_nominais_distrito)],
             by = c("ano_eleicao", "SG_PARTIDO_PAD", "SG_UF"), all.x = TRUE)
e31[, `:=`(cargo_rot = "Dep. estadual/distrital", SG_UF = NULL)]
d31 <- rbind(m31, e31, use.names = TRUE)
d31[, cargo_rot := factor(cargo_rot, levels = c("Vereador", "Dep. estadual/distrital"))]
# partido sem lista na eleição anterior = 0% dos votos
d31[, sem_lista := is.na(pct_votos)]
d31[is.na(pct_votos), pct_votos := 0]
d31[, faixa := cut(pct_votos, c(-Inf, 0, 5, 10, 20, 30, Inf), right = TRUE,
                   labels = c("Sem votos/lista", "0-5%", "5-10%", "10-20%", "20-30%", "30% ou mais"))]
for (v in v31[-(1:2)]) set(d31, j = v, value = as.numeric(d31[[v]]))

indic31 <- c(proporcao_mulheres = "Mulheres na direção (todos os cargos)",
             proporcao_mulheres_decisao = "Mulheres em cargos de decisão",
             tem_mulher_presidente = "Mulher na presidência",
             tem_mulher_tesoureiro = "Mulher na tesouraria")
long31 <- melt(d31, id.vars = c("cargo_rot", "ANO", "SG_PARTIDO_PAD", "distrito", "pct_votos", "faixa", "total_membros"),
               measure.vars = names(indic31), variable.name = "indicador", value.name = "valor")
long31[, indicador_rot := factor(indic31[as.character(indicador)], levels = indic31)]

t31a <- long31[!is.na(valor), .(n_diretorios = .N, media = 100 * mean(valor)), by = .(cargo_rot, indicador_rot, faixa)][order(cargo_rot, indicador_rot, faixa)]
salvar_tab(dcast(t31a, cargo_rot + faixa ~ indicador_rot, value.var = "media"), "3_1_mulheres_direcao_por_votacao_anterior")
salvar_tab(d31[, .(n_diretorios = .N), by = .(cargo_rot, faixa)][order(cargo_rot, faixa)], "3_1_n_diretorios_por_faixa")

# Regressões: votação anterior (em 10 p.p.) sobre cada indicador, por cargo eleitoral
#   (1) efeitos fixos de ano; (2) efeitos fixos de distrito-ano (comparação entre
#   partidos do mesmo distrito no mesmo ano); controle: log do tamanho da direção
d31[, votos10 := pct_votos / 10]
t31b <- rbindlist(lapply(levels(d31$cargo_rot), function(cg) rbindlist(lapply(names(indic31), function(v) {
  x  <- d31[cargo_rot == cg & total_membros > 0]
  m1 <- feols_s(as.formula(paste(v, "~ votos10 + log(total_membros) | ANO")), x, cluster = ~distrito)
  m2 <- feols_s(as.formula(paste(v, "~ votos10 + log(total_membros) | distrito^ANO")), x, cluster = ~distrito)
  rbind(cbind(cargo = cg, indicador = indic31[[v]], modelo = "EF ano", coef_tab(m1, "votos10")),
        cbind(cargo = cg, indicador = indic31[[v]], modelo = "EF distrito-ano", coef_tab(m2, "votos10")))
}))))
t31b[, `:=`(coef_pp = 100 * coef, ep_pp = 100 * ep)]
salvar_tab(t31b[, .(cargo, indicador, modelo, coef_pp, ep_pp, p, n)], "3_1_regressoes_votacao_mulheres_direcao")

# Por ano da direção
t31c <- long31[!is.na(valor), .(media = 100 * mean(valor), n = .N), by = .(cargo_rot, ANO, indicador_rot, faixa)]
salvar_tab(t31c[order(cargo_rot, ANO, indicador_rot, faixa)], "3_1_mulheres_direcao_por_votacao_e_ano")

p31 <- ggplot(t31a, aes(faixa, media, colour = cargo_rot, group = cargo_rot)) +
  geom_line(linewidth = 0.7) + geom_point(size = 2.5) +
  facet_wrap(~indicador_rot, scales = "free_y") +
  scale_colour_manual(values = cores[1:2], name = NULL) +
  labs(x = "Votação nominal da lista do partido na eleição anterior (% do distrito)",
       y = "% (média dos diretórios)",
       title = "Votação do partido e presença de mulheres na direção",
       subtitle = "Vereador: diretórios municipais; Dep. estadual/distrital: diretórios estaduais; 2015–2025",
       caption = paste(fonte_txt, "Bancos: bd_diretorios_municipal/estadual e bd_partidos_vereanca/assembleias.")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
salvar_fig(p31, "3_1_mulheres_direcao_por_votacao_anterior", 10, 6.5)


### 3.2 Dirigente mulher e financiamento de candidatas ----------------------------------
analise <- "3.2"
usar_bd("bd_partidos_vereanca.csv", c("ano", "SG_PARTIDO_PAD", "id_municipio_tse", "prop_candidatos_mulheres",
                                       "prop_receitas_mulheres", "prop_receitas_partidarias_mulheres",
                                       "prop_receitas_direcao_mulheres"))
usar_bd("bd_partidos_camara.xlsx", c("ano", "SG_PARTIDO_PAD", "sigla_uf", "prop_candidatos_mulheres",
                                      "prop_receitas_mulheres", "prop_receitas_partidarias_mulheres",
                                      "prop_receitas_direcao_mulheres"))
usar_bd("bd_partidos_assembleias.xlsx", c("ano", "SG_PARTIDO_PAD", "sigla_uf", "prop_candidatos_mulheres",
                                           "prop_receitas_mulheres", "prop_receitas_partidarias_mulheres",
                                           "prop_receitas_direcao_mulheres"))
usar_bd("bd_diretorios_municipal.csv", c("ANO", "SG_PARTIDO_PAD", "CD_MUNICIPIO", "tem_mulher_presidente", "tem_mulher_decisao"))
usar_bd("bd_diretorios_estadual.xlsx", c("ANO", "SG_PARTIDO_PAD", "SG_UF", "tem_mulher_presidente", "tem_mulher_decisao"))

# Direção medida no ano anterior à eleição (2015 -> 2016 ... 2023 -> 2024); 2026 fica de fora
v32 <- c("prop_candidatos_mulheres", "prop_receitas_mulheres", "prop_receitas_partidarias_mulheres",
         "prop_receitas_direcao_mulheres")
l_ver <- merge(part_ver[ano %in% c(2016L, 2020L, 2024L), c("ano", "SG_PARTIDO_PAD", "CD_MUNICIPIO", v32), with = FALSE],
               dir_mun[, .(ano = ANO + 1L, SG_PARTIDO_PAD, CD_MUNICIPIO, tem_mulher_presidente, tem_mulher_decisao)],
               by = c("ano", "SG_PARTIDO_PAD", "CD_MUNICIPIO"))
l_ver[, `:=`(cargo_rot = "Vereador", distrito = as.character(CD_MUNICIPIO), CD_MUNICIPIO = NULL)]
l_ger <- rbind(cbind(part_cam[, c("ano", "SG_PARTIDO_PAD", "sigla_uf", v32), with = FALSE], cargo_rot = "Dep. federal"),
               cbind(part_ass[, c("ano", "SG_PARTIDO_PAD", "sigla_uf", v32), with = FALSE], cargo_rot = "Dep. estadual/distrital"))
l_ger <- merge(l_ger[ano %in% c(2018L, 2022L)],
               dir_est[, .(ano = ANO + 1L, SG_PARTIDO_PAD, sigla_uf = SG_UF, tem_mulher_presidente, tem_mulher_decisao)],
               by = c("ano", "SG_PARTIDO_PAD", "sigla_uf"))
l_ger[, distrito := sigla_uf][, sigla_uf := NULL]
d32 <- rbind(l_ver, l_ger, use.names = TRUE)
d32[, cargo_rot := factor(cargo_rot, levels = c("Vereador", "Dep. estadual/distrital", "Dep. federal"))]

rec32 <- c(prop_receitas_mulheres = "Receitas totais",
           prop_receitas_partidarias_mulheres = "Recursos de partidos",
           prop_receitas_direcao_mulheres = "Recursos doados por direções")
trat32 <- c(tem_mulher_presidente = "Mulher na presidência", tem_mulher_decisao = "Mulher em cargo de decisão")

t32 <- rbindlist(lapply(names(trat32), function(tr) rbindlist(lapply(names(rec32), function(y) {
  x <- d32[!is.na(get(y)) & !is.na(get(tr))]
  desc <- x[, .(com_mulher = 100 * mean(get(y)[get(tr) == 1]), sem_mulher = 100 * mean(get(y)[get(tr) == 0]),
                cand_com = 100 * mean(prop_candidatos_mulheres[get(tr) == 1], na.rm = TRUE),
                cand_sem = 100 * mean(prop_candidatos_mulheres[get(tr) == 0], na.rm = TRUE),
                n_com = sum(get(tr) == 1), n_sem = sum(get(tr) == 0)), by = .(cargo_rot, ano)]
  reg <- x[, {
    m <- feols_s(as.formula(paste(y, "~", tr, "+ prop_candidatos_mulheres")), .SD, cluster = ~distrito)
    coef_tab(m, tr)
  }, by = .(cargo_rot, ano)]
  out <- merge(desc, reg, by = c("cargo_rot", "ano"))
  out[, `:=`(tratamento = trat32[[tr]], recurso = rec32[[y]], dif_pp = com_mulher - sem_mulher,
             coef_ajustado_pp = 100 * coef, ep_pp = 100 * ep)]
  out[, .(tratamento, recurso, cargo_rot, ano, n_com, n_sem, cand_com, cand_sem, com_mulher, sem_mulher,
          dif_pp, coef_ajustado_pp, ep_pp, p)]
}))))
salvar_tab(t32[order(tratamento, recurso, cargo_rot, ano)], "3_2_direcao_mulher_e_recursos_candidatas")

g32 <- melt(t32, id.vars = c("tratamento", "recurso", "cargo_rot", "ano"), measure.vars = c("com_mulher", "sem_mulher"),
            variable.name = "grupo", value.name = "pct")
g32[, grupo := factor(fifelse(grupo == "com_mulher", "Com mulher", "Sem mulher"), levels = c("Com mulher", "Sem mulher"))]
for (tr in unique(g32$tratamento)) {
  p <- ggplot(g32[tratamento == tr], aes(factor(ano), pct, colour = grupo, group = grupo)) +
    geom_line(linewidth = 0.7) + geom_point(size = 2.5) +
    facet_grid(recurso ~ cargo_rot, scales = "free_x", space = "free_x") +
    scale_colour_manual(values = cores[1:2], name = NULL) +
    labs(x = "Eleição", y = "% dos recursos da lista destinados a mulheres",
         title = paste0("Direção partidária com ", tolower(tr), " (ano anterior à eleição) e recursos para candidatas"),
         subtitle = "Média das listas partidárias no distrito", caption = fonte_txt)
  salvar_fig(p, paste0("3_2_recursos_candidatas_", if (grepl("presid", tr)) "presidencia" else "decisao"), 10, 7.5)
}


### 3.3 PIB per capita e índices de igualdade de gênero e raça ---------------------------
analise <- "3.3"
med33 <- c("candidatos", "receitas", "receitas_partidarias", "eleitos")
v_mun <- c(paste0("indice_genero_", med33, "_vereanca"), paste0("indice_racial_", med33, "_vereanca"))
v_est <- as.vector(outer(c("indice_genero_", "indice_racial_"), paste0(rep(med33, 2), "_", rep(c("camara", "assembleia"), each = 4)), paste0))
dist_m <- ler_bd("bd_distritos_municipal.xlsx", c("ano", "id_municipio", "pib_per_capita", v_mun))
dist_e <- ler_bd("bd_distritos_estadual.xlsx", c("ano", "sigla_uf", "pib_per_capita", v_est))

l33m <- melt(dist_m, id.vars = c("ano", "id_municipio", "pib_per_capita"), variable.name = "v", value.name = "indice")
l33m[, `:=`(nivel = "Municipal", distrito = id_municipio, cargo = "vereanca")]
l33e <- melt(dist_e, id.vars = c("ano", "sigla_uf", "pib_per_capita"), variable.name = "v", value.name = "indice")
l33e[, `:=`(nivel = "Estadual", distrito = sigla_uf, cargo = sub(".*_", "", v))]
l33 <- rbind(l33m[, .(nivel, cargo, ano, distrito, pib_per_capita, v, indice)],
             l33e[, .(nivel, cargo, ano, distrito, pib_per_capita, v, indice)])
l33[, `:=`(grupo = fifelse(grepl("genero", v), "Gênero (mulheres)", "Raça (pessoas negras)"),
           medida = sub("^indice_(genero|racial)_(.*)_(vereanca|camara|assembleia)$", "\\2", v),
           ano = as.integer(ano), pib_per_capita = as.numeric(pib_per_capita), indice = as.numeric(indice))]
l33[, cargo_rot := factor(rotulo_cargo[cargo], levels = rotulo_cargo)]
l33[, medida := factor(medida, levels = med33, labels = c("Candidaturas", "Receitas", "Receitas partidárias", "Eleitos"))]
l33 <- l33[is.finite(indice) & !is.na(pib_per_capita) & pib_per_capita > 0]
l33[, log_pib := log(pib_per_capita)]

t33 <- l33[, correlacoes(log_pib, indice), by = .(nivel, cargo_rot, grupo, medida, ano)][order(nivel, cargo_rot, grupo, medida, ano)]
t33p <- l33[, correlacoes(log_pib, indice), by = .(nivel, cargo_rot, grupo, medida)]
t33p[, ano := "todos"]
salvar_tab(rbind(t33[, ano := as.character(ano)], t33p, use.names = TRUE), "3_3_correlacao_pib_per_capita_indices")

# Municipal: média do índice por décimo de PIB per capita (dentro do ano)
l33[nivel == "Municipal", decil := frank(pib_per_capita, ties.method = "first") / .N, by = .(ano, v)]
l33[nivel == "Municipal", decil := pmin(10L, as.integer(ceiling(decil * 10)))]
t33d <- l33[nivel == "Municipal", .(indice = mean(indice), n = .N), by = .(grupo, medida, decil)]
salvar_tab(t33d[order(grupo, medida, decil)], "3_3_indices_por_decil_pib_municipal")
p33a <- ggplot(t33d, aes(decil, indice, colour = medida)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  facet_wrap(~grupo) + scale_x_continuous(breaks = 1:10) +
  scale_colour_manual(values = cores[1:4], name = NULL) +
  labs(x = "Décimo de PIB per capita do município (1 = mais pobre)", y = "Índice de equidade (DRI; 0 = paridade)",
       title = "PIB per capita municipal e equidade na vereança",
       subtitle = "Gênero: 2012–2020; raça: 2016–2020 (PIB municipal disponível até 2020 no banco)",
       caption = fonte_txt)
salvar_fig(p33a, "3_3_indices_por_decil_pib_municipal", 10, 5.5)

p33b <- ggplot(l33[nivel == "Estadual" & medida == "Eleitos"], aes(log_pib, indice, colour = cargo_rot)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_point(size = 2, alpha = 0.7) + geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, formula = y ~ x) +
  facet_wrap(~grupo) + scale_colour_manual(values = cores[1:2], name = NULL) +
  labs(x = "log do PIB per capita da UF", y = "Índice de equidade entre eleitos (DRI)",
       title = "PIB per capita estadual e equidade entre eleitos", subtitle = "UF-eleição, 2010–2022",
       caption = fonte_txt)
salvar_fig(p33b, "3_3_indices_eleitos_pib_estadual", 10, 5.5)


### F Ideologia: BLS x Bolognesi ---------------------------------------------------------
analise <- "F"
usar_bd("bd_diretorios_nacional.xlsx", c("ANO", "SG_PARTIDO_PAD", "bls_ideologia", "bolognesi_ideologia"))
ano_ref <- 2018L
tF <- dir_nac[, correlacoes(bls_ideologia, bolognesi_ideologia), by = ANO][order(ANO)]
salvar_tab(tF, "F_correlacao_bls_bolognesi_por_ano")
dF <- dir_nac[ANO == ano_ref & !is.na(bls_ideologia) & !is.na(bolognesi_ideologia),
              .(partido = SG_PARTIDO_PAD, bls_ideologia, bolognesi_ideologia)][order(bls_ideologia)]
salvar_tab(dF, paste0("F_ideologia_partidos_", ano_ref))
rF <- tF[ANO == ano_ref]
pF <- ggplot(dF, aes(bls_ideologia, bolognesi_ideologia, label = partido)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey60", linewidth = 0.5, formula = y ~ x) +
  geom_point(colour = cores[1], size = 2.5) + geom_text_repel(size = 3, max.overlaps = 30, seed = 1) +
  labs(x = "BLS (IDEO_IMPUTED; -1 esquerda a 1 direita)", y = "Bolognesi et al. (0 esquerda a 10 direita)",
       title = paste0("Ideologia dos partidos: BLS x Bolognesi (", ano_ref, ")"),
       subtitle = sprintf("Pearson = %.2f; Spearman = %.2f; %d partidos", rF$pearson, rF$spearman, rF$n),
       caption = paste(fonte_txt, "Banco: bd_diretorios_nacional."))
salvar_fig(pF, paste0("F_ideologia_bls_bolognesi_", ano_ref), 8, 6.5)


### G Democracia partidária: Salgado x Ribeiro ---------------------------------------------
analise <- "G"
usar_bd("bd_diretorios_nacional.xlsx", c("ANO", "SG_PARTIDO_PAD", "salgado_democracia_interna", "ribeiro_ipd"))
tG <- dir_nac[, correlacoes(salgado_democracia_interna, ribeiro_ipd), by = ANO][order(ANO)]
salvar_tab(tG, "G_correlacao_salgado_ribeiro_por_ano")
dG <- dir_nac[ANO == ano_ref & !is.na(salgado_democracia_interna) & !is.na(ribeiro_ipd),
              .(partido = SG_PARTIDO_PAD, salgado_democracia_interna, ribeiro_ipd)][order(salgado_democracia_interna)]
salvar_tab(dG, paste0("G_democracia_partidos_", ano_ref))
rG <- tG[ANO == ano_ref]
pG <- ggplot(dG, aes(salgado_democracia_interna, ribeiro_ipd, label = partido)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey60", linewidth = 0.5, formula = y ~ x) +
  geom_point(colour = cores[1], size = 2.5) + geom_text_repel(size = 3, max.overlaps = 30, seed = 1) +
  labs(x = "Democracia interna (Salgado)", y = "IPD (Ribeiro et al., média 2011–2014)",
       title = paste0("Democracia partidária: Salgado x Ribeiro (", ano_ref, ")"),
       subtitle = sprintf("Pearson = %.2f; Spearman = %.2f; %d partidos", rG$pearson, rG$spearman, rG$n),
       caption = paste(fonte_txt, "Banco: bd_diretorios_nacional."))
salvar_fig(pG, paste0("G_democracia_salgado_ribeiro_", ano_ref), 8, 6.5)


### H Despesas eleitorais por tipo, cargo e ano ---------------------------------------------
analise <- "H"
for (k in names(arq_cand)) usar_bd(arq_cand[[k]], c("ano", "cargo", "despesas_total_can", "despesas_internet_can",
                                                   "despesas_tradicional_can", "despesas_moderno_can",
                                                   "despesas_organizacao_can", "despesas_outras_can"))
tH <- cand[, .(n_candidatos = .N, total = sum(despesas_total_can, na.rm = TRUE),
               tradicional = sum(despesas_tradicional_can, na.rm = TRUE), moderno = sum(despesas_moderno_can, na.rm = TRUE),
               organizacao = sum(despesas_organizacao_can, na.rm = TRUE), outras = sum(despesas_outras_can, na.rm = TRUE),
               internet = sum(despesas_internet_can, na.rm = TRUE)), by = .(cargo_rot, ano)][order(cargo_rot, ano)]
tH[, soma_tipos := tradicional + moderno + organizacao + outras]
for (v in c("tradicional", "moderno", "organizacao", "outras", "internet")) tH[, paste0("pct_", v) := 100 * razao(get(v), total)]
tH[, total_mi := total / 1e6]
salvar_tab(tH[, .(cargo_rot, ano, n_candidatos, total_mi, pct_tradicional, pct_moderno, pct_organizacao, pct_outras,
                  pct_internet, cobertura_tipologia = 100 * razao(soma_tipos, total))], "H_despesas_eleitorais_por_tipo")
gH <- melt(tH, id.vars = c("cargo_rot", "ano"), measure.vars = c("pct_tradicional", "pct_moderno", "pct_organizacao", "pct_outras"),
           variable.name = "tipo", value.name = "pct")
gH[, tipo := factor(tipo, levels = c("pct_tradicional", "pct_moderno", "pct_organizacao", "pct_outras"),
                    labels = c("Tradicional", "Moderno", "Organização", "Outras"))]
# A tipologia só existe a partir de 2018 (gerais) e 2020 (municipais): antes, tudo cai em "outras"
anos_tipologia <- tH[pct_tradicional + pct_moderno > 0, .(cargo_rot, ano)]
gH <- merge(gH, anos_tipologia, by = c("cargo_rot", "ano"))
pH <- ggplot(gH, aes(factor(ano), pct, fill = tipo)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  geom_point(data = merge(tH, anos_tipologia, by = c("cargo_rot", "ano")), aes(factor(ano), pct_internet, shape = "Internet (parte de Moderno)"), inherit.aes = FALSE,
             size = 2.2, colour = "grey15") +
  facet_wrap(~cargo_rot, scales = "free_x") + scale_fill_manual(values = cores[1:4], name = NULL) +
  scale_shape_manual(values = 18, name = NULL) +
  labs(x = "Eleição", y = "% das despesas contratadas", title = "Composição das despesas de campanha por cargo e ano",
       caption = paste(fonte_txt, "Bancos: bd_candidatos_*. 2026: contas ainda parciais."))
salvar_fig(pH, "H_despesas_eleitorais_por_tipo", 11, 7)


### I Despesas partidárias por tipo, esfera e ano -------------------------------------------
analise <- "I"
for (a in c("bd_diretorios_nacional.xlsx", "bd_diretorios_estadual.xlsx", "bd_diretorios_municipal.csv"))
  usar_bd(a, c("ANO", vars_desp))
esferas <- rbind(cbind(esfera = "Nacional", dir_nac[, c("ANO", "SG_PARTIDO_PAD", vars_desp, vars_rec), with = FALSE]),
                 cbind(esfera = "Estadual", dir_est[, c("ANO", "SG_PARTIDO_PAD", vars_desp, vars_rec), with = FALSE]),
                 cbind(esfera = "Municipal", dir_mun[, c("ANO", "SG_PARTIDO_PAD", vars_desp, vars_rec), with = FALSE]))
esferas[, esfera := factor(esfera, levels = c("Nacional", "Estadual", "Municipal"))]
for (v in c(vars_desp, vars_rec)) set(esferas, j = v, value = as.numeric(esferas[[v]]))

catI <- c(desp_valor_aparato_pessoal = "Aparato: pessoal", desp_valor_aparato_sedes = "Aparato: sedes",
          desp_valor_aparato_politica = "Aparato: atividade política", desp_valor_eleicoes_comunicacao = "Eleições: comunicação",
          desp_valor_eleicoes_publicidade = "Eleições: publicidade", desp_valor_transferencias = "Transferências",
          desp_valor_financeiro_contabil = "Financeiro/contábil", desp_valor_nao_classificado = "Não classificado")
tI <- esferas[ANO >= 2017, c(list(n_orgaos_com_despesa = sum(desp_valor_total_bruto > 0, na.rm = TRUE),
                                 total_mi = sum(desp_valor_total_bruto, na.rm = TRUE) / 1e6),
                            lapply(.SD, function(x) sum(x, na.rm = TRUE))), by = .(esfera, ANO), .SDcols = names(catI)]
for (v in names(catI)) tI[, (v) := 100 * get(v) / (total_mi * 1e6)]
setnames(tI, names(catI), paste0("pct_", sub("desp_valor_", "", names(catI))))
salvar_tab(tI[order(esfera, ANO)], "I_despesas_partidarias_por_tipo")
gI <- melt(tI, id.vars = c("esfera", "ANO"), measure.vars = patterns("^pct_"), variable.name = "tipo", value.name = "pct")
gI[, tipo := factor(tipo, levels = paste0("pct_", sub("desp_valor_", "", names(catI))), labels = catI)]
pI <- ggplot(gI, aes(factor(ANO), pct, fill = tipo)) + geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
  facet_wrap(~esfera, ncol = 1) + scale_fill_manual(values = cores, name = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "Ano", y = "% das despesas (total bruto)", title = "Composição das despesas partidárias por esfera e ano",
       caption = paste(fonte_txt, "Bancos: bd_diretorios_*. Tipologia de Roeder (2019). 2025 incompleto."))
salvar_fig(pI, "I_despesas_partidarias_por_tipo", 10, 9)


### J Receitas eleitorais por tipo, cargo e ano ---------------------------------------------
analise <- "J"
for (k in names(arq_cand)) usar_bd(arq_cand[[k]], c("ano", "cargo", "soma_receitas", "soma_recursos_publicos",
                                                   "soma_recursos_pessoa_fisica", "soma_recursos_pessoa_juridica",
                                                   "soma_recursos_proprios", "soma_recursos_partidos",
                                                   "soma_recursos_outros_candidatos"))
catJ <- c(soma_recursos_partidos = "Partidos", soma_recursos_pessoa_fisica = "Pessoas físicas",
          soma_recursos_pessoa_juridica = "Pessoas jurídicas", soma_recursos_proprios = "Recursos próprios",
          soma_recursos_outros_candidatos = "Outros candidatos")
tJ <- cand[, c(list(n_candidatos = .N, total = sum(soma_receitas, na.rm = TRUE),
                    publicos = sum(soma_recursos_publicos, na.rm = TRUE)),
               lapply(.SD, function(x) sum(x, na.rm = TRUE))), by = .(cargo_rot, ano), .SDcols = names(catJ)][order(cargo_rot, ano)]
tJ[, outras := total - rowSums(.SD), .SDcols = names(catJ)]
for (v in c(names(catJ), "outras", "publicos")) tJ[, (v) := 100 * razao(get(v), total)]
setnames(tJ, c(names(catJ), "outras", "publicos"), c(paste0("pct_", sub("soma_recursos_", "", names(catJ))), "pct_outras_origens", "pct_recursos_publicos"))
tJ[, total_mi := total / 1e6][, total := NULL]
salvar_tab(tJ, "J_receitas_eleitorais_por_tipo")
gJ <- melt(tJ, id.vars = c("cargo_rot", "ano"), measure.vars = c(paste0("pct_", sub("soma_recursos_", "", names(catJ))), "pct_outras_origens"),
           variable.name = "tipo", value.name = "pct")
gJ[, tipo := factor(tipo, labels = c(catJ, "Outras origens"))]
pJ <- ggplot(gJ, aes(factor(ano), pct, fill = tipo)) + geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  geom_point(data = tJ, aes(factor(ano), pct_recursos_publicos, shape = "Recursos públicos (FEFC/FP)"),
             inherit.aes = FALSE, size = 2.2, colour = "grey15") +
  facet_wrap(~cargo_rot, scales = "free_x") + scale_fill_manual(values = cores[1:6], name = NULL) +
  scale_shape_manual(values = 18, name = NULL) +
  labs(x = "Eleição", y = "% das receitas de campanha", title = "Origem das receitas de campanha por cargo e ano",
       caption = paste(fonte_txt, "Bancos: bd_candidatos_*. 2026: contas ainda parciais."))
salvar_fig(pJ, "J_receitas_eleitorais_por_tipo", 11, 7)


### K Receitas partidárias por tipo, esfera e ano -------------------------------------------
analise <- "K"
for (a in c("bd_diretorios_nacional.xlsx", "bd_diretorios_estadual.xlsx", "bd_diretorios_municipal.csv"))
  usar_bd(a, c("ANO", vars_rec))
catK <- c(rec_valor_origem_cota_fundo_part = "Cota do Fundo Partidário", rec_valor_origem_cota_fefc = "Cota do FEFC",
          rec_valor_origem_partidos_politicos = "Partidos políticos", rec_valor_origem_candidatos = "Candidaturas",
          rec_valor_origem_pessoas_fisicas = "Pessoas físicas", rec_valor_origem_pessoas_juridicas = "Pessoas jurídicas",
          rec_valor_origem_nao_identificada = "Não identificada")
tK <- esferas[ANO >= 2017, c(list(n_orgaos_com_receita = sum(rec_valor_total_bruto > 0, na.rm = TRUE),
                                 total = sum(rec_valor_total_bruto, na.rm = TRUE)),
                            lapply(.SD, function(x) sum(x, na.rm = TRUE))),
              by = .(esfera, ANO), .SDcols = c(names(catK), "rec_valor_recursos_publicos", "rec_valor_recursos_intrapartidarios", "rec_valor_recursos_privados")]
for (v in c(names(catK), "rec_valor_recursos_publicos", "rec_valor_recursos_intrapartidarios", "rec_valor_recursos_privados"))
  tK[, (v) := 100 * razao(get(v), total)]
setnames(tK, names(tK), sub("^rec_valor_", "pct_", names(tK)))
tK[, total_mi := total / 1e6][, total := NULL]
salvar_tab(tK[order(esfera, ANO)], "K_receitas_partidarias_por_tipo")
gK <- melt(tK, id.vars = c("esfera", "ANO"), measure.vars = sub("^rec_valor_", "pct_", names(catK)), variable.name = "tipo", value.name = "pct")
gK[, tipo := factor(tipo, levels = sub("^rec_valor_", "pct_", names(catK)), labels = catK)]
pK <- ggplot(gK, aes(factor(ANO), pct, fill = tipo)) + geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
  facet_wrap(~esfera, ncol = 1) + scale_fill_manual(values = cores, name = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "Ano", y = "% das receitas (total bruto)", title = "Origem das receitas partidárias por esfera e ano",
       caption = paste(fonte_txt, "Bancos: bd_diretorios_*. 2025 incompleto."))
salvar_fig(pK, "K_receitas_partidarias_por_tipo", 10, 9)


### L Despesas partidárias e ideologia ---------------------------------------------------
analise <- "L"
usar_bd("bd_diretorios_nacional.xlsx", c("ANO", "SG_PARTIDO_PAD", "bls_ideologia", "bolognesi_categoria", vars_desp))
usar_bd("bd_diretorios_estadual.xlsx", c("ANO", "SG_PARTIDO_PAD", vars_desp))
usar_bd("bd_diretorios_municipal.csv", c("ANO", "SG_PARTIDO_PAD", vars_desp))
# Partido-ano em cada esfera (soma dos órgãos), com a ideologia nacional do partido
dL <- esferas[ANO >= 2017, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = .(esfera, ANO, SG_PARTIDO_PAD), .SDcols = vars_desp]
dL <- dL[desp_valor_total_bruto > 0]
dL[, `:=`(pct_aparato = 100 * (desp_valor_aparato_pessoal + desp_valor_aparato_sedes + desp_valor_aparato_politica) / desp_valor_total_bruto,
          pct_eleicoes = 100 * (desp_valor_eleicoes_comunicacao + desp_valor_eleicoes_publicidade) / desp_valor_total_bruto,
          pct_transferencias = 100 * desp_valor_transferencias / desp_valor_total_bruto,
          pct_pessoal = 100 * desp_valor_aparato_pessoal / desp_valor_total_bruto)]
dL <- merge(dL, dir_nac[, .(ANO, SG_PARTIDO_PAD, bls_ideologia, bolognesi_categoria)], by = c("ANO", "SG_PARTIDO_PAD"))
catL <- c(pct_aparato = "Aparato", pct_pessoal = "Aparato: pessoal", pct_eleicoes = "Eleições", pct_transferencias = "Transferências")
lL <- melt(dL, id.vars = c("esfera", "ANO", "SG_PARTIDO_PAD", "bls_ideologia", "bolognesi_categoria"),
           measure.vars = names(catL), variable.name = "tipo", value.name = "pct")
lL[, tipo := factor(catL[as.character(tipo)], levels = catL)]
tL1 <- lL[, correlacoes(bls_ideologia, pct), by = .(esfera, tipo)][order(esfera, tipo)]
salvar_tab(tL1, "L_correlacao_ideologia_bls_despesas")
lL[, bloco := factor(tolower(bolognesi_categoria))]
tL2 <- lL[!is.na(bloco), .(media_pct = mean(pct), n_partido_ano = .N), by = .(esfera, tipo, bloco)]
salvar_tab(dcast(tL2, esfera + tipo ~ bloco, value.var = "media_pct"), "L_despesas_por_bloco_ideologico")
pL <- ggplot(lL[!is.na(bls_ideologia)], aes(bls_ideologia, pct)) +
  geom_point(alpha = 0.35, size = 1.3, colour = cores[1]) +
  geom_smooth(method = "lm", se = FALSE, colour = cores[2], linewidth = 0.7, formula = y ~ x) +
  facet_grid(tipo ~ esfera) +
  labs(x = "Ideologia do partido (BLS; -1 esquerda a 1 direita)", y = "% das despesas do partido na esfera",
       title = "Ideologia e composição das despesas partidárias", subtitle = "Partido-ano, 2017–2025",
       caption = paste(fonte_txt, "Bancos: bd_diretorios_*."))
salvar_fig(pL, "L_ideologia_despesas_partidarias", 10, 9)


### M Dependência das direções estaduais -------------------------------------------------
analise <- "M"
usar_bd("bd_diretorios_estadual.xlsx", c("ANO", "SG_PARTIDO_PAD", "SG_UF", "rec_valor_total_bruto",
                                         "rec_valor_origem_partidos_politicos", "rec_valor_recursos_intrapartidarios",
                                         "rec_valor_origem_cota_fundo_part", "rec_valor_origem_cota_fefc"))
dM <- dir_est[ANO >= 2017 & rec_valor_total_bruto > 0,
              .(ANO, SG_PARTIDO_PAD, SG_UF, rec = as.numeric(rec_valor_total_bruto),
                partidos = as.numeric(rec_valor_origem_partidos_politicos),
                intrap = as.numeric(rec_valor_recursos_intrapartidarios))]
dM[, `:=`(dep_partidos = 100 * partidos / rec, dep_intrap = 100 * intrap / rec)]
dM[, ano_tipo := fifelse(ANO %in% c(2018L, 2020L, 2022L, 2024L), "Ano eleitoral", "Ano não eleitoral")]
tM1 <- dM[, .(n_diretorios = .N, media = mean(dep_partidos), mediana = median(dep_partidos),
              pct_acima_50 = 100 * mean(dep_partidos > 50), pct_acima_90 = 100 * mean(dep_partidos > 90),
              agregado = 100 * sum(partidos) / sum(rec), media_intrapartidario = mean(dep_intrap)), by = ANO][order(ANO)]
salvar_tab(tM1, "M_dependencia_estaduais_por_ano")
tM2 <- dM[, .(n_diretorio_ano = .N, mediana = median(dep_partidos), agregado = 100 * sum(partidos) / sum(rec),
              receita_mi = sum(rec) / 1e6), by = SG_PARTIDO_PAD][n_diretorio_ano >= 20][order(-mediana)]
salvar_tab(tM2, "M_dependencia_estaduais_por_partido")
tM3 <- dM[, .(n = .N, mediana = median(dep_partidos), agregado = 100 * sum(partidos) / sum(rec)), by = SG_UF][order(-mediana)]
salvar_tab(tM3, "M_dependencia_estaduais_por_uf")
pM <- ggplot(dM, aes(factor(ANO), dep_partidos, fill = ano_tipo)) +
  geom_boxplot(width = 0.6, outlier.size = 0.6, outlier.alpha = 0.4, linewidth = 0.3) +
  scale_fill_manual(values = cores[c(1, 4)], name = NULL) +
  labs(x = "Ano", y = "% da receita vinda de outros órgãos partidários",
       title = "Dependência das direções estaduais de repasses partidários",
       subtitle = "Diretórios estaduais com receita no ano", caption = paste(fonte_txt, "Banco: bd_diretorios_estadual."))
salvar_fig(pM, "M_dependencia_estaduais_por_ano", 9, 5.5)


### N e O Recursos da direção e bancada eleita ---------------------------------------------
recursos_bancada <- function(d, chave, cargo_txt) {
  x <- d[!is.na(diretorio_rec_valor_total_bruto), c("ano", "SG_PARTIDO_PAD", chave, "pct_cadeiras",
                                                   "pct_votos_nominais_distrito", "diretorio_rec_valor_total_bruto"), with = FALSE]
  x[, rec := as.numeric(diretorio_rec_valor_total_bruto)]
  x[, pct_recursos := 100 * razao(rec, sum(rec)), by = c("ano", chave)]
  x <- x[!is.na(pct_recursos)]
  x[, cargo := cargo_txt]
  x
}
analise <- "N"
usar_bd("bd_partidos_assembleias.xlsx", c("ano", "SG_PARTIDO_PAD", "sigla_uf", "pct_cadeiras", "pct_votos_nominais_distrito",
                                           "diretorio_rec_valor_total_bruto"))
dN <- recursos_bancada(part_ass[ano < 2026L], "sigla_uf", "Dep. estadual/distrital")
analise <- "O"
usar_bd("bd_partidos_vereanca.csv", c("ano", "SG_PARTIDO_PAD", "id_municipio_tse", "pct_cadeiras", "pct_votos_nominais_distrito",
                                       "diretorio_rec_valor_total_bruto"))
dO <- recursos_bancada(part_ver, "CD_MUNICIPIO", "Vereador")
for (z in list(list(dN, "N", "sigla_uf"), list(dO, "O", "CD_MUNICIPIO"))) {
  x <- z[[1]]; id <- z[[2]]
  t1 <- x[, c(correlacoes(pct_cadeiras, pct_recursos), list(n_distritos = uniqueN(get(z[[3]])))), by = ano][order(ano)]
  t1p <- x[, c(correlacoes(pct_cadeiras, pct_recursos), list(n_distritos = uniqueN(paste(ano, get(z[[3]])))))]
  t1 <- rbind(t1[, ano := as.character(ano)], cbind(ano = "todos", t1p))
  m <- feols(as.formula(paste("pct_recursos ~ pct_cadeiras |", z[[3]], "^ano")), x, cluster = as.formula(paste("~", z[[3]])))
  t1[, `:=`(coef_ef_distrito_ano = c(rep(NA, .N - 1), coef(m)[["pct_cadeiras"]]),
            ep = c(rep(NA, .N - 1), se(m)[["pct_cadeiras"]]))]
  salvar_tab(t1, paste0(id, "_recursos_direcao_e_bancada"))
  x[, faixa := cut(pct_cadeiras, c(-Inf, 0, 10, 20, 30, 50, Inf), labels = c("0", "0-10", "10-20", "20-30", "30-50", "50+"))]
  t2 <- x[!is.na(faixa), .(n = .N, media_pct_recursos = mean(pct_recursos), mediana_pct_recursos = median(pct_recursos)), by = .(ano, faixa)][order(ano, faixa)]
  salvar_tab(t2, paste0(id, "_recursos_por_faixa_de_bancada"))
  p <- ggplot(x, aes(pct_cadeiras, pct_recursos)) +
    geom_point(alpha = if (id == "O") 0.08 else 0.5, size = 1.2, colour = cores[1]) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    geom_smooth(method = "lm", se = FALSE, colour = cores[2], linewidth = 0.7, formula = y ~ x) +
    facet_wrap(~ano) + coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
    labs(x = "% das cadeiras do distrito obtidas pelo partido", y = "% das receitas dos diretórios do distrito",
         title = paste0("Bancada eleita e recursos da direção partidária (", x$cargo[1], ")"),
         subtitle = "Receitas do diretório no ano da eleição; linha tracejada = proporcionalidade",
         caption = paste(fonte_txt, if (id == "N") "Banco: bd_partidos_assembleias." else "Banco: bd_partidos_vereanca."))
  salvar_fig(p, paste0(id, "_recursos_direcao_e_bancada"), 10, 5.5)
}


### Dirigentes no ano da eleição (para P e R) ------------------------------------------
# Títulos eleitorais dos dirigentes de cada partido em cada ano eleitoral (2016-2024),
# em qualquer esfera; cargo_decisao = presidência ou tesouraria
analise <- "P"
v_dir <- c("ANO", "SG_PARTIDO_PAD", "NR_TITULO_ELEITORAL_MEMBRO", "cargo_decisao")
dirig <- rbind(
  ler_bd("bd_dirigentes_nacional.xlsx", v_dir)[, esfera := "Nacional"],
  ler_bd("bd_dirigentes_estadual.xlsx", v_dir)[, esfera := "Estadual"],
  rbindlist(lapply(c(2016, 2018, 2020, 2022, 2024), function(a)
    ler_bd(file.path("bd_dirigentes_municipal", paste0("bd_dirigentes_municipal_", a, ".csv")), v_dir)))[, esfera := "Municipal"]
)
dirig[, `:=`(ano = as.integer(ANO), titulo_eleitoral = titulo_norm(NR_TITULO_ELEITORAL_MEMBRO),
             cargo_decisao = as.integer(cargo_decisao))]
dirig <- dirig[ano %in% c(2016L, 2018L, 2020L, 2022L, 2024L) & !is.na(titulo_eleitoral),
               .(decisao = max(cargo_decisao, na.rm = TRUE), esferas = paste(sort(unique(esfera)), collapse = "+")),
               by = .(ano, SG_PARTIDO_PAD, titulo_eleitoral)]
cand_d <- merge(cand[ano %in% c(2016L, 2018L, 2020L, 2022L, 2024L)], dirig, by = c("ano", "SG_PARTIDO_PAD", "titulo_eleitoral"), all.x = TRUE)
cand_d[, dirigente := factor(fcase(is.na(decisao), "Não dirigente", decisao == 1, "Dirigente (presidência/tesouraria)",
                                   default = "Dirigente (outros cargos)"),
                             levels = c("Não dirigente", "Dirigente (outros cargos)", "Dirigente (presidência/tesouraria)"))]
cand_d[, eh_dirigente := as.integer(dirigente != "Não dirigente")]


### P Dirigentes candidatos ---------------------------------------------------------------
for (k in names(arq_cand)) usar_bd(arq_cand[[k]], c("ano", "cargo", "SG_PARTIDO_PAD", "titulo_eleitoral", "eleito_final"))
for (a in c("bd_partidos_vereanca.csv", "bd_partidos_camara.xlsx", "bd_partidos_assembleias.xlsx"))
  usar_bd(a, c("ano", "n_candidatos", "n_eleitos", "n_dirigentes_candidatos", "n_dirigentes_decisao_candidatos", "n_dirigentes_eleitos"))

# (a) cruzamento candidato-dirigente pelo título, mesmo partido, qualquer esfera
tP1 <- cand_d[, .(candidatos = .N, dirigentes = sum(eh_dirigente),
                  pct_dirigentes = 100 * mean(eh_dirigente),
                  pct_dirigentes_decisao = 100 * mean(dirigente == "Dirigente (presidência/tesouraria)"),
                  eleitos = sum(eleito_final == 1, na.rm = TRUE),
                  pct_dirigentes_entre_eleitos = 100 * mean(eh_dirigente[eleito_final %in% 1L]),
                  taxa_eleicao_dirigentes = 100 * mean(eleito_final[eh_dirigente == 1] == 1, na.rm = TRUE),
                  taxa_eleicao_nao_dirigentes = 100 * mean(eleito_final[eh_dirigente == 0] == 1, na.rm = TRUE)),
              by = .(cargo_rot, ano)][order(cargo_rot, ano)]
salvar_tab(tP1, "P_dirigentes_candidatos_por_cargo_ano")
# (b) contagens dos bancos de listas (dirigentes do diretório do próprio distrito)
tP2 <- rbind(part_ver[ano >= 2016L, .(cargo_rot = "Vereador", ano, n_candidatos, n_eleitos, n_dirigentes_candidatos,
                                     n_dirigentes_decisao_candidatos, n_dirigentes_eleitos)],
             part_ass[ano >= 2018L & ano < 2026L, .(cargo_rot = "Dep. estadual/distrital", ano, n_candidatos, n_eleitos,
                                                    n_dirigentes_candidatos, n_dirigentes_decisao_candidatos, n_dirigentes_eleitos)],
             part_cam[ano >= 2018L & ano < 2026L, .(cargo_rot = "Dep. federal", ano, n_candidatos, n_eleitos,
                                                    n_dirigentes_candidatos, n_dirigentes_decisao_candidatos, n_dirigentes_eleitos)])
tP2 <- tP2[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)), by = .(cargo_rot, ano)]
tP2[, `:=`(pct_dirigentes = 100 * n_dirigentes_candidatos / n_candidatos,
           pct_dirigentes_entre_eleitos = 100 * n_dirigentes_eleitos / n_eleitos)]
salvar_tab(tP2, "P_dirigentes_candidatos_bancos_listas")
pP <- ggplot(tP1, aes(factor(ano), pct_dirigentes, fill = cargo_rot)) +
  geom_col(width = 0.7) + geom_text(aes(label = sprintf("%.1f%%", pct_dirigentes)), vjust = -0.4, size = 3) +
  facet_wrap(~cargo_rot, scales = "free_x") + scale_fill_manual(values = cores[1:6], guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Eleição", y = "% dos candidatos que eram dirigentes do partido no ano",
       title = "Candidatos que eram dirigentes partidários", subtitle = "Dirigente em qualquer esfera do próprio partido",
       caption = paste(fonte_txt, "Bancos: bd_candidatos_* e bd_dirigentes_*."))
salvar_fig(pP, "P_dirigentes_candidatos_por_cargo_ano", 10, 6)


### Q Gasto com internet e votos ----------------------------------------------------------
analise <- "Q"
for (k in names(arq_cand)) usar_bd(arq_cand[[k]], c("ano", "cargo", "sigla_uf", if (k %in% c("vereanca", "prefeitura")) "id_municipio",
                                                   "despesas_internet_can", "pct_votos_distrito"))
dQ <- cand[, .(banco, cargo_rot, ano, distrito, internet = fcoalesce(despesas_internet_can, 0), pct_votos_distrito)]
dQ[, tot_internet := sum(internet), by = .(banco, ano, distrito)]
dQ <- dQ[tot_internet > 0 & !is.na(pct_votos_distrito)]
dQ[, pct_internet := 100 * internet / tot_internet]
tQ <- dQ[, c(correlacoes(pct_internet, pct_votos_distrito),
             list(n_distritos = uniqueN(distrito), pct_cand_com_gasto = 100 * mean(internet > 0))), by = .(cargo_rot, ano)][order(cargo_rot, ano)]
salvar_tab(tQ, "Q_internet_e_votos_correlacoes")
# médias por faixa de participação no gasto com internet
dQ[, faixa := cut(pct_internet, c(-Inf, 0, 1, 5, 10, 25, 50, Inf), labels = c("0", "0-1", "1-5", "5-10", "10-25", "25-50", "50+"))]
tQ2 <- dQ[, .(n = .N, media_pct_votos = mean(pct_votos_distrito)), by = .(cargo_rot, ano, faixa)][order(cargo_rot, ano, faixa)]
salvar_tab(tQ2, "Q_votos_por_faixa_de_gasto_internet")
pQ <- ggplot(tQ2[n >= 10], aes(faixa, media_pct_votos, colour = factor(ano), group = factor(ano))) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  facet_wrap(~cargo_rot, scales = "free_y") + scale_colour_manual(values = cores, name = "Eleição") +
  labs(x = "% do gasto com internet do distrito feito pelo candidato", y = "% de votos no distrito (média)",
       title = "Gasto com internet e votação por cargo e ano",
       caption = paste(fonte_txt, "Bancos: bd_candidatos_*. Só distritos com algum gasto com internet."))
salvar_fig(pQ, "Q_internet_e_votos", 11, 7)


### R Ser dirigente e acesso a recursos ------------------------------------------------
analise <- "R"
for (k in names(arq_cand)) usar_bd(arq_cand[[k]], c("ano", "cargo", "SG_PARTIDO_PAD", "titulo_eleitoral", "mulher", "incumbente",
                                                   "soma_receitas", "soma_recursos_partidos"))
usar_bd("bd_dirigentes_nacional.xlsx", v_dir); usar_bd("bd_dirigentes_estadual.xlsx", v_dir)
usar_bd("bd_dirigentes_municipal/bd_dirigentes_municipal_<ano>.csv", v_dir)
dR <- cand_d[, .(cargo_rot, ano, distrito, SG_PARTIDO_PAD, dirigente, eh_dirigente,
                 mulher = as.integer(mulher), incumbente = as.integer(incumbente),
                 receitas = fcoalesce(soma_receitas, 0), partidarios = fcoalesce(soma_recursos_partidos, 0))]
tR1 <- dR[, .(n = .N, media_receitas = mean(receitas), mediana_receitas = median(receitas),
              media_partidarios = mean(partidarios), mediana_partidarios = median(partidarios),
              pct_recebeu_partidarios = 100 * mean(partidarios > 0)), by = .(cargo_rot, ano, dirigente)][order(cargo_rot, ano, dirigente)]
salvar_tab(tR1, "R_recursos_dirigentes_e_nao_dirigentes")
# Diferença ajustada: log(1 + R$) ~ dirigente + mulher + incumbente, com efeitos fixos
# de partido-distrito nos cargos proporcionais (comparação dentro da mesma lista) e de
# distrito nos majoritários (cada partido tem um só candidato no distrito)
majoritarios <- c("Prefeito", "Senador", "Governador")
tR2 <- rbindlist(lapply(c(receitas = "receitas", partidarios = "partidarios"), function(y) {
  dR[, {
    ef <- if (as.character(cargo_rot) %in% majoritarios) "distrito" else "SG_PARTIDO_PAD^distrito"
    m <- feols_s(as.formula(paste0("log1p(", y, ") ~ eh_dirigente + mulher + incumbente | ", ef)),
               .SD, cluster = ~distrito)
    ct <- coef_tab(m, "eh_dirigente")
    ct[, dif_pct_aprox := 100 * (exp(coef) - 1)]
    ct
  }, by = .(cargo_rot, ano)][, recurso := y]
}))
salvar_tab(tR2[order(recurso, cargo_rot, ano)], "R_regressao_dirigente_recursos")
gR <- tR1[, .(cargo_rot, ano, dirigente, mediana_receitas, media_partidarios)]
pR <- ggplot(tR1, aes(factor(ano), media_partidarios / 1e3, fill = dirigente)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~cargo_rot, scales = "free") + scale_fill_manual(values = cores[c(1, 2, 3)], name = NULL) +
  labs(x = "Eleição", y = "Recursos de partidos recebidos (média, R$ mil de dez/2025)",
       title = "Dirigentes partidários e acesso a recursos de partidos na campanha",
       caption = paste(fonte_txt, "Bancos: bd_candidatos_* e bd_dirigentes_*."))
salvar_fig(pR, "R_recursos_partidarios_dirigentes", 11, 7)
pR2 <- ggplot(tR1, aes(factor(ano), media_receitas / 1e3, fill = dirigente)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~cargo_rot, scales = "free") + scale_fill_manual(values = cores[c(1, 2, 3)], name = NULL) +
  labs(x = "Eleição", y = "Receitas de campanha (média, R$ mil de dez/2025)",
       title = "Dirigentes partidários e receitas totais de campanha",
       caption = paste(fonte_txt, "Bancos: bd_candidatos_* e bd_dirigentes_*."))
salvar_fig(pR2, "R_receitas_totais_dirigentes", 11, 7)


### S Rotatividade das direções após as eleições ------------------------------------------
# Rotatividade (turnover) do ano t: proporção de membros que entraram ou saíram da
# direção entre t-1 e t (variáveis turnover_* dos bancos de diretórios; NA em 2015,
# primeiro ano). Os anos são classificados pelo ciclo eleitoral: o ano seguinte a
# uma eleição municipal (2017, 2021, 2025) ou geral (2015, 2019, 2023), os anos
# eleitorais (municipais: 2016, 2020, 2024; gerais: 2018, 2022).
analise <- "S"
for (a in c("bd_diretorios_nacional.xlsx", "bd_diretorios_estadual.xlsx", "bd_diretorios_municipal.csv"))
  usar_bd(a, c("ANO", "SG_PARTIDO_PAD", vars_turn))
usar_bd("bd_partidos_vereanca.csv", c("ano", "SG_PARTIDO_PAD", "id_municipio_tse", "n_eleitos"))
usar_bd("bd_partidos_assembleias.xlsx", c("ano", "SG_PARTIDO_PAD", "sigla_uf", "n_eleitos"))

tipo_ano <- function(a) fcase(a %in% c(2017L, 2021L, 2025L), "Pós-eleição municipal",
                              a %in% c(2015L, 2019L, 2023L), "Pós-eleição geral",
                              a %in% c(2016L, 2020L, 2024L), "Ano de eleição municipal",
                              a %in% c(2018L, 2022L), "Ano de eleição geral")
vS <- c("turnover_prop", "turnover_prop_decisao", "turnover_prop_mulheres", "turnover_prop_homens")
dS <- rbind(cbind(esfera = "Nacional", dir_nac[, c("ANO", "SG_PARTIDO_PAD", vars_turn), with = FALSE], distrito = "BR"),
            cbind(esfera = "Estadual", dir_est[, c("ANO", "SG_PARTIDO_PAD", vars_turn), with = FALSE], distrito = dir_est$SG_UF),
            cbind(esfera = "Municipal", dir_mun[, c("ANO", "SG_PARTIDO_PAD", vars_turn), with = FALSE],
                  distrito = as.character(dir_mun$CD_MUNICIPIO)))
dS[, esfera := factor(esfera, levels = c("Nacional", "Estadual", "Municipal"))]
for (v in vars_turn) set(dS, j = v, value = as.numeric(dS[[v]]))
# só órgãos que existiam no ano anterior (rotatividade definida)
dS <- dS[!is.na(turnover_prop) & n_membros_t1 > 0]
dS[, tipo := factor(tipo_ano(ANO), levels = c("Pós-eleição municipal", "Pós-eleição geral",
                                              "Ano de eleição municipal", "Ano de eleição geral"))]

tS1 <- dS[, c(list(n_orgaos = .N), lapply(.SD, function(x) 100 * mean(x, na.rm = TRUE))),
          by = .(esfera, ANO, tipo), .SDcols = vS][order(esfera, ANO)]
salvar_tab(tS1, "S_rotatividade_por_esfera_e_ano")
tS2 <- dS[, c(list(n_orgao_ano = .N), lapply(.SD, function(x) 100 * mean(x, na.rm = TRUE))),
          by = .(esfera, tipo), .SDcols = vS][order(esfera, tipo)]
salvar_tab(tS2, "S_rotatividade_por_esfera_e_ciclo")

# Desempenho eleitoral e rotatividade no ano seguinte à eleição do próprio nível:
# municipal (vereança 2016, 2020, 2024 -> direção 2017, 2021, 2025) e estadual
# (Assembleia 2018, 2022 -> direção 2019, 2023)
pos_m <- merge(dS[esfera == "Municipal" & ANO %in% c(2017L, 2021L, 2025L)][, CD_MUNICIPIO := as.integer(distrito)],
               part_ver[, .(ANO = ano + 1L, SG_PARTIDO_PAD, CD_MUNICIPIO, n_eleitos)],
               by = c("ANO", "SG_PARTIDO_PAD", "CD_MUNICIPIO"), all.x = TRUE)[, CD_MUNICIPIO := NULL]
pos_e <- merge(dS[esfera == "Estadual" & ANO %in% c(2019L, 2023L)],
               part_ass[, .(ANO = ano + 1L, SG_PARTIDO_PAD, distrito = sigla_uf, n_eleitos)],
               by = c("ANO", "SG_PARTIDO_PAD", "distrito"), all.x = TRUE)
pos <- rbind(pos_m, pos_e, use.names = TRUE)
pos[, resultado := factor(fcase(is.na(n_eleitos), "Sem lista na eleição", n_eleitos == 0, "Lista sem eleitos",
                                default = "Elegeu ao menos um"),
                          levels = c("Elegeu ao menos um", "Lista sem eleitos", "Sem lista na eleição"))]
tS3 <- pos[, c(list(n_orgaos = .N), lapply(.SD, function(x) 100 * mean(x, na.rm = TRUE))),
           by = .(esfera, ANO, resultado), .SDcols = c("turnover_prop", "turnover_prop_decisao")][order(esfera, ANO, resultado)]
salvar_tab(tS3, "S_rotatividade_pos_eleicao_por_resultado")
pos[, sem_eleitos := as.integer(resultado != "Elegeu ao menos um")]
tS4 <- rbindlist(lapply(c("Municipal", "Estadual"), function(es) rbindlist(lapply(c("turnover_prop", "turnover_prop_decisao"), function(y) {
  m <- feols_s(as.formula(paste(y, "~ sem_eleitos + log(n_membros_t1) | distrito^ANO")), pos[esfera == es], cluster = ~distrito)
  cbind(esfera = es, variavel = y, coef_tab(m, "sem_eleitos"))
}))))
tS4[, `:=`(coef_pp = 100 * coef, ep_pp = 100 * ep)]
salvar_tab(tS4[, .(esfera, variavel, coef_pp, ep_pp, p, n)], "S_regressao_rotatividade_sem_eleitos")

gS <- melt(tS1, id.vars = c("esfera", "ANO", "tipo"), measure.vars = c("turnover_prop", "turnover_prop_decisao"),
           variable.name = "medida", value.name = "pct")
gS[, medida := factor(medida, labels = c("Todos os membros", "Cargos de decisão"))]
pS <- ggplot(gS, aes(ANO, pct)) +
  geom_line(aes(linetype = medida), colour = "grey55", linewidth = 0.5) +
  geom_point(aes(colour = tipo, shape = medida), size = 2.8) +
  facet_wrap(~esfera, ncol = 3) +
  scale_colour_manual(values = cores[c(1, 2, 3, 4)], name = NULL) +
  scale_x_continuous(breaks = 2016:2025) +
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(title = NULL), linetype = "none") +
  labs(x = "Ano da direção", y = "Rotatividade média (% de entradas e saídas)",
       title = "Rotatividade das direções partidárias e ciclo eleitoral",
       caption = paste(fonte_txt, "Bancos: bd_diretorios_*.")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
salvar_fig(pS, "S_rotatividade_ciclo_eleitoral", 11, 5.5)


### Registro e planilha única ------------------------------------------------------------
reg <- unique(rbindlist(registro))[analise != "(leitura)"]
reg <- reg[, .(variaveis = paste(unique(unlist(strsplit(variaveis, ", "))), collapse = ", ")), by = .(analise, banco)]
salvar_tab(reg, "0_bancos_e_variaveis")
salvar_tab(data.table(banco = sort(unique(sub("_<ano>|_20[0-9]{2}$", "", reg$banco)))), "0_bancos_utilizados")

# Abas numeradas (o Excel limita o nome a 31 caracteres); a aba "indice" liga
# cada aba ao arquivo .csv correspondente
abas <- sprintf("T%02d_%s", seq_along(tabelas), substr(gsub("[^A-Za-z0-9_]", "", names(tabelas)), 1, 27))
wb <- createWorkbook()
addWorksheet(wb, "indice")
writeData(wb, "indice", data.table(aba = abas, tabela = paste0(names(tabelas), ".csv")))
for (i in seq_along(tabelas)) {
  addWorksheet(wb, abas[i])
  writeData(wb, abas[i], tabelas[[i]])
}
saveWorkbook(wb, file.path(saida, "tabelas_bd_representa_analise.xlsx"), overwrite = TRUE)
message("Concluído. Saídas em: ", saida)
