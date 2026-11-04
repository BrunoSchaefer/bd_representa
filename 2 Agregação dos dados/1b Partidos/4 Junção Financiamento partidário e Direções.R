### 4 Junção Financiamento partidário e Direções ------------------------------
# Bruno Schaefer (IESP-UERJ)
#
# Objetivo: montar bancos no nível nacional, estadual e municipal que juntam,
# para cada unidade partidária e ano, a composição da direção (código 1_x),
# as despesas (código 2_x) e as receitas (código 3_x) do partido.
#
# Unidades de análise:
#   - Nacional:  ano-partido, em dois bancos: Comissão Executiva e órgão de
#                direção (Órgão Definitivo/Provisório). Os dois órgãos
#                coexistem no mesmo ano, por isso ficam em bancos separados.
#   - Estadual:  ano-partido-UF (inclui o DF).
#   - Municipal: ano-partido-município (inclui as zonas eleitorais do DF).
#
# No TSE a Comissão Executiva só existe no nível nacional. Nos níveis estadual
# e municipal a direção é um Órgão Definitivo, um Órgão Provisório ou uma
# Comissão Interventora. Quando mais de um tipo esteve vigente na mesma
# unidade-ano (troca de provisório para definitivo, intervenção), fica o de
# maior tempo de atividade no ano (dias_atividade_ano, calculado nos códigos
# 1_x); em caso de empate, o de mais membros e, depois, a ordem definitivo >
# provisório > interventora. As variáveis n_tipos_orgao_ano e tipos_orgao_ano
# registram quantos e quais tipos estiveram vigentes.
#
# Versão dos diretórios:
#   - nacional e estadual: restritiva (dirigentes com >= 183 dias no ano), que
#     representa quem comandava o partido no ano;
#   - municipal: ampla (dirigentes com qualquer dia de exercício no ano). No
#     nível municipal as direções são curtas e trocam com frequência; com o
#     critério restritivo, muitas unidades com prestação de contas ficariam sem
#     diretório no ano.
# Para trocar a versão, mudar o nome do arquivo ("*.xlsx" restritivo,
# "*2.xlsx" amplo).
#
# Financiamento: valores deflacionados pelo IPCA de 12/2025. Os dados de
# prestação de contas cobrem 2017–2025; nos anos 2015–2016 as variáveis de
# despesa e receita ficam NA.

library(here)
library(tidyverse)
library(openxlsx)
library(data.table)

source(here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))


### Dados Dirigentes e Diretórios ---------------------------------------------
pasta_dirigentes <- pasta$prod_dirigentes

diretorios_nacional  <- read.xlsx(file.path(pasta_dirigentes, "banco_diretorios_nacional.xlsx"))
diretorios_estadual  <- read.xlsx(file.path(pasta_dirigentes, "banco_diretorios_estadual.xlsx"))
diretorios_municipal <- read.xlsx(file.path(pasta_dirigentes, "banco_diretorios_municipal2.xlsx"))  # amplo


### Dados Financiamento Partidário --------------------------------------------
pasta_financiamento <- pasta$prod_financ_partidario

despesas_nacional  <- read.xlsx(file.path(pasta_financiamento, "banco_despesas_nacional.xlsx"))
despesas_estadual  <- read.xlsx(file.path(pasta_financiamento, "banco_despesas_estadual.xlsx"))
despesas_municipal <- read.xlsx(file.path(pasta_financiamento, "banco_despesas_municipal.xlsx"))

receitas_nacional  <- read.xlsx(file.path(pasta_financiamento, "banco_receitas_nacional.xlsx"))
receitas_estadual  <- read.xlsx(file.path(pasta_financiamento, "banco_receitas_estadual.xlsx"))
receitas_municipal <- read.xlsx(file.path(pasta_financiamento, "banco_receitas_municipal.xlsx"))


### Funções de preparo --------------------------------------------------------

# Chave de município: código TSE do banco de dirigentes, comparado como número
# (os dirigentes trazem zeros à esquerda, ex.: "03050"). O código que aparece
# nas prestações de contas é outro (ex.: Acrelândia/AC = 643 nas contas e
# 01120 nos dirigentes); ele é convertido para o código dos dirigentes na
# seção municipal, pelo nome do município dentro da UF.
codigo_municipio <- function(x) as.integer(x)

# Financiamento: sigla padronizada, remove colunas descritivas e acrescenta
# prefixo às variáveis (despesas e receitas têm nomes em comum, como
# valor_total_bruto).
preparar_financiamento <- function(df, prefixo, chaves) {
  df <- df |>
    mutate(SG_PARTIDO_PAD = padronizar_sigla(SG_PARTIDO))
  if ("CD_MUNICIPIO" %in% chaves) {
    df <- df |> mutate(CD_MUNICIPIO = codigo_municipio(CD_MUNICIPIO))
  }
  df <- df |>
    select(-any_of(c("SG_PARTIDO", "NM_PARTIDO", "NM_MUNICIPIO",
                     "DS_TP_ESFERA_PARTIDARIA", "DS_TP_ESPERA_PARTIDARIA")))
  if (!"SG_UF" %in% chaves) df <- df |> select(-any_of("SG_UF"))

  # Cada unidade-ano deve aparecer uma única vez
  dup <- df |> count(across(all_of(chaves))) |> filter(n > 1)
  if (nrow(dup) > 0) {
    print(dup)
    stop("Chaves repetidas no banco de ", prefixo)
  }

  df |>
    rename_with(~ paste0(prefixo, .x), -all_of(chaves)) |>
    mutate("{prefixo}presente" := 1L)
}

# Diretórios: sigla padronizada e, quando há mais de um tipo de órgão de
# direção na mesma unidade-ano, fica o de maior tempo de atividade no ano
# (empate: mais membros; depois definitivo > provisório > interventora).
ordem_orgao <- c("ORGAO DEFINITIVO", "ORGAO PROVISORIO", "COMISSAO INTERVENTORA")

um_orgao_por_unidade <- function(df, chaves) {
  df |>
    mutate(ordem = match(NM_TIPO_ORGAO_PARTIDARIO, ordem_orgao)) |>
    group_by(across(all_of(chaves))) |>
    mutate(
      n_tipos_orgao_ano = n_distinct(NM_TIPO_ORGAO_PARTIDARIO),
      tipos_orgao_ano   = paste(sort(unique(NM_TIPO_ORGAO_PARTIDARIO)), collapse = " | ")
    ) |>
    arrange(desc(dias_atividade_ano), desc(total_membros), ordem, .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    select(-ordem)
}

# Junção e diagnóstico
juntar <- function(diretorios, despesas, receitas, chaves, rotulo) {

  base <- diretorios |>
    left_join(despesas, by = chaves) |>
    left_join(receitas, by = chaves) |>
    mutate(
      desp_presente = coalesce(desp_presente, 0L),
      rec_presente  = coalesce(rec_presente, 0L)
    )

  cat("\n===", rotulo, "===\n")
  cat("Unidades-ano:", nrow(base), "\n")
  cat("Com despesas:", sum(base$desp_presente), "| com receitas:", sum(base$rec_presente), "\n")
  print(table(ANO = base$ANO, despesa = base$desp_presente))

  # Unidades com prestação de contas e sem diretório no banco de dirigentes
  # (anos com dados dos dois lados)
  anos_comuns <- intersect(unique(diretorios$ANO), unique(despesas$ANO))
  sem_diretorio <- despesas |>
    filter(ANO %in% anos_comuns) |>
    anti_join(diretorios, by = chaves)
  cat("Unidades-ano com despesas e sem diretório:", nrow(sem_diretorio), "\n")
  print(sem_diretorio |> count(SG_PARTIDO_PAD, sort = TRUE) |> head(15))

  base
}

# Ordenar colunas: chaves, identificação, direção, despesas, receitas
ordenar <- function(df, chaves) {
  df |>
    relocate(all_of(chaves), SG_PARTIDO, NR_PARTIDO, any_of(c("NM_MUNICIPIO", "SG_UE", "NM_UE")),
             NM_TIPO_ORGAO_PARTIDARIO, any_of(c("n_tipos_orgao_ano", "tipos_orgao_ano"))) |>
    relocate(starts_with("desp_"), .after = last_col()) |>
    relocate(starts_with("rec_"), .after = last_col()) |>
    arrange(across(all_of(chaves)))
}


### Nacional ------------------------------------------------------------------

chaves_nac <- c("ANO", "SG_PARTIDO_PAD")

table(diretorios_nacional$NM_TIPO_ORGAO_PARTIDARIO)

diretorios_nacional <- diretorios_nacional |>
  mutate(SG_PARTIDO_PAD = padronizar_sigla(SG_PARTIDO))

desp_nac <- preparar_financiamento(despesas_nacional, "desp_", chaves_nac)
rec_nac  <- preparar_financiamento(receitas_nacional, "rec_",  chaves_nac)

# Comissão Executiva
nacional_comissao_executiva <- diretorios_nacional |>
  filter(NM_TIPO_ORGAO_PARTIDARIO == "COMISSAO EXECUTIVA") |>
  mutate(n_tipos_orgao_ano = 1L, tipos_orgao_ano = NM_TIPO_ORGAO_PARTIDARIO) |>
  juntar(desp_nac, rec_nac, chaves_nac, "Nacional — Comissão Executiva") |>
  ordenar(chaves_nac)

# Órgão de direção (definitivo ou provisório)
nacional_orgao_direcao <- diretorios_nacional |>
  filter(NM_TIPO_ORGAO_PARTIDARIO != "COMISSAO EXECUTIVA") |>
  um_orgao_por_unidade(chaves_nac) |>
  juntar(desp_nac, rec_nac, chaves_nac, "Nacional — Órgão de direção") |>
  ordenar(chaves_nac)


### Estadual ------------------------------------------------------------------

chaves_est <- c("ANO", "SG_PARTIDO_PAD", "SG_UF")

table(diretorios_estadual$NM_TIPO_ORGAO_PARTIDARIO)

diretorios_estadual <- diretorios_estadual |>
  mutate(SG_PARTIDO_PAD = padronizar_sigla(SG_PARTIDO))

desp_est <- preparar_financiamento(despesas_estadual, "desp_", chaves_est)
rec_est  <- preparar_financiamento(receitas_estadual, "rec_",  chaves_est)

estadual <- diretorios_estadual |>
  um_orgao_por_unidade(chaves_est) |>
  juntar(desp_est, rec_est, chaves_est, "Estadual") |>
  ordenar(chaves_est)


### Municipal -----------------------------------------------------------------

chaves_mun <- c("ANO", "SG_PARTIDO_PAD", "SG_UF", "CD_MUNICIPIO")

table(diretorios_municipal$NM_TIPO_ORGAO_PARTIDARIO)

diretorios_municipal <- diretorios_municipal |>
  mutate(
    SG_PARTIDO_PAD = padronizar_sigla(SG_PARTIDO),
    CD_MUNICIPIO   = codigo_municipio(CD_MUNICIPIO)
  )

# Correspondência de municípios -------------------------------------------------
# As prestações de contas usam um código de município diferente do código TSE
# do banco de dirigentes. A correspondência é feita pelo nome normalizado (sem
# acentos e pontuação) dentro da UF. As zonas do DF ("20. ZONA ELEITORAL")
# têm o mesmo nome nas duas fontes.

mun_dirigentes <- diretorios_municipal |>
  distinct(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO) |>
  mutate(nome = normalizar_nome(NM_MUNICIPIO)) |>
  distinct(SG_UF, nome, CD_MUNICIPIO)

# Um nome deve corresponder a um único código dentro da UF
print(mun_dirigentes |> count(SG_UF, nome) |> filter(n > 1))

# Grafias diferentes entre as duas fontes (nome nas contas -> nome nos
# dirigentes), identificadas pela menor distância de edição dentro da UF
grafias <- tribble(
  ~SG_UF, ~nome_contas,                  ~nome_dirigentes,
  "BA",   "CAMACAN",                     "CAMACA",
  "BA",   "QUINJINGUE",                  "QUIJINGUE",
  "CE",   "ITAPAGE",                     "ITAPAJE",
  "GO",   "BOM JESUS",                   "BOM JESUS DE GOIAS",
  "PB",   "SAO DOMINGOS DE POMBAL",      "SAO DOMINGOS",
  "PE",   "BELEM DE SAO FRANCISCO",      "BELEM DO SAO FRANCISCO",
  "PE",   "IGUARACI",                    "IGUARACY",
  "PR",   "MUNHOZ DE MELO",              "MUNHOZ DE MELLO",
  "RJ",   "TRAJANO DE MORAIS",           "TRAJANO DE MORAES",
  "RN",   "ARES",                        "AREZ",
  "RO",   "ALTA FLORESTA DO OESTE",      "ALTA FLORESTA D OESTE",
  "RO",   "MACHADINHO DO OESTE",         "MACHADINHO D OESTE",
  "RO",   "NOVA BRASILANDIA DO OESTE",   "NOVA BRASILANDIA D OESTE",
  "RO",   "SANTA LUZIA DO OESTE",        "SANTA LUZIA D OESTE",
  "RO",   "SAO FELIPE DO OESTE",         "SAO FELIPE D OESTE",
  "RS",   "SANTANA DO LIVRAMENTO",       "SANT ANA DO LIVRAMENTO",
  "SC",   "LUIS ALVES",                  "LUIZ ALVES",
  "SC",   "PRESIDENTE CASTELO BRANCO",   "PRESIDENTE CASTELLO BRANCO",
  "TO",   "COUTO DE MAGALHAES",          "COUTO MAGALHAES",
  "TO",   "SAO VALERIO DA NATIVIDADE",   "SAO VALERIO"
)

mun_financiamento <- bind_rows(
  despesas_municipal |> distinct(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO),
  receitas_municipal |> distinct(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO)
) |>
  mutate(CD_MUNICIPIO = as.character(CD_MUNICIPIO)) |>
  distinct() |>
  mutate(nome = normalizar_nome(NM_MUNICIPIO)) |>
  left_join(grafias, by = c("SG_UF", "nome" = "nome_contas")) |>
  mutate(nome = coalesce(nome_dirigentes, nome)) |>
  select(-nome_dirigentes)

correspondencia_municipios <- mun_financiamento |>
  left_join(
    mun_dirigentes |> distinct(SG_UF, nome, .keep_all = TRUE) |>
      rename(CD_MUNICIPIO_TSE = CD_MUNICIPIO),
    by = c("SG_UF", "nome")
  )

cat("Municípios das prestações de contas:", nrow(correspondencia_municipios),
    "| sem correspondência nos dirigentes:",
    sum(is.na(correspondencia_municipios$CD_MUNICIPIO_TSE)), "\n")
correspondencia_municipios |>
  filter(is.na(CD_MUNICIPIO_TSE)) |>
  arrange(SG_UF, NM_MUNICIPIO) |>
  as_tibble() |>
  print(n = 100)

recodificar_municipio <- function(df) {
  df |>
    mutate(CD_MUNICIPIO = as.character(CD_MUNICIPIO)) |>
    left_join(
      correspondencia_municipios |> select(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, CD_MUNICIPIO_TSE),
      by = c("SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO")
    ) |>
    filter(!is.na(CD_MUNICIPIO_TSE)) |>
    mutate(CD_MUNICIPIO = CD_MUNICIPIO_TSE) |>
    select(-CD_MUNICIPIO_TSE)
}

desp_mun <- preparar_financiamento(recodificar_municipio(despesas_municipal), "desp_", chaves_mun)
rec_mun  <- preparar_financiamento(recodificar_municipio(receitas_municipal), "rec_",  chaves_mun)

municipal <- diretorios_municipal |>
  um_orgao_por_unidade(chaves_mun) |>
  juntar(desp_mun, rec_mun, chaves_mun, "Municipal") |>
  ordenar(chaves_mun)


### Salvar bancos -------------------------------------------------------------
# O banco municipal é salvo em .csv (separador ";", UTF-8) por causa do
# tamanho (centenas de milhares de linhas e mais de 150 colunas).

pasta_saida <- pasta$juncao_partidos

write.xlsx(nacional_comissao_executiva,
           file.path(pasta_saida, "banco_nacional_comissao_executiva.xlsx"), overwrite = TRUE)
write.xlsx(nacional_orgao_direcao,
           file.path(pasta_saida, "banco_nacional_orgao_direcao.xlsx"), overwrite = TRUE)
write.xlsx(estadual,
           file.path(pasta_saida, "banco_estadual.xlsx"), overwrite = TRUE)
fwrite(municipal,
       file.path(pasta_saida, "banco_municipal.csv"), sep = ";")

cat("Bancos salvos em:", pasta_saida, "\n")
