### 13 Junção Partidos e Distritos (Etapa 2, 1b) --------------------------------
# Bruno Schaefer (IESP-UERJ)
#
# Objetivo: acrescentar aos bancos de diretórios (códigos 4 e 5_1: direção,
# financiamento partidário e características nacionais do partido) as
# informações do distrito: vagas, comparecimento e perfil do eleitorado
# (código 10) e informações socioeconômicas (código 11).
#
# Entradas: 2 Agregação dos dados/1b Partidos/bancos (saídas do 5_1) e
#           1 Pesquisas individuais/4a Produtos (Distritos, Socioeconômico)
# Saídas (2 Agregação dos dados/1b Partidos/bancos):
#   - diretorios_nacional_comissao_executiva.xlsx
#   - diretorios_nacional_orgao_direcao.xlsx
#   - diretorios_estadual.xlsx
#   - diretorios_municipal.csv
#   - distritos_brasil_ano.xlsx, distritos_uf_ano.xlsx e
#     distritos_municipio_ano.csv: as informações de distrito por ano, usadas
#     também no banco de dirigentes (código 14)
#   - distritos_brasil_eleicao.xlsx, distritos_uf_eleicao.xlsx e
#     distritos_municipio_eleicao.csv: as mesmas informações por eleição
#     (2010–2024), usadas no banco de distritos (código 16)
#
# Decisões:
#   - As informações do distrito vêm das eleições. Cada eleição vale para o
#     próprio ano e para o ano seguinte (2014 vale para 2014 e 2015; 2016 para
#     2016 e 2017). Nos demais anos as variáveis ficam NA. A variável
#     distrito_ano_eleicao informa a eleição de referência.
#   - Nível estadual e nacional: eleições gerais (2010, 2014, 2018, 2022).
#     Nível municipal: eleições municipais (2012, 2016, 2020, 2024).
#   - Variáveis que dependem do cargo (vagas, votos válidos, brancos, nulos,
#     eleitores por vaga) levam o nome do cargo: governador, senado, camara
#     (deputado federal), assembleia (deputado estadual ou distrital),
#     prefeitura e vereanca. As variáveis do eleitorado (aptos, comparecimento,
#     gênero e instrução) são comuns aos cargos e aparecem uma vez.
#   - Brasil (diretórios nacionais): soma das UFs para aptos, comparecimento,
#     eleitores e vagas; proporções ponderadas pelo eleitorado.
#   - Municípios: o código TSE do município (diretórios) é ligado ao código
#     IBGE (id_municipio) pela tabela do código 10. As zonas eleitorais do DF
#     não têm eleição municipal e ficam sem informações de distrito.

library(here)
library(tidyverse)
library(openxlsx)
library(data.table)

source(here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))

pasta_saida <- pasta$juncao_partidos

anos_gerais     <- c(2010, 2014, 2018, 2022)
anos_municipais <- c(2012, 2016, 2020, 2024)
anos_diretorios <- 2015:2025

sufixo_cargo <- c(
  "governador"         = "governador",
  "senador"            = "senado",
  "deputado federal"   = "camara",
  "deputado estadual"  = "assembleia",
  "deputado distrital" = "assembleia",
  "prefeito"           = "prefeitura",
  "vereador"           = "vereanca"
)

vars_cargo   <- c("vagas", "votos_validos", "pct_validos", "pct_brancos", "pct_nulos",
                  "eleitores_por_vaga")
vars_comuns  <- c("aptos", "comparecimento", "abstencoes", "pct_comparecimento",
                  "pct_abstencao", "total_eleitores", "eleitores_mulheres",
                  "prop_mulheres", "prop_homens", "prop_analfabeto", "prop_le_escreve",
                  "prop_fund_incompleto", "prop_fund_completo", "prop_medio_incompleto",
                  "prop_medio_completo", "prop_sup_incompleto", "prop_sup_completo")


### Funções -------------------------------------------------------------------

# Ano do banco -> eleição de referência (a do próprio ano ou a do ano anterior)
eleicao_referencia <- function(ano, anos_eleicao) {
  case_when(ano %in% anos_eleicao       ~ ano,
            (ano - 1) %in% anos_eleicao ~ ano - 1,
            TRUE                        ~ NA_real_)
}

# Uma linha por eleição-distrito: variáveis comuns + variáveis por cargo
distrito_largo <- function(df, chave) {
  df <- df |> mutate(sufixo = sufixo_cargo[cargo]) |> filter(!is.na(sufixo))

  comuns <- df |>
    arrange(across(all_of(c("ano", chave))), match(sufixo, sufixo_cargo)) |>
    distinct(across(all_of(c("ano", chave))), .keep_all = TRUE) |>
    select(all_of(c("ano", chave)), any_of(vars_comuns))

  por_cargo <- df |>
    select(all_of(c("ano", chave)), sufixo, any_of(vars_cargo)) |>
    pivot_wider(names_from = sufixo, values_from = any_of(vars_cargo),
                names_glue = "{.value}_{sufixo}")

  comuns |>
    left_join(por_cargo, by = c("ano", chave)) |>
    rename_with(~ paste0("distrito_", .x), -all_of(c("ano", chave)))
}

# Expande para os anos dos diretórios pela regra da eleição de referência
por_ano <- function(distrito, chave, anos_eleicao) {
  tibble(ANO = anos_diretorios) |>
    mutate(distrito_ano_eleicao = eleicao_referencia(ANO, anos_eleicao)) |>
    filter(!is.na(distrito_ano_eleicao)) |>
    inner_join(distrito, by = c("distrito_ano_eleicao" = "ano"), relationship = "many-to-many") |>
    relocate(all_of(chave), .after = ANO)
}


### Dados de distrito (código 10) ----------------------------------------------

base_uf  <- read.xlsx(file.path(pasta$prod_distritos, "base_uf.xlsx")) |> as_tibble()
base_mun <- readRDS(file.path(pasta$prod_distritos, "base_mun.rds")) |>
  as_tibble() |>
  mutate(id_municipio = as.character(id_municipio),
         CD_MUNICIPIO = as.integer(id_municipio_tse))

# UF
distrito_uf <- distrito_largo(base_uf, "sigla_uf")

# Brasil: soma das UFs (vagas e votos por cargo; eleitorado uma vez)
distrito_br <- base_uf |>
  mutate(sufixo = sufixo_cargo[cargo]) |>
  filter(!is.na(sufixo)) |>
  group_by(ano, sufixo) |>
  summarise(vagas = sum(vagas), votos_validos = sum(votos_validos),
            aptos = sum(aptos), comparecimento = sum(comparecimento),
            total_eleitores = sum(total_eleitores),
            eleitores_mulheres = sum(eleitores_mulheres), .groups = "drop") |>
  mutate(prop_mulheres = eleitores_mulheres / total_eleitores,
         pct_comparecimento = 100 * comparecimento / aptos)
distrito_br <- distrito_br |>
  filter(sufixo == "camara") |>
  select(ano, aptos, comparecimento, pct_comparecimento, total_eleitores,
         eleitores_mulheres, prop_mulheres) |>
  left_join(distrito_br |> select(ano, sufixo, vagas, votos_validos) |>
              pivot_wider(names_from = sufixo, values_from = c(vagas, votos_validos),
                          names_glue = "{.value}_{sufixo}"),
            by = "ano") |>
  rename_with(~ paste0("distrito_", .x), -ano)

# Municípios (chave: código TSE, como nos diretórios; mantém o código IBGE)
distrito_mun <- distrito_largo(base_mun, c("CD_MUNICIPIO", "id_municipio")) |>
  filter(!is.na(CD_MUNICIPIO))

# Por eleição (usadas no banco de distritos, código 16)
write.xlsx(distrito_br, file.path(pasta_saida, "distritos_brasil_eleicao.xlsx"), overwrite = TRUE)
write.xlsx(distrito_uf, file.path(pasta_saida, "distritos_uf_eleicao.xlsx"), overwrite = TRUE)
fwrite(distrito_mun, file.path(pasta_saida, "distritos_municipio_eleicao.csv"), sep = ";")

# Por ano (regra da eleição de referência)
distrito_br_ano  <- por_ano(distrito_br,  character(0),                    anos_gerais)
distrito_uf_ano  <- por_ano(distrito_uf,  "sigla_uf",                      anos_gerais)
distrito_mun_ano <- por_ano(distrito_mun, c("CD_MUNICIPIO", "id_municipio"), anos_municipais)

# Socioeconômico (código 11)
socio_uf  <- file.path(pasta$prod_socioeconomico, "socioeconomico_uf.csv")
socio_mun <- file.path(pasta$prod_socioeconomico, "socioeconomico_municipio.csv")
if (file.exists(socio_uf)) {
  distrito_uf_ano <- distrito_uf_ano |>
    left_join(ler_banco(socio_uf) |> rename(ANO = ano), by = c("ANO", "sigla_uf"))
} else message("Socioeconômico por UF ainda não disponível (código 11).")
if (file.exists(socio_mun)) {
  distrito_mun_ano <- distrito_mun_ano |>
    left_join(ler_banco(socio_mun) |> rename(ANO = ano) |> mutate(id_municipio = as.character(id_municipio)),
              by = c("ANO", "id_municipio"))
} else message("Socioeconômico por município ainda não disponível (código 11).")

# Uma linha por ano-distrito
stopifnot(nrow(count(distrito_uf_ano, ANO, sigla_uf) |> filter(n > 1)) == 0)
stopifnot(nrow(count(distrito_mun_ano, ANO, CD_MUNICIPIO) |> filter(n > 1)) == 0)

write.xlsx(distrito_br_ano, file.path(pasta_saida, "distritos_brasil_ano.xlsx"), overwrite = TRUE)
write.xlsx(distrito_uf_ano, file.path(pasta_saida, "distritos_uf_ano.xlsx"), overwrite = TRUE)
fwrite(distrito_mun_ano, file.path(pasta_saida, "distritos_municipio_ano.csv"), sep = ";")


### Junção com os diretórios ----------------------------------------------------

juntar <- function(dir, dist, chaves, rotulo) {
  base <- dir |>
    mutate(ANO = as.integer(ANO)) |>
    left_join(dist |> mutate(ANO = as.integer(ANO)), by = chaves, relationship = "many-to-one")
  stopifnot(nrow(base) == nrow(dir))
  cat("\n===", rotulo, "===\n")
  base |>
    group_by(ANO) |>
    summarise(unidades = n(),
              com_distrito = round(100 * mean(!is.na(distrito_ano_eleicao)), 1),
              .groups = "drop") |>
    print(n = Inf)
  base
}

nac_ce <- read.xlsx(file.path(pasta_saida, "banco_partidos_nacional_comissao_executiva.xlsx")) |>
  as_tibble() |> juntar(distrito_br_ano, "ANO", "Nacional — Comissão Executiva")
nac_od <- read.xlsx(file.path(pasta_saida, "banco_partidos_nacional_orgao_direcao.xlsx")) |>
  as_tibble() |> juntar(distrito_br_ano, "ANO", "Nacional — Órgão de direção")
est    <- read.xlsx(file.path(pasta_saida, "banco_partidos_estadual.xlsx")) |>
  as_tibble() |> juntar(distrito_uf_ano, c("ANO", "SG_UF" = "sigla_uf"), "Estadual")
mun    <- ler_banco(file.path(pasta_saida, "banco_partidos_municipal.csv")) |>
  as_tibble() |>
  mutate(CD_MUNICIPIO = as.integer(CD_MUNICIPIO)) |>
  juntar(distrito_mun_ano, c("ANO", "CD_MUNICIPIO"), "Municipal") |>
  relocate(id_municipio, .after = CD_MUNICIPIO)


### Salvar --------------------------------------------------------------------

write.xlsx(nac_ce, file.path(pasta_saida, "diretorios_nacional_comissao_executiva.xlsx"), overwrite = TRUE)
write.xlsx(nac_od, file.path(pasta_saida, "diretorios_nacional_orgao_direcao.xlsx"), overwrite = TRUE)
write.xlsx(est,    file.path(pasta_saida, "diretorios_estadual.xlsx"), overwrite = TRUE)
fwrite(mun,        file.path(pasta_saida, "diretorios_municipal.csv"), sep = ";")

cat("Bancos salvos em:", pasta_saida, "\n")
