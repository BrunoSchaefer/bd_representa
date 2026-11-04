# Pastas do projeto bd_representa ---------------------------------------------
# Bruno Schaefer (IESP-UERJ)
#
# Todos os códigos leem e gravam nestas pastas. Para mudar a estrutura, basta
# alterar este arquivo. A raiz é a pasta do projeto R (arquivo .Rproj), que o
# pacote here encontra sozinho.
#
# Uso: source(here::here("0 Funções, estrutura e passo-a-passo", "0_pastas.R"))

library(here)

local({
  p0 <- "0 Funções, estrutura e passo-a-passo"
  p1 <- "1 Pesquisas individuais"
  p2 <- "2 Agregação dos dados"
  p3 <- "3 Produtos finais"

  partidos <- file.path(p1, "1a Partidos")
  eleicoes <- file.path(p1, "2a Eleições")
  produtos <- file.path(p1, "4a Produtos")

  pasta <<- list(
    # Pasta 0 — funções, estrutura e passo-a-passo
    funcoes                  = here(p0),

    # Pasta 1 — Etapa 1: dados brutos de cada pesquisa
    brutos_dirigentes        = here(partidos, "Dirigentes e diretórios", "dados_brutos"),
    brutos_financ_partidario = here(partidos, "Financiamento partidário", "dados_brutos"),
    brutos_partidos          = here(partidos, "Ideologia, democracia interna e institucionalização", "dados_brutos"),
    brutos_candidatos        = here(eleicoes, "Características dos candidatos", "dados_brutos"),
    brutos_financ_eleitoral  = here(eleicoes, "Financiamento eleitoral", "dados_brutos"),
    brutos_bens              = here(eleicoes, "Bens", "dados_brutos"),
    brutos_resultados        = here(eleicoes, "Resultados eleitorais", "dados_brutos"),
    brutos_distritos         = here(eleicoes, "Vagas, comparecimento e eleitorado", "dados_brutos"),
    brutos_socioeconomico    = here(p1, "3a Socioeconômico", "dados_brutos"),

    # Pasta 1 — Etapa 1: produtos (4a)
    prod_dirigentes          = here(produtos, "Dirigentes e diretórios"),
    prod_financ_partidario   = here(produtos, "Financiamento partidário"),
    prod_partidos            = here(produtos, "Partidos"),
    prod_candidatos          = here(produtos, "Candidatos"),
    prod_distritos           = here(produtos, "Distritos"),
    prod_socioeconomico      = here(produtos, "Socioeconômico"),

    # Pasta 1 — validação (5a)
    validacao                = here(p1, "5a Validação"),

    # Pasta 2 — Etapa 2: agregação
    juncao_partidos          = here(p2, "1b Partidos", "bancos"),
    juncao_eleicoes          = here(p2, "2b Eleições", "bancos"),

    # Pasta 3 — Etapa 3: produtos finais
    produtos_finais          = here(p3, "bancos")
  )
})
