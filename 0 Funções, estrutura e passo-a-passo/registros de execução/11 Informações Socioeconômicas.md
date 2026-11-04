---
title: "11 Informações Socioeconômicas"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-11-04"
output: html_document
---



# Objetivo

Reunir informações demográficas e socioeconômicas da basedosdados por distrito e ano (2010–2026):

- **Município:** chave `ano` e `id_municipio` (código IBGE, 7 dígitos);
- **UF:** chave `ano` e `sigla_uf` (soma dos municípios).

Os códigos das etapas seguintes procuram estes arquivos em `1 Pesquisas individuais/4a Produtos/Socioeconômico`:

| Arquivo | Chaves | Usado em |
|---|---|---|
| `socioeconomico_municipio.csv` | `ano`, `id_municipio` | 12 (candidatos), 13 (diretórios), 16 (distritos) |
| `socioeconomico_uf.csv` | `ano`, `sigla_uf` | 12, 13, 16 |

`prop_negros_populacao` (pretas + pardas sobre a população) é o parâmetro *p* do índice de equidade racial (DRI) no código 16.

**Fontes (basedosdados):**

| Informação | Tabela | Anos disponíveis |
|---|---|---|
| População por sexo e cor/raça (Censo) | `br_ibge_censo_2022.populacao_grupo_idade_sexo_raca` | 2010 e 2022 |
| Estimativas populacionais | `br_ibge_populacao.municipio` | 1991–2025 |
| PIB municipal e valor adicionado | `br_ibge_pib.municipio` | 2002–2023 (componentes até 2021) |

**Decisões:**

- **Janela dos censos:** a composição por sexo e cor/raça é constante dentro da janela de cada censo — Censo 2010 para 2010–2015 e Censo 2022 para 2016–2025 (`ano_censo` informa o censo usado). Municípios instalados depois de 2010 (ex.: Mojuí dos Campos, Pescaria Brava, Balneário Rincão, Pinto Bandeira, Paraíso das Águas) ficam sem composição até 2015.
- **População:** estimativas anuais do IBGE (`populacao`); nos anos de censo, a própria contagem.
- **PIB:** valores deflacionados pelo IPCA para reais de dezembro de 2025 (valor de cada ano em 31/12). O PIB municipal só está disponível até 2023: em 2024 e 2025 as variáveis de PIB ficam NA. Em 2022 e 2023 a basedosdados traz apenas o PIB total (sem impostos e valor adicionado por setor): nesses anos `impostos_pib` e `dependencia_*` ficam NA.
- **Participações setoriais:** `dependencia_*` = valor adicionado do setor / valor adicionado total (`dependencia_adm` = administração, defesa, educação e saúde públicas e seguridade social); `impostos_pib` = impostos líquidos / PIB.

# Pacotes e pastas


``` r
library(tidyverse)
library(basedosdados)
library(here)
library(deflateBR)
library(data.table)

source(here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))

set_billing_id("seu-projeto") ## substituir por seu projeto

anos <- 2010:2026
dir.create(pasta$prod_socioeconomico, showWarnings = FALSE, recursive = TRUE)
```

# Diretório de municípios


``` r
query <- "
SELECT
    dados.id_municipio AS id_municipio,
    dados.id_municipio_tse AS id_municipio_tse,
    dados.nome AS nome_municipio,
    dados.sigla_uf AS sigla_uf
FROM `basedosdados.br_bd_diretorios_brasil.municipio` AS dados
"

diretorios <- read_sql(query, billing_project_id = get_billing_id()) |>
  mutate(id_municipio = as.character(id_municipio))

nrow(diretorios)
```

```
## [1] 5571
```

# População por sexo e cor/raça (Censos 2010 e 2022)

A tabela traz 21 grupos de idade, sem linhas de total: a soma é feita no BigQuery.


``` r
query <- "
SELECT
    dados.ano AS ano,
    dados.id_municipio AS id_municipio,
    dados.sexo AS sexo,
    dados.cor_raca AS cor_raca,
    SUM(dados.populacao) AS populacao
FROM `basedosdados.br_ibge_censo_2022.populacao_grupo_idade_sexo_raca` AS dados
GROUP BY 1, 2, 3, 4
"

populacao_censo <- read_sql(query, billing_project_id = get_billing_id()) |>
  mutate(ano = as.integer(ano), id_municipio = as.character(id_municipio), populacao = as.numeric(populacao))

count(populacao_censo, ano, cor_raca, wt = populacao)
```

```
## # A tibble: 10 × 3
##      ano cor_raca        n
##    <int> <chr>       <dbl>
##  1  2010 Amarela   2084288
##  2  2010 Branca   91051646
##  3  2010 Indígena   817963
##  4  2010 Parda    82277333
##  5  2010 Preta    14517961
##  6  2022 Amarela    850130
##  7  2022 Branca   88252121
##  8  2022 Indígena  1227642
##  9  2022 Parda    92083286
## 10  2022 Preta    20656458
```

``` r
count(populacao_censo, ano, sexo, wt = populacao)
```

```
## # A tibble: 4 × 3
##     ano sexo             n
##   <int> <chr>        <dbl>
## 1  2010 Homens    93402274
## 2  2010 Mulheres  97346917
## 3  2022 Homens    98526088
## 4  2022 Mulheres 104543549
```

## Categorização - Município


``` r
censo_mun <- populacao_censo |>
  group_by(ano_censo = ano, id_municipio) |>
  summarise(
    censo_populacao         = sum(populacao, na.rm = TRUE),
    censo_mulheres          = sum(populacao[sexo == "Mulheres"], na.rm = TRUE),
    censo_pessoas_pretas    = sum(populacao[cor_raca == "Preta"], na.rm = TRUE),
    censo_pessoas_pardas    = sum(populacao[cor_raca == "Parda"], na.rm = TRUE),
    censo_pessoas_brancas   = sum(populacao[cor_raca == "Branca"], na.rm = TRUE),
    censo_pessoas_amarelas  = sum(populacao[cor_raca == "Amarela"], na.rm = TRUE),
    censo_pessoas_indigenas = sum(populacao[cor_raca == "Indígena"], na.rm = TRUE),
    .groups = "drop"
  )

# Toda a população tem cor/raça declarada (categorias exaustivas)
stopifnot(with(censo_mun, all(censo_populacao == censo_pessoas_pretas + censo_pessoas_pardas + censo_pessoas_brancas +
                                censo_pessoas_amarelas + censo_pessoas_indigenas)))

# Proporções (também usadas, com os totais somados, no nível da UF)
proporcoes_censo <- function(x) {
  x |>
    mutate(prop_mulheres_populacao  = censo_mulheres / censo_populacao,
           prop_pretos_populacao    = censo_pessoas_pretas / censo_populacao,
           prop_pardos_populacao    = censo_pessoas_pardas / censo_populacao,
           prop_negros_populacao    = (censo_pessoas_pretas + censo_pessoas_pardas) / censo_populacao,
           prop_brancos_populacao   = censo_pessoas_brancas / censo_populacao,
           prop_amarelos_populacao  = censo_pessoas_amarelas / censo_populacao,
           prop_indigenas_populacao = censo_pessoas_indigenas / censo_populacao)
}

censo_mun <- proporcoes_censo(censo_mun)
count(censo_mun, ano_censo)
```

```
## # A tibble: 2 × 2
##   ano_censo     n
##       <int> <int>
## 1      2010  5570
## 2      2022  5570
```

``` r
summary(select(censo_mun, starts_with("prop_")))
```

```
##  prop_mulheres_populacao prop_pretos_populacao prop_pardos_populacao
##  Min.   :0.1839          Min.   :0.00000       Min.   :0.005837     
##  1st Qu.:0.4895          1st Qu.:0.03732       1st Qu.:0.294225     
##  Median :0.4987          Median :0.06129       Median :0.507982     
##  Mean   :0.4975          Mean   :0.07476       Mean   :0.461939     
##  3rd Qu.:0.5072          3rd Qu.:0.09595       3rd Qu.:0.631527     
##  Max.   :0.5468          Max.   :0.58479       Max.   :0.926845     
##  NA's   :5               NA's   :5             NA's   :5            
##  prop_negros_populacao prop_brancos_populacao prop_amarelos_populacao
##  Min.   :0.007782      Min.   :0.008597       Min.   :0.0000000      
##  1st Qu.:0.343919      1st Qu.:0.247432       1st Qu.:0.0008386      
##  Median :0.584886      Median :0.400053       Median :0.0028541      
##  Mean   :0.536704      Mean   :0.449003       Mean   :0.0058013      
##  3rd Qu.:0.734367      3rd Qu.:0.643469       3rd Qu.:0.0080375      
##  Max.   :0.971966      Max.   :0.991594       Max.   :0.1215605      
##  NA's   :5             NA's   :5              NA's   :5              
##  prop_indigenas_populacao
##  Min.   :0.0000000       
##  1st Qu.:0.0002041       
##  Median :0.0006431       
##  Mean   :0.0084916       
##  3rd Qu.:0.0014409       
##  Max.   :0.9445131       
##  NA's   :5
```

# Estimativa populacional


``` r
query <- "
SELECT
    dados.ano AS ano,
    dados.id_municipio AS id_municipio,
    dados.populacao AS populacao
FROM `basedosdados.br_ibge_populacao.municipio` AS dados
WHERE dados.ano BETWEEN 2010 AND 2025
"

estimativa_populacional <- read_sql(query, billing_project_id = get_billing_id()) |>
  mutate(ano = as.integer(ano), id_municipio = as.character(id_municipio), populacao = as.numeric(populacao))

count(estimativa_populacional, ano)
```

```
## # A tibble: 16 × 2
##      ano     n
##    <int> <int>
##  1  2010  5565
##  2  2011  5565
##  3  2012  5570
##  4  2013  5570
##  5  2014  5570
##  6  2015  5570
##  7  2016  5570
##  8  2017  5570
##  9  2018  5570
## 10  2019  5570
## 11  2020  5570
## 12  2021  5570
## 13  2022  5570
## 14  2023  5570
## 15  2024  5570
## 16  2025  5571
```

# PIB

## Coleta dos dados


``` r
query <- "
SELECT
    dados.id_municipio AS id_municipio,
    dados.ano AS ano,
    dados.pib AS pib,
    dados.impostos_liquidos AS impostos_liquidos,
    dados.va AS va,
    dados.va_agropecuaria AS va_agropecuaria,
    dados.va_industria AS va_industria,
    dados.va_servicos AS va_servicos,
    dados.va_adespss AS va_adespss
FROM `basedosdados.br_ibge_pib.municipio` AS dados
WHERE dados.ano BETWEEN 2010 AND 2025
"

pib <- read_sql(query, billing_project_id = get_billing_id()) |>
  mutate(ano = as.integer(ano), id_municipio = as.character(id_municipio),
         across(c(pib, impostos_liquidos, starts_with("va")), as.numeric))

count(pib, ano)
```

```
## # A tibble: 14 × 2
##      ano     n
##    <int> <int>
##  1  2010  5565
##  2  2011  5565
##  3  2012  5565
##  4  2013  5570
##  5  2014  5570
##  6  2015  5570
##  7  2016  5570
##  8  2017  5570
##  9  2018  5570
## 10  2019  5570
## 11  2020  5570
## 12  2021  5570
## 13  2022  5570
## 14  2023  5570
```

## Deflacionamento e participações setoriais


``` r
vars_pib <- c("pib", "impostos_liquidos", "va", "va_agropecuaria", "va_industria", "va_servicos", "va_adespss")

# Deflaciona pelo IPCA para reais de 12/2025
pib_municipios <- pib |>
  mutate(across(all_of(vars_pib), ~ ipca(.x, as.Date(paste0(ano, "-12-31")), "12/2025")))

# Participações (razões: indiferentes ao deflacionamento)
participacoes <- function(x) {
  x |>
    mutate(impostos_pib          = impostos_liquidos / pib,
           dependencia_adm       = va_adespss / va,
           dependencia_industria = va_industria / va,
           dependencia_agro      = va_agropecuaria / va,
           dependencia_servicos  = va_servicos / va)
}

pib_municipios <- participacoes(pib_municipios)
summary(select(pib_municipios, pib, starts_with("dependencia"), impostos_pib))
```

```
##       pib             dependencia_adm   dependencia_industria dependencia_agro 
##  Min.   :-3.945e+07   Min.   :-1.9090   Min.   :-1.5498       Min.   :-0.4190  
##  1st Qu.: 1.199e+08   1st Qu.: 0.1859   1st Qu.: 0.0455       1st Qu.: 0.0774  
##  Median : 2.654e+08   Median : 0.2985   Median : 0.0735       Median : 0.1634  
##  Mean   : 1.857e+09   Mean   : 0.3311   Mean   : 0.1382       Mean   : 0.2047  
##  3rd Qu.: 7.173e+08   3rd Qu.: 0.4681   3rd Qu.: 0.1794       3rd Qu.: 0.3086  
##  Max.   : 1.173e+12   Max.   : 0.9305   Max.   : 6.9627       Max.   : 0.9350  
##                       NA's   :11140     NA's   :11140         NA's   :11140    
##  dependencia_servicos  impostos_pib    
##  Min.   :-5.0396      Min.   :-3.8345  
##  1st Qu.: 0.2280      1st Qu.: 0.0367  
##  Median : 0.3020      Median : 0.0549  
##  Mean   : 0.3259      Mean   : 0.0663  
##  3rd Qu.: 0.4065      3rd Qu.: 0.0842  
##  Max.   : 2.2354      Max.   :17.0500  
##  NA's   :11140        NA's   :11140
```

# Montagem e exportação

## Município


``` r
# Municípios presentes em alguma das fontes (exclui municípios recém-instalados
# que só constam do diretório, como Boa Esperança do Norte-MT)
com_dados <- unique(c(estimativa_populacional$id_municipio, censo_mun$id_municipio, pib$id_municipio))

painel_mun <- diretorios |>
  filter(id_municipio %in% com_dados) |>
  select(id_municipio) |>
  cross_join(tibble(ano = anos)) |>
  mutate(ano_censo = if_else(ano <= 2015, 2010L, 2022L))

socioeconomico_municipio <- painel_mun |>
  left_join(estimativa_populacional, by = c("ano", "id_municipio")) |>
  left_join(censo_mun, by = c("ano_censo", "id_municipio")) |>
  left_join(pib_municipios, by = c("ano", "id_municipio")) |>
  mutate(pib_per_capita = pib / populacao) |>
  relocate(ano, id_municipio, ano_censo, populacao) |>
  arrange(ano, id_municipio)

# Uma linha por ano-município
stopifnot(nrow(count(socioeconomico_municipio, ano, id_municipio) |> filter(n > 1)) == 0)

# Cobertura por ano
socioeconomico_municipio |>
  group_by(ano) |>
  summarise(municipios = n(),
            com_populacao = sum(!is.na(populacao)),
            com_censo = sum(!is.na(prop_negros_populacao)),
            com_pib = sum(!is.na(pib)),
            prop_negros_media = round(mean(prop_negros_populacao, na.rm = TRUE), 3)) |>
  knitr::kable()
```



|  ano| municipios| com_populacao| com_censo| com_pib| prop_negros_media|
|----:|----------:|-------------:|---------:|-------:|-----------------:|
| 2010|       5570|          5565|      5565|    5565|             0.514|
| 2011|       5570|          5565|      5565|    5565|             0.514|
| 2012|       5570|          5570|      5565|    5565|             0.514|
| 2013|       5570|          5570|      5565|    5570|             0.514|
| 2014|       5570|          5570|      5565|    5570|             0.514|
| 2015|       5570|          5570|      5565|    5570|             0.514|
| 2016|       5570|          5570|      5570|    5570|             0.560|
| 2017|       5570|          5570|      5570|    5570|             0.560|
| 2018|       5570|          5570|      5570|    5570|             0.560|
| 2019|       5570|          5570|      5570|    5570|             0.560|
| 2020|       5570|          5570|      5570|    5570|             0.560|
| 2021|       5570|          5570|      5570|    5570|             0.560|
| 2022|       5570|          5570|      5570|    5570|             0.560|
| 2023|       5570|          5570|      5570|    5570|             0.560|
| 2024|       5570|          5570|      5570|       0|             0.560|
| 2025|       5570|          5570|      5570|       0|             0.560|
| 2026|       5570|             0|      5570|       0|             0.560|

## UF

Soma dos municípios (população, contagens do censo e PIB) e razões recalculadas sobre as somas.


``` r
somar <- c("populacao", names(select(censo_mun, starts_with("censo_"))), vars_pib)

socioeconomico_uf <- socioeconomico_municipio |>
  left_join(select(diretorios, id_municipio, sigla_uf), by = "id_municipio") |>
  group_by(ano, sigla_uf, ano_censo) |>
  summarise(across(all_of(somar), ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)), .groups = "drop") |>
  proporcoes_censo() |>
  participacoes() |>
  mutate(pib_per_capita = pib / populacao) |>
  relocate(ano, sigla_uf, ano_censo, populacao) |>
  arrange(ano, sigla_uf)

stopifnot(nrow(count(socioeconomico_uf, ano, sigla_uf) |> filter(n > 1)) == 0)
stopifnot(n_distinct(socioeconomico_uf$sigla_uf) == 27)

socioeconomico_uf |>
  filter(ano %in% c(2010, 2016, 2022)) |>
  select(ano, sigla_uf, populacao, prop_mulheres_populacao, prop_negros_populacao, pib_per_capita) |>
  mutate(across(starts_with("prop_"), ~ round(.x, 3)), pib_per_capita = round(pib_per_capita)) |>
  knitr::kable()
```



|  ano|sigla_uf | populacao| prop_mulheres_populacao| prop_negros_populacao| pib_per_capita|
|----:|:--------|---------:|-----------------------:|---------------------:|--------------:|
| 2010|AC       |    733559|                   0.498|                 0.721|          26510|
| 2010|AL       |   3120494|                   0.516|                 0.668|          20269|
| 2010|AM       |   3483985|                   0.497|                 0.730|          40732|
| 2010|AP       |    669526|                   0.499|                 0.739|          28682|
| 2010|BA       |  14016906|                   0.509|                 0.763|          25681|
| 2010|CE       |   8452381|                   0.513|                 0.665|          21880|
| 2010|DF       |   2570160|                   0.522|                 0.560|         130764|
| 2010|ES       |   3514952|                   0.507|                 0.570|          56577|
| 2010|GO       |   6003788|                   0.503|                 0.565|          41456|
| 2010|MA       |   6574789|                   0.504|                 0.762|          16419|
| 2010|MG       |  19597330|                   0.508|                 0.535|          41766|
| 2010|MS       |   2449024|                   0.502|                 0.485|          44994|
| 2010|MT       |   3035122|                   0.489|                 0.600|          43472|
| 2010|PA       |   7581051|                   0.496|                 0.768|          25425|
| 2010|PB       |   3766528|                   0.516|                 0.584|          20747|
| 2010|PE       |   8796448|                   0.519|                 0.618|          25756|
| 2010|PI       |   3118360|                   0.510|                 0.734|          16647|
| 2010|PR       |  10444526|                   0.509|                 0.283|          50263|
| 2010|RJ       |  15989929|                   0.523|                 0.517|          65583|
| 2010|RN       |   3168027|                   0.511|                 0.577|          26625|
| 2010|RO       |   1562409|                   0.491|                 0.625|          35670|
| 2010|RR       |    450479|                   0.492|                 0.671|          34356|
| 2010|RS       |  10693929|                   0.513|                 0.161|          52588|
| 2010|SC       |   6248436|                   0.504|                 0.154|          57350|
| 2010|SE       |   2068017|                   0.514|                 0.703|          29764|
| 2010|SP       |  41262199|                   0.513|                 0.346|          73144|
| 2010|TO       |   1383445|                   0.492|                 0.722|          27642|
| 2016|AC       |    816687|                   0.500|                 0.748|          26186|
| 2016|AL       |   3358963|                   0.521|                 0.699|          22899|
| 2016|AM       |   4001667|                   0.501|                 0.737|          34596|
| 2016|AP       |    782295|                   0.503|                 0.771|          28506|
| 2016|BA       |  15276566|                   0.517|                 0.797|          26334|
| 2016|CE       |   8963663|                   0.516|                 0.715|          24011|
| 2016|DF       |   2977216|                   0.523|                 0.594|         123011|
| 2016|ES       |   3973697|                   0.512|                 0.610|          42754|
| 2016|GO       |   6695855|                   0.509|                 0.634|          42207|
| 2016|MA       |   6954036|                   0.509|                 0.790|          19074|
| 2016|MG       |  20997560|                   0.512|                 0.586|          40343|
| 2016|MS       |   2682386|                   0.508|                 0.534|          53266|
| 2016|MT       |   3305531|                   0.497|                 0.659|          58271|
| 2016|PA       |   8272724|                   0.501|                 0.797|          25957|
| 2016|PB       |   3999415|                   0.517|                 0.635|          22978|
| 2016|PE       |   9410336|                   0.523|                 0.653|          27650|
| 2016|PI       |   3212180|                   0.511|                 0.771|          20048|
| 2016|PR       |  11242720|                   0.513|                 0.343|          55570|
| 2016|RJ       |  16635996|                   0.528|                 0.578|          59854|
| 2016|RN       |   3474998|                   0.516|                 0.601|          26702|
| 2016|RO       |   1787279|                   0.502|                 0.679|          34329|
| 2016|RR       |    514229|                   0.497|                 0.651|          33300|
| 2016|RS       |  11286500|                   0.517|                 0.212|          56316|
| 2016|SC       |   6910553|                   0.507|                 0.233|          57769|
| 2016|SE       |   2265779|                   0.521|                 0.745|          26679|
| 2016|SP       |  44749699|                   0.518|                 0.409|          70838|
| 2016|TO       |   1532902|                   0.499|                 0.753|          32037|
| 2022|AC       |    830018|                   0.500|                 0.748|          32821|
| 2022|AL       |   3127683|                   0.521|                 0.699|          27983|
| 2022|AM       |   3941613|                   0.501|                 0.737|          42369|
| 2022|AP       |    733759|                   0.503|                 0.771|          37030|
| 2022|BA       |  14141626|                   0.517|                 0.797|          32761|
| 2022|CE       |   8794957|                   0.516|                 0.715|          27945|
| 2022|DF       |   2817381|                   0.523|                 0.594|         134277|
| 2022|ES       |   3833712|                   0.512|                 0.610|          54788|
| 2022|GO       |   7056495|                   0.509|                 0.634|          51948|
| 2022|MA       |   6776699|                   0.509|                 0.790|          23735|
| 2022|MG       |  20539989|                   0.512|                 0.586|          50794|
| 2022|MS       |   2757013|                   0.508|                 0.534|          69449|
| 2022|MT       |   3658649|                   0.497|                 0.659|          80361|
| 2022|PA       |   8120131|                   0.501|                 0.797|          33461|
| 2022|PB       |   3974687|                   0.517|                 0.635|          24923|
| 2022|PE       |   9058931|                   0.523|                 0.653|          31224|
| 2022|PI       |   3271199|                   0.511|                 0.771|          25619|
| 2022|PR       |  11444380|                   0.513|                 0.343|          61793|
| 2022|RJ       |  16055174|                   0.528|                 0.578|          82668|
| 2022|RN       |   3302729|                   0.516|                 0.601|          32685|
| 2022|RO       |   1581196|                   0.502|                 0.679|          48606|
| 2022|RR       |    636707|                   0.497|                 0.651|          38122|
| 2022|RS       |  10882965|                   0.517|                 0.212|          62763|
| 2022|SC       |   7610361|                   0.507|                 0.233|          70496|
| 2022|SE       |   2210004|                   0.521|                 0.745|          29870|
| 2022|SP       |  44411238|                   0.518|                 0.409|          81101|
| 2022|TO       |   1511460|                   0.499|                 0.753|          44312|

## Validação

Brasil (soma das UFs) comparado aos valores publicados pelo IBGE: Censo 2010 — 190,76 milhões de habitantes, 50,7% pretos e pardos; Censo 2022 — 203,08 milhões, 55,5% pretos e pardos.


``` r
socioeconomico_uf |>
  filter(ano %in% c(2010, 2022)) |>
  group_by(ano) |>
  summarise(censo_populacao_mi = sum(censo_populacao) / 1e6,
            prop_negros = sum(censo_pessoas_pretas + censo_pessoas_pardas) / sum(censo_populacao),
            prop_mulheres = sum(censo_mulheres) / sum(censo_populacao)) |>
  knitr::kable(digits = 3)
```



|  ano| censo_populacao_mi| prop_negros| prop_mulheres|
|----:|------------------:|-----------:|-------------:|
| 2010|            190.749|       0.507|         0.510|
| 2022|            203.070|       0.555|         0.515|

## Exportação


``` r
fwrite(socioeconomico_municipio, file.path(pasta$prod_socioeconomico, "socioeconomico_municipio.csv"), sep = ";")
fwrite(socioeconomico_uf, file.path(pasta$prod_socioeconomico, "socioeconomico_uf.csv"), sep = ";")
```
