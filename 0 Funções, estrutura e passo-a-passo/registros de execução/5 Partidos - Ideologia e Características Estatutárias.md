---
title: "5 Partidos - Ideologia e Características Estatutárias"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-06-11"
output: pdf_document
---



## Introdução

Este código monta um banco no nível **partido-ano** (2010–2025) com características nacionais dos partidos: ideologia (duas medidas), democracia interna e o índice PSI. O banco é juntado aos bancos de direções e financiamento no código `5_1 Junção Partidos.R`.

Todas as fontes usam siglas da época da coleta (PMDB, PR, PRB, PPS etc.). Os bancos de dirigentes e de financiamento usam a sigla atual do partido em todos os anos (MDB, PL, REPUBLICANOS, CIDADANIA...). Por isso as siglas das fontes são convertidas para a sigla atual padronizada (`SG_PARTIDO_PAD`, a mesma usada no código 4).

## Pacotes


``` r
library(here)
```

```
## Warning: pacote 'here' foi compilado no R versão 4.5.2
```

```
## here() starts at <raiz do projeto>
```

``` r
library(openxlsx)
```

```
## Warning: pacote 'openxlsx' foi compilado no R versão 4.5.2
```

``` r
library(tidyverse)
```

```
## Warning: pacote 'ggplot2' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'tibble' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'tidyr' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'readr' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'purrr' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'dplyr' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'stringr' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'forcats' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'lubridate' foi compilado no R versão 4.5.2
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.2.0     ✔ readr     2.1.6
## ✔ forcats   1.0.1     ✔ stringr   1.6.0
## ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
## ✔ purrr     1.2.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
source(here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))
```

```
## Warning: pacote 'data.table' foi compilado no R versão 4.5.3
```

```
## 
## Anexando pacote: 'data.table'
## 
## Os seguintes objetos são mascarados por 'package:lubridate':
## 
##     hour, isoweek, isoyear, mday, minute, month, quarter, second, wday,
##     week, yday, year
## 
## Os seguintes objetos são mascarados por 'package:dplyr':
## 
##     between, first, last
## 
## O seguinte objeto é mascarado por 'package:purrr':
## 
##     transpose
```

``` r
pasta_partidos <- pasta$brutos_partidos

# 2010–2025: cobre as eleições desde 2010 (banco de candidatos) e os
# diretórios desde 2015
anos <- 2010:2025
```

## Correspondência de siglas

Siglas antigas ou abreviadas das fontes e a sigla atual correspondente, pela função `sigla_atual()` de `0_funcoes_auxiliares.R` (a mesma usada para os partidos dos candidatos no código 12). Partidos que se fundiram (PHS, PPL, PRP, DEM, PSL, PTB, Patriota, PROS, PSC) mantêm a própria sigla, com dados só nos anos em que existiram nas fontes.


``` r
# Partidos do banco de dirigentes (um arquivo por partido, sigla atual)
partidos <- list.files(pasta$brutos_dirigentes, pattern = "\\.csv$") |>
  str_remove("^orgao_partidario_") |>
  str_remove("\\.csv$") |>
  sigla_atual() |>
  unique() |>
  sort()

partidos
```

```
##  [1] "AGIR"          "AVANTE"        "CIDADANIA"     "DC"           
##  [5] "DEM"           "MDB"           "MISSAO"        "MOBILIZA"     
##  [9] "NOVO"          "PATRIOTA"      "PCB"           "PCDOB"        
## [13] "PCO"           "PDT"           "PHS"           "PL"           
## [17] "PMB"           "PODE"          "PP"            "PPL"          
## [21] "PRD"           "PROS"          "PRP"           "PRTB"         
## [25] "PSB"           "PSC"           "PSD"           "PSDB"         
## [29] "PSL"           "PSOL"          "PSTU"          "PT"           
## [33] "PTB"           "PV"            "REDE"          "REPUBLICANOS" 
## [37] "SOLIDARIEDADE" "UNIAO"         "UP"
```

``` r
base <- expand_grid(SG_PARTIDO_PAD = partidos, ANO = anos)
```

## Dados

### Ideologia

Dados de Ideologia retirados de três fontes:

- Bolognesi, B., Ribeiro, E., & Codato, A.. (2023). Uma Nova Classificação Ideológica dos Partidos Políticos Brasileiros. Dados, 66(2), e20210164. https://doi.org/10.1590/dados.2023.66.2.303

- Bolognesi B, Ribeiro E, Codato A, Silva BF da. O desaparecimento do centro ideológico no sistema partidário brasileiro: a classificação mais atualizada dos experts, . Opin Publica [Internet]. 2025;31:e31120. Available from: https://doi.org/10.1590/1807-0191202531120

- Zucco, Cesar, 2023, "Brazilian Legislative Surveys (Waves 1-9, 1990-2021)", https://doi.org/10.7910/DVN/WM9IZ8, Harvard Dataverse, V1, UNF:6:h2YTat2Lb1dDZFgDj2u/Qg== [fileUNF]


``` r
ideologia_BLS <- openxlsx::read.xlsx(file.path(pasta_partidos, "ideologia_BLS.xlsx"))

summary(ideologia_BLS)
```

```
##     BLS_YEAR       NUM_PART     SIGLA_PARTIDO      NOME_PARTIDO      
##  Min.   :1993   Min.   :10.00   Length:385         Length:385        
##  1st Qu.:2001   1st Qu.:17.00   Class :character   Class :character  
##  Median :2005   Median :27.00   Mode  :character   Mode  :character  
##  Mean   :2007   Mean   :30.97                                        
##  3rd Qu.:2013   3rd Qu.:43.00                                        
##  Max.   :2018   Max.   :90.00                                        
##                                                                      
##     IDEOLOGY          IDEO_IMPUTED        CRITERIA          ANO_ELEICAO  
##  Min.   :-0.941994   Min.   :-0.94199   Length:385         Min.   :1994  
##  1st Qu.:-0.496903   1st Qu.:-0.45997   Class :character   1st Qu.:2000  
##  Median : 0.097552   Median : 0.34648   Mode  :character   Median :2006  
##  Mean   :-0.002513   Mean   : 0.07024                      Mean   :2007  
##  3rd Qu.: 0.445155   3rd Qu.: 0.46415                      3rd Qu.:2014  
##  Max.   : 0.902939   Max.   : 0.90294                      Max.   :2018  
##  NA's   :211
```

``` r
ideologia_bolognesi <- openxlsx::read.xlsx(file.path(pasta_partidos, "ideologia_bolognesi.xlsx"))

summary(ideologia_bolognesi)
```

```
##    partido          ideologia_2018_media ideologia_2022_media
##  Length:32          Min.   :0.510        Min.   :0.510       
##  Class :character   1st Qu.:4.685        1st Qu.:3.665       
##  Mode  :character   Median :6.900        Median :6.750       
##                     Mean   :5.884        Mean   :5.709       
##                     3rd Qu.:7.780        3rd Qu.:7.827       
##                     Max.   :8.340        Max.   :8.800       
##                     NA's   :1                                
##  media_ponderada_ideologia ideologia_categorica_2022
##  Min.   :0.525             Length:32                
##  1st Qu.:3.776             Class :character         
##  Median :6.955             Mode  :character         
##  Mean   :5.883                                      
##  3rd Qu.:8.066                                      
##  Max.   :9.068                                      
## 
```

#### BLS (Zucco e Power)

A escala vai de -1 (esquerda) a 1 (direita). Usa-se `IDEO_IMPUTED`, que completa os partidos sem estimativa na onda com os critérios dos autores (variável `CRITERIA`: MP = estimativa própria; demais = imputação). `IDEOLOGY` (só estimativas próprias) também é mantida.

As ondas disponíveis no arquivo são 2013, 2017 e 2018. Cada ano recebe a onda mais recente realizada até aquele ano: 2015–2016 recebem a onda de 2013, 2017 a de 2017 e 2018 em diante a de 2018. A onda de 2013 aparece duas vezes no arquivo (associada às eleições de 2012 e 2014) com os mesmos valores; as duplicatas são removidas.


``` r
bls <- ideologia_BLS |>
  mutate(SG_PARTIDO_PAD = sigla_atual(SIGLA_PARTIDO)) |>
  distinct(BLS_YEAR, SG_PARTIDO_PAD, .keep_all = TRUE) |>
  select(bls_onda = BLS_YEAR, SG_PARTIDO_PAD,
         bls_ideologia = IDEO_IMPUTED, bls_ideologia_estimada = IDEOLOGY,
         bls_criterio = CRITERIA)

# Uma linha por onda-partido
bls |> count(bls_onda, SG_PARTIDO_PAD) |> filter(n > 1)
```

```
## [1] bls_onda       SG_PARTIDO_PAD n             
## <0 linhas> (ou row.names de comprimento 0)
```

``` r
ondas_bls <- sort(unique(bls$bls_onda))

bls_ano <- base |>
  mutate(bls_onda = map_dbl(ANO, \(a) max(ondas_bls[ondas_bls <= a]))) |>
  left_join(bls, by = c("bls_onda", "SG_PARTIDO_PAD"))

bls_ano |> count(ANO, bls_onda, com_dado = !is.na(bls_ideologia))
```

```
## # A tibble: 32 × 4
##      ANO bls_onda com_dado     n
##    <int>    <dbl> <lgl>    <int>
##  1  2010     2009 FALSE       12
##  2  2010     2009 TRUE        27
##  3  2011     2009 FALSE       12
##  4  2011     2009 TRUE        27
##  5  2012     2009 FALSE       12
##  6  2012     2009 TRUE        27
##  7  2013     2013 FALSE        7
##  8  2013     2013 TRUE        32
##  9  2014     2013 FALSE        7
## 10  2014     2013 TRUE        32
## # ℹ 22 more rows
```

#### Bolognesi, Ribeiro e Codato

Escala de 0 (extrema esquerda) a 10 (extrema direita), a partir de surveys com especialistas em 2018 e 2022. Os dados de 2022 valem para os anos de 2019 em diante; os de 2018, para os anos até 2018. A classificação categórica existe apenas para 2022 e segue a mesma regra.


``` r
bolognesi <- ideologia_bolognesi |>
  mutate(SG_PARTIDO_PAD = sigla_atual(partido)) |>
  select(SG_PARTIDO_PAD, ideologia_2018_media, ideologia_2022_media,
         ideologia_categorica_2022)

bolognesi |> count(SG_PARTIDO_PAD) |> filter(n > 1)
```

```
## [1] SG_PARTIDO_PAD n             
## <0 linhas> (ou row.names de comprimento 0)
```

``` r
bolognesi_ano <- base |>
  left_join(bolognesi, by = "SG_PARTIDO_PAD") |>
  mutate(
    bolognesi_onda      = if_else(ANO >= 2019, 2022L, 2018L),
    bolognesi_ideologia = if_else(ANO >= 2019, ideologia_2022_media, ideologia_2018_media),
    bolognesi_categoria = if_else(ANO >= 2019, ideologia_categorica_2022, NA_character_)
  ) |>
  select(SG_PARTIDO_PAD, ANO, bolognesi_onda, bolognesi_ideologia, bolognesi_categoria)
```

### Democracia Interna

Dados de democracia interna:

- Ribeiro, P. F., Locatelli, L., & Assis, P. P. de .. (2022). “ Acompanho o Meu Partido ”: Organização Partidária e Comportamento Legislativo no Brasil  . Dados, 65(4), e20200052. https://doi.org/10.1590/dados.2022.65.4.276

- SALGADO, E. D.. (2020). Intra-party democracy index: a measure model from Brazil. Revista De Investigações Constitucionais, 7(1), 107–136. https://doi.org/10.5380/rinc.v7i1.74101


``` r
democracia_ribeiro <- openxlsx::read.xlsx(file.path(pasta_partidos, "Base de Dados.xlsx"))

summary(democracia_ribeiro)
```

```
##    Disciplina        Coesao           PSI           Peso_na_coalizão
##  Min.   :71.70   Min.   :66.59   Min.   :-3.62515   Min.   : 0.000  
##  1st Qu.:87.05   1st Qu.:78.58   1st Qu.:-2.01401   1st Qu.: 0.000  
##  Median :91.38   Median :83.17   Median : 0.02529   Median : 0.000  
##  Mean   :90.36   Mean   :84.07   Mean   : 0.06398   Mean   : 6.264  
##  3rd Qu.:94.67   3rd Qu.:90.61   3rd Qu.: 1.18708   3rd Qu.:10.765  
##  Max.   :99.54   Max.   :98.17   Max.   : 5.75840   Max.   :28.010  
##  NA's   :5       NA's   :5       NA's   :5          NA's   :5       
##  Ocupação_Ministério      IPD         Esquerda_direita   Partido         
##  Min.   : 0.000      Min.   :0.3280   Min.   :2.000    Length:68         
##  1st Qu.: 0.000      1st Qu.:0.3380   1st Qu.:4.617    Class :character  
##  Median : 0.000      Median :0.3480   Median :5.857    Mode  :character  
##  Mean   : 5.148      Mean   :0.3612   Mean   :5.347                      
##  3rd Qu.: 4.000      3rd Qu.:0.3800   3rd Qu.:6.500                      
##  Max.   :46.000      Max.   :0.4910   Max.   :7.429                      
##  NA's   :5           NA's   :5        NA's   :5                          
##       Ano      
##  Min.   :2011  
##  1st Qu.:2012  
##  Median :2012  
##  Mean   :2012  
##  3rd Qu.:2013  
##  Max.   :2014  
## 
```

``` r
democracia_salgado <- openxlsx::read.xlsx(file.path(pasta_partidos, "democracia_interna_salgado.xlsx"))

summary(democracia_salgado)
```

```
##    partido          democracia_interna
##  Length:35          Min.   :0.1000    
##  Class :character   1st Qu.:0.2350    
##  Mode  :character   Median :0.3000    
##                     Mean   :0.3031    
##                     3rd Qu.:0.3650    
##                     Max.   :0.5600
```

``` r
tomada_decisao <- openxlsx::read.xlsx(file.path(pasta_partidos, "tomada_decisao.xlsx"))

summary(tomada_decisao)
```

```
##    partido          definicao_programa  recrutamento    tomada_decisao  
##  Length:35          Min.   :0.0000     Min.   :0.0000   Min.   :0.0000  
##  Class :character   1st Qu.:0.0000     1st Qu.:0.0000   1st Qu.:0.0000  
##  Mode  :character   Median :0.0000     Median :0.2000   Median :0.1000  
##                     Mean   :0.1086     Mean   :0.1257   Mean   :0.1143  
##                     3rd Qu.:0.2000     3rd Qu.:0.2000   3rd Qu.:0.2000  
##                     Max.   :0.6000     Max.   :0.4000   Max.   :0.4000
```

``` r
estrutura_organizacional <- openxlsx::read.xlsx(file.path(pasta_partidos, "estrutura_organizacional.xlsx"))

summary(estrutura_organizacional)
```

```
##    partido          composicao_orgao_partidario resolucao_conflitos
##  Length:35          Min.   :0.0000              Min.   :0.0000     
##  Class :character   1st Qu.:0.2900              1st Qu.:0.3300     
##  Mode  :character   Median :0.4300              Median :0.4400     
##                     Mean   :0.4586              Mean   :0.4431     
##                     3rd Qu.:0.5700              3rd Qu.:0.6050     
##                     Max.   :0.8600              Max.   :0.7700     
##  relacao_intrapartidaria estrutura_organizacional
##  Min.   :0.0000          Min.   :0.1600          
##  1st Qu.:0.2000          1st Qu.:0.3250          
##  Median :0.4000          Median :0.4500          
##  Mean   :0.3829          Mean   :0.4283          
##  3rd Qu.:0.6000          3rd Qu.:0.5300          
##  Max.   :0.6000          Max.   :0.6600
```

#### Salgado (2020)

Índice de democracia interna (0 a 1) e seus componentes, calculados a partir dos estatutos partidários. É uma medida única por partido, repetida em todos os anos.


``` r
di <- democracia_salgado |>
  left_join(tomada_decisao, by = "partido") |>
  left_join(estrutura_organizacional, by = "partido") |>
  mutate(SG_PARTIDO_PAD = sigla_atual(partido)) |>
  select(-partido) |>
  rename_with(~ paste0("salgado_", .x), -SG_PARTIDO_PAD)

di |> count(SG_PARTIDO_PAD) |> filter(n > 1)
```

```
## [1] SG_PARTIDO_PAD n             
## <0 linhas> (ou row.names de comprimento 0)
```

#### Ribeiro, Locatelli e Assis (2022): PSI e IPD

A base traz, por partido e ano (2011–2014, 17 partidos), o PSI e o IPD (índice de democracia partidária). Como o banco começa em 2015, usa-se a média de 2011–2014 de cada partido, repetida em todos os anos. O IPD é constante no período; o PSI varia pouco entre anos.


``` r
table(democracia_ribeiro$Ano)
```

```
## 
## 2011 2012 2013 2014 
##   17   17   17   17
```

``` r
ribeiro <- democracia_ribeiro |>
  mutate(SG_PARTIDO_PAD = sigla_atual(Partido)) |>
  group_by(SG_PARTIDO_PAD) |>
  summarise(
    ribeiro_psi = mean(PSI, na.rm = TRUE),
    ribeiro_ipd = mean(IPD, na.rm = TRUE),
    .groups = "drop"
  )

ribeiro
```

```
## # A tibble: 17 × 3
##    SG_PARTIDO_PAD ribeiro_psi ribeiro_ipd
##    <chr>                <dbl>       <dbl>
##  1 CIDADANIA          -2.35         0.338
##  2 DEM                 1.03         0.348
##  3 MDB                 3.57         0.38 
##  4 PCDOB              -3.41         0.338
##  5 PDT                -0.246        0.38 
##  6 PL                  1.12         0.338
##  7 PP                  1.02         0.38 
##  8 PROS               -2.73         0.338
##  9 PSB                 0.0130       0.38 
## 10 PSC                -0.950        0.338
## 11 PSD                 0.482        0.328
## 12 PSDB                2.39         0.369
## 13 PT                  5.19         0.491
## 14 PTB                 0.0801       0.33 
## 15 PV                 -3.07         0.36 
## 16 REPUBLICANOS       -2.03         0.338
## 17 SOLIDARIEDADE      -1.82         0.338
```

## Banco partido-ano


``` r
banco_partidos <- base |>
  left_join(bls_ano,       by = c("SG_PARTIDO_PAD", "ANO")) |>
  left_join(bolognesi_ano, by = c("SG_PARTIDO_PAD", "ANO")) |>
  left_join(di,            by = "SG_PARTIDO_PAD") |>
  left_join(ribeiro,       by = "SG_PARTIDO_PAD")

# Uma linha por partido-ano
banco_partidos |> count(SG_PARTIDO_PAD, ANO) |> filter(n > 1)
```

```
## # A tibble: 0 × 3
## # ℹ 3 variables: SG_PARTIDO_PAD <chr>, ANO <int>, n <int>
```

``` r
# Siglas das fontes que não correspondem a nenhum partido do banco de dirigentes
# (devem ser apenas partidos extintos antes de 2015 ou sem diretório)
setdiff(unique(c(bls$SG_PARTIDO_PAD, bolognesi$SG_PARTIDO_PAD,
                 di$SG_PARTIDO_PAD, ribeiro$SG_PARTIDO_PAD)), partidos)
```

```
##  [1] "PPR"   "PTRB"  "PFL"   "PRN"   "PRONA" "PPB"   "PST"   "PAN"   "PGT"  
## [10] "PSN"
```

``` r
# Cobertura por fonte
banco_partidos |>
  group_by(ANO) |>
  summarise(
    partidos      = n(),
    bls           = sum(!is.na(bls_ideologia)),
    bolognesi     = sum(!is.na(bolognesi_ideologia)),
    salgado       = sum(!is.na(salgado_democracia_interna)),
    ribeiro_psi   = sum(!is.na(ribeiro_psi))
  )
```

```
## # A tibble: 16 × 6
##      ANO partidos   bls bolognesi salgado ribeiro_psi
##    <int>    <int> <int>     <int>   <int>       <int>
##  1  2010       39    27        31      35          17
##  2  2011       39    27        31      35          17
##  3  2012       39    27        31      35          17
##  4  2013       39    32        31      35          17
##  5  2014       39    32        31      35          17
##  6  2015       39    32        31      35          17
##  7  2016       39    32        31      35          17
##  8  2017       39    35        31      35          17
##  9  2018       39    35        31      35          17
## 10  2019       39    35        32      35          17
## 11  2020       39    35        32      35          17
## 12  2021       39    35        32      35          17
## 13  2022       39    35        32      35          17
## 14  2023       39    35        32      35          17
## 15  2024       39    35        32      35          17
## 16  2025       39    35        32      35          17
```

``` r
# Correlação entre as duas medidas de ideologia
cor(banco_partidos$bls_ideologia, banco_partidos$bolognesi_ideologia, use = "complete.obs")
```

```
## [1] 0.9500477
```

## Exportação


``` r
write.xlsx(
  banco_partidos,
  file.path(pasta$prod_partidos, "banco_partidos_ano.xlsx"),
  overwrite = TRUE
)
```
