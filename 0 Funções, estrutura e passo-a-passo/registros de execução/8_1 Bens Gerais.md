---
title: "8_1 Bens Gerais"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-11-04"
output: html_document
---




Dados dos bens dos candidatos das eleições gerais (2010, 2014, 2018, 2022 e 2026)

Por candidato

Retirados da basedosdados


Posteriormente, validar a partir das informaçoes retiradas diretamente do Portal de Dados Abertos do TSE 

https://dadosabertos.tse.jus.br/dataset/ 


## Pacotes e configuração


``` r
library(tidyverse)
```

```
## Warning: pacote 'ggplot2' foi compilado no R versão 4.5.3
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
## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
## ✔ purrr     1.2.1     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(basedosdados)
```

```
## Warning: pacote 'basedosdados' foi compilado no R versão 4.5.2
```

``` r
library(stringr)
library(stringi)
library(openxlsx)
```

```
## Warning: pacote 'openxlsx' foi compilado no R versão 4.5.2
```

``` r
library(scales)
```

```
## 
## Anexando pacote: 'scales'
## 
## O seguinte objeto é mascarado por 'package:purrr':
## 
##     discard
## 
## O seguinte objeto é mascarado por 'package:readr':
## 
##     col_factor
```

``` r
library(deflateBR)
```

```
## 
## To cite deflateBR in publications, please use: citation('deflateBR')
```

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
source(here("0 Funções, estrutura e passo-a-passo", "0_pastas.R"))

set_billing_id("seu-projeto") ## substituir por seu projeto
```

```
## Project keys set successfully
```

``` r
# Somente eleições ordinárias. "eleicoes municipais 2020 - ap" é a eleição
# ordinária de Macapá (AP) de 2020, adiada para dezembro por causa do apagão
# no estado; as demais categorias são eleições suplementares e novas eleições.
tipos_ordinarios <- c("eleicao ordinaria", "eleicoes municipais 2020 - ap")
```



## Dados Candidatos

### Carregar dados


``` r
bd_rec <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.bens_candidato") %>%
  dplyr::filter(ano %in% c(2010, 2014, 2018, 2022, 2026), tipo_eleicao %in% tipos_ordinarios) %>%
  dplyr::select(
    ano, sigla_uf, tipo_eleicao,
    titulo_eleitoral_candidato, sequencial_candidato, 
    tipo_item, descricao_item,
    valor_item
  )
```

```
## ! Using an auto-discovered, cached token.
```

```
##   To suppress this message, modify your code or options to clearly consent to
##   the use of a cached token.
```

```
##   See gargle's "Non-interactive auth" vignette for more details:
```

```
##   <https://gargle.r-lib.org/articles/non-interactive-auth.html>
```

```
## ℹ The bigrquery package is using a cached token for '<e-mail>'.
```

```
## Successfully connected to table `basedosdados.br_tse_eleicoes.bens_candidato`.
```

``` r
bens <- basedosdados::bd_collect(bd_rec)
rm(bd_rec); gc()
```

```
##           used  (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells 2146146 114.7    3828924 204.5  3517843 187.9
## Vcells 9231987  70.5   13767138 105.1  9935988  75.9
```

``` r
names(bens)
```

```
## [1] "ano"                        "sigla_uf"                  
## [3] "tipo_eleicao"               "titulo_eleitoral_candidato"
## [5] "sequencial_candidato"       "tipo_item"                 
## [7] "descricao_item"             "valor_item"
```

``` r
summary(bens)
```

```
##       ano         sigla_uf         tipo_eleicao      
##  Min.   :2010   Length:425939      Length:425939     
##  1st Qu.:2014   Class :character   Class :character  
##  Median :2018   Mode  :character   Mode  :character  
##  Mean   :2018                                        
##  3rd Qu.:2022                                        
##  Max.   :2026                                        
##  titulo_eleitoral_candidato sequencial_candidato  tipo_item        
##  Length:425939              Length:425939        Length:425939     
##  Class :character           Class :character     Class :character  
##  Mode  :character           Mode  :character     Mode  :character  
##                                                                    
##                                                                    
##                                                                    
##  descricao_item       valor_item        
##  Length:425939      Min.   :    -38101  
##  Class :character   1st Qu.:      8637  
##  Mode  :character   Median :     38057  
##                     Mean   :    259035  
##                     3rd Qu.:    124442  
##                     Max.   :2135000000
```

``` r
bens %>%
  count(ano, sem_tipo = is.na(tipo_item)) %>%
  tidyr::pivot_wider(names_from = sem_tipo, values_from = n)
```

```
## # A tibble: 5 × 3
##     ano `TRUE` `FALSE`
##   <int>  <int>   <int>
## 1  2010  81226      NA
## 2  2014     NA   82707
## 3  2018     NA   93196
## 4  2022     NA   92561
## 5  2026     NA   76249
```

``` r
# Se os NA se concentram em algum ano, veja se a descrição permite recuperar o tipo
bens %>% filter(is.na(tipo_item)) %>% slice_sample(n = 30) %>% select(ano, descricao_item, valor_item)
```

```
## # A tibble: 30 × 3
##      ano descricao_item                                               valor_item
##    <int> <chr>                                                             <dbl>
##  1  2010 SL409 ED.ZIRCONIO EM CONST.ADQ. JUN/2008 DA CARREL ENGENHAR…     69435 
##  2  2010 49% DO CAPITAL SOCIAL DA EMPRESA DROGARIA PRO SAUDE CNPJ 38…      9800 
##  3  2010 SALDO ACOES ITAU VALE DO RIO DOCE EM NOME DO CONJUGE              1000 
##  4  2010 BANCO DEPOSITO                                                 1188719 
##  5  2010 BANCO DO BRASIL                                                    553.
##  6  2010 DISPONIVEL                                                        5000 
##  7  2010 DIREITO DE POSSE S UMA AREA DE TERRENO BAIRRO CLEMENTES          15000 
##  8  2010 NO LOTE 16 DA Q 01 NO LOTEAMENTO MARTA ROCHA EM FATIMA DO S…     20819.
##  9  2010 COROLLA (TOYOTA) 2007                                            35000 
## 10  2010 LOTE 14, QUADRA 16, JD EUCALIPTOS, MARACANA, COLOMBO, PR          5000 
## # ℹ 20 more rows
```

### Categorização



``` r
table(bens$ano)
```

```
## 
##  2010  2014  2018  2022  2026 
## 81226 82707 93196 92561 76249
```

``` r
itens <- bens %>%
  count(tipo_item, name = "n_doacoes", sort = TRUE)

head(itens, 50)
```

```
## # A tibble: 50 × 2
##    tipo_item                                                    n_doacoes
##    <chr>                                                            <int>
##  1 <NA>                                                             81226
##  2 Veículo automotor terrestre: caminhão, automóvel, moto, etc.     66852
##  3 Terreno                                                          36632
##  4 Casa                                                             33008
##  5 Depósito bancário em conta corrente no País                      29008
##  6 Apartamento                                                      25329
##  7 Quotas ou quinhões de capital                                    23517
##  8 Aplicação de renda fixa (CDB, RDB e outros)                      15279
##  9 Outros bens imóveis                                              15193
## 10 Caderneta de poupança                                            11949
## # ℹ 40 more rows
```

``` r
library(stringi)

norm_txt <- function(x) x %>% stri_trans_general("Latin-ASCII") %>% str_to_lower() %>% str_squish()

tipos_imoveis <- c(
  "terreno", "casa", "apartamento", "outros bens imoveis", "terra nua",
  "sala ou conjunto", "predio comercial", "loja", "construcao",
  "predio residencial", "benfeitorias", "galpao"
)

tipos_veiculos <- c(
  "veiculo automotor terrestre: caminhao, automovel, moto, etc.",
  "embarcacao", "aeronave"
)

tipos_participacoes <- c(
  "quotas ou quinhoes de capital", "outras participacoes societarias"
)

tipos_liquidez <- c(
  "deposito bancario em conta corrente no pais",
  "deposito bancario em conta corrente no exterior",
  "caderneta de poupanca",
  "dinheiro em especie - moeda nacional",
  "dinheiro em especie - moeda estrangeira",
  "outros depositos a vista e numerario"
)

tipos_aplicacoes <- c(
  "aplicacao de renda fixa (cdb, rdb e outros)",
  "outras aplicacoes e investimentos",
  "acoes (inclusive as provenientes de linha telefonica)",
  "outros fundos",
  "fundos: acoes, mutuos de privatizacao, invest. empresas emergentes, invest.participacao e invest. indice mercado",
  "vgbl - vida gerador de beneficio livre",
  "fundo de capitalizacao",
  "fundo de investimento financeiro - fif",
  "fundo de longo prazo e fundo de investimentos em direitos creditorios (fidc)",
  "fundo de investimento imobiliario",
  "fundo de curto prazo",
  "fundo de acoes, inclusive carteira livre e fundo de investimento no exterior",
  "fundo de aplicacao em quotas de fundos de investimento",
  "ouro, ativo financeiro",
  "mercado futuros, de opcoes e a termo",
  "plano pait e caderneta de peculio"
)

tipos_creditos <- c(
  "credito decorrente de emprestimo", "credito decorrente de alienacao",
  "outros creditos e poupanca vinculados", "consorcio nao contemplado",
  "poupanca para construcao ou aquisicao de bem imovel", "leasing"
)

tipos_outros <- c(
  "outros bens e direitos", "outros bens moveis", "linha telefonica",
  "joia, quadro, objeto de arte, de colecao, antiguidade, etc.",
  "titulo de clube e assemelhado",
  "bem relacionado com o exercicio da atividade autonoma",
  "direito de autor, de inventor e patente",
  "licenca e concessoes especiais", "direito de lavra e assemelhado"
)

padrao_rural <- "\\b(fazenda|sitio|chacara|rural|gleba|hectare|ha\\b|lote rural|imovel rural)"

bens <- bens %>%
  mutate(
    tipo_norm = norm_txt(tipo_item),
    desc_norm = norm_txt(descricao_item),
    valor_item = as.numeric(valor_item),

    bens_imoveis      = if_else(tipo_norm %in% tipos_imoveis, 1, 0),
    bens_veiculos     = if_else(tipo_norm %in% tipos_veiculos, 1, 0),
    bens_participacoes = if_else(tipo_norm %in% tipos_participacoes, 1, 0),
    bens_liquidez     = if_else(tipo_norm %in% tipos_liquidez, 1, 0),
    bens_aplicacoes   = if_else(tipo_norm %in% tipos_aplicacoes, 1, 0),
    bens_creditos     = if_else(tipo_norm %in% tipos_creditos, 1, 0),
    bens_outros       = if_else(tipo_norm %in% tipos_outros, 1, 0),

    # Subcategoria: imóvel rural (terra nua ou descrição rural dentro de imóveis)
    bens_imoveis_rurais = if_else(
      tipo_norm %in% "terra nua" |
        (bens_imoveis == 1 & str_detect(desc_norm, padrao_rural) %in% TRUE), 1, 0),

    # Agregados
    bens_reais       = pmax(bens_imoveis, bens_veiculos),
    bens_financeiros = pmax(bens_liquidez, bens_aplicacoes, bens_creditos)
  )

# Checagens ------------------------------------------------------------------
# 1. Tipos não NA que não caíram em nenhuma categoria (deve voltar vazio)
bens <- bens %>%
  mutate(
    bens_nao_identificados = if_else(is.na(tipo_norm), 1, 0)
  )

# Checagem: agora toda linha cai em exatamente uma categoria
bens %>%
  mutate(n_cat = bens_imoveis + bens_veiculos + bens_participacoes + bens_liquidez +
           bens_aplicacoes + bens_creditos + bens_outros + bens_nao_identificados) %>%
  count(ano, n_cat)
```

```
## # A tibble: 5 × 3
##     ano n_cat     n
##   <int> <dbl> <int>
## 1  2010     1 81226
## 2  2014     1 82707
## 3  2018     1 93196
## 4  2022     1 92561
## 5  2026     1 76249
```

``` r
# Confirma que os não identificados estão em 2010
table(bens$ano, bens$bens_nao_identificados)
```

```
##       
##            0     1
##   2010     0 81226
##   2014 82707     0
##   2018 93196     0
##   2022 92561     0
##   2026 76249     0
```



### Unificar ao nível do candidato 



``` r
vars_bens <- c(
  "bens_imoveis", "bens_imoveis_rurais", "bens_veiculos", "bens_participacoes",
  "bens_liquidez", "bens_aplicacoes", "bens_creditos", "bens_outros",
  "bens_nao_identificados",
  "bens_reais", "bens_financeiros"
)


dados_cand_bens <- bens %>%
  group_by(ano, sigla_uf, sequencial_candidato) %>%
  summarise(
    n_bens          = n(),
    patrimonio_total = sum(valor_item, na.rm = TRUE),
    across(all_of(vars_bens),
           ~ sum(valor_item[.x == 1], na.rm = TRUE),
           .names = "soma_{.col}"),
    across(all_of(vars_bens), ~ as.integer(any(.x == 1)), .names = "tem_{.col}"),
    .groups = "drop"
  ) %>%
  mutate(
    across(starts_with("soma_bens_"),
           ~ if_else(patrimonio_total > 0, 100 * .x / patrimonio_total, NA_real_),
           .names = "{sub('soma_', 'pct_', .col)}")
  )

dados_cand_bens %>%
  group_by(ano) %>%
  summarise(across(starts_with("pct_"), ~ round(mean(.x, na.rm = TRUE), 1)))
```

```
## # A tibble: 5 × 12
##     ano pct_bens_imoveis pct_bens_imoveis_rurais pct_bens_veiculos
##   <int>            <dbl>                   <dbl>             <dbl>
## 1  2010              0                       0                 0  
## 2  2014             53.7                     4.9              25.5
## 3  2018             52.1                     2.4              24.4
## 4  2022             50.2                     3.8              23.8
## 5  2026             49.6                     4.1              23.3
## # ℹ 8 more variables: pct_bens_participacoes <dbl>, pct_bens_liquidez <dbl>,
## #   pct_bens_aplicacoes <dbl>, pct_bens_creditos <dbl>, pct_bens_outros <dbl>,
## #   pct_bens_nao_identificados <dbl>, pct_bens_reais <dbl>,
## #   pct_bens_financeiros <dbl>
```

``` r
# Nenhum percentual deveria passar de 100
dados_cand_bens %>% filter(if_any(starts_with("pct_"), ~ .x > 100))
```

```
## # A tibble: 209 × 38
##      ano sigla_uf sequencial_candidato n_bens patrimonio_total soma_bens_imoveis
##    <int> <chr>    <chr>                 <int>            <dbl>             <dbl>
##  1  2010 AC       10000000184              12          769745.                 0
##  2  2010 AL       20000000603               3          419970.                 0
##  3  2010 AM       40000000436               5          187455.                 0
##  4  2010 AP       30000000217               8          176064.                 0
##  5  2010 AP       30000000269               7          777889.                 0
##  6  2010 BA       50000000048               8          352720.                 0
##  7  2010 BA       50000000144              18         1572228.                 0
##  8  2010 BA       50000000673               2           28627.                 0
##  9  2010 BA       50000000744              10         2793254.                 0
## 10  2010 BA       50000000846               4          779850.                 0
## # ℹ 199 more rows
## # ℹ 32 more variables: soma_bens_imoveis_rurais <dbl>,
## #   soma_bens_veiculos <dbl>, soma_bens_participacoes <dbl>,
## #   soma_bens_liquidez <dbl>, soma_bens_aplicacoes <dbl>,
## #   soma_bens_creditos <dbl>, soma_bens_outros <dbl>,
## #   soma_bens_nao_identificados <dbl>, soma_bens_reais <dbl>,
## #   soma_bens_financeiros <dbl>, tem_bens_imoveis <int>, …
```


### Deflação


``` r
names(dados_cand_bens)
```

```
##  [1] "ano"                         "sigla_uf"                   
##  [3] "sequencial_candidato"        "n_bens"                     
##  [5] "patrimonio_total"            "soma_bens_imoveis"          
##  [7] "soma_bens_imoveis_rurais"    "soma_bens_veiculos"         
##  [9] "soma_bens_participacoes"     "soma_bens_liquidez"         
## [11] "soma_bens_aplicacoes"        "soma_bens_creditos"         
## [13] "soma_bens_outros"            "soma_bens_nao_identificados"
## [15] "soma_bens_reais"             "soma_bens_financeiros"      
## [17] "tem_bens_imoveis"            "tem_bens_imoveis_rurais"    
## [19] "tem_bens_veiculos"           "tem_bens_participacoes"     
## [21] "tem_bens_liquidez"           "tem_bens_aplicacoes"        
## [23] "tem_bens_creditos"           "tem_bens_outros"            
## [25] "tem_bens_nao_identificados"  "tem_bens_reais"             
## [27] "tem_bens_financeiros"        "pct_bens_imoveis"           
## [29] "pct_bens_imoveis_rurais"     "pct_bens_veiculos"          
## [31] "pct_bens_participacoes"      "pct_bens_liquidez"          
## [33] "pct_bens_aplicacoes"         "pct_bens_creditos"          
## [35] "pct_bens_outros"             "pct_bens_nao_identificados" 
## [37] "pct_bens_reais"              "pct_bens_financeiros"
```

``` r
bens_candidatos_gerais <- dados_cand_bens %>%
  # Deflaciona variáveis pelo IPCA de 12/2025 (2026: a partir do IPCA de 09/2026, último divulgado)
  mutate(across(c(patrimonio_total, starts_with("soma_bens_")),   # selecionar variáveis
                ~ ipca(.x, pmin(as.Date(paste0(ano, "-12-31")), as.Date("2026-09-01")), "12/2025")))
```

```
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
```

``` r
summary(bens_candidatos_gerais)
```

```
##       ano         sigla_uf         sequencial_candidato     n_bens      
##  Min.   :2010   Length:78591       Length:78591         Min.   :  1.00  
##  1st Qu.:2014   Class :character   Class :character     1st Qu.:  2.00  
##  Median :2018   Mode  :character   Mode  :character     Median :  3.00  
##  Mean   :2018                                           Mean   :  5.42  
##  3rd Qu.:2022                                           3rd Qu.:  6.00  
##  Max.   :2026                                           Max.   :278.00  
##                                                                         
##  patrimonio_total    soma_bens_imoveis   soma_bens_imoveis_rurais
##  Min.   :0.000e+00   Min.   :0.000e+00   Min.   :0.000e+00       
##  1st Qu.:1.018e+05   1st Qu.:0.000e+00   1st Qu.:0.000e+00       
##  Median :3.553e+05   Median :1.018e+05   Median :0.000e+00       
##  Mean   :1.994e+06   Mean   :7.327e+05   Mean   :1.223e+05       
##  3rd Qu.:9.887e+05   3rd Qu.:4.942e+05   3rd Qu.:0.000e+00       
##  Max.   :3.924e+09   Max.   :3.924e+09   Max.   :1.076e+09       
##                                                                  
##  soma_bens_veiculos  soma_bens_participacoes soma_bens_liquidez 
##  Min.   :        0   Min.   :0.000e+00       Min.   :   -36308  
##  1st Qu.:        0   1st Qu.:0.000e+00       1st Qu.:        0  
##  Median :    10356   Median :0.000e+00       Median :        0  
##  Mean   :    89092   Mean   :3.093e+05       Mean   :    48596  
##  3rd Qu.:    77493   3rd Qu.:0.000e+00       3rd Qu.:     3308  
##  Max.   :290725844   Max.   :1.388e+09       Max.   :119596929  
##                                                                 
##  soma_bens_aplicacoes soma_bens_creditos  soma_bens_outros   
##  Min.   :        0    Min.   :        0   Min.   :        0  
##  1st Qu.:        0    1st Qu.:        0   1st Qu.:        0  
##  Median :        0    Median :        0   Median :        0  
##  Mean   :   224528    Mean   :    52592   Mean   :   107754  
##  3rd Qu.:        0    3rd Qu.:        0   3rd Qu.:        0  
##  Max.   :684505082    Max.   :204809552   Max.   :485238886  
##                                                              
##  soma_bens_nao_identificados soma_bens_reais     soma_bens_financeiros
##  Min.   :0.000e+00           Min.   :0.000e+00   Min.   :    -1164    
##  1st Qu.:0.000e+00           1st Qu.:0.000e+00   1st Qu.:        0    
##  Median :0.000e+00           Median :1.580e+05   Median :        0    
##  Mean   :4.295e+05           Mean   :8.217e+05   Mean   :   325716    
##  3rd Qu.:0.000e+00           3rd Qu.:5.815e+05   3rd Qu.:    24402    
##  Max.   :2.792e+09           Max.   :3.924e+09   Max.   :694909220    
##                                                                       
##  tem_bens_imoveis tem_bens_imoveis_rurais tem_bens_veiculos
##  Min.   :0.0000   Min.   :0.00000         Min.   :0.0000   
##  1st Qu.:0.0000   1st Qu.:0.00000         1st Qu.:0.0000   
##  Median :1.0000   Median :0.00000         Median :1.0000   
##  Mean   :0.5769   Mean   :0.09552         Mean   :0.5317   
##  3rd Qu.:1.0000   3rd Qu.:0.00000         3rd Qu.:1.0000   
##  Max.   :1.0000   Max.   :1.00000         Max.   :1.0000   
##                                                            
##  tem_bens_participacoes tem_bens_liquidez tem_bens_aplicacoes tem_bens_creditos
##  Min.   :0.0000         Min.   :0.0000    Min.   :0.0000      Min.   :0.00000  
##  1st Qu.:0.0000         1st Qu.:0.0000    1st Qu.:0.0000      1st Qu.:0.00000  
##  Median :0.0000         Median :0.0000    Median :0.0000      Median :0.00000  
##  Mean   :0.1901         Mean   :0.3112    Mean   :0.1897      Mean   :0.06419  
##  3rd Qu.:0.0000         3rd Qu.:1.0000    3rd Qu.:0.0000      3rd Qu.:0.00000  
##  Max.   :1.0000         Max.   :1.0000    Max.   :1.0000      Max.   :1.00000  
##                                                                                
##  tem_bens_outros  tem_bens_nao_identificados tem_bens_reais  
##  Min.   :0.0000   Min.   :0.0000             Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.0000             1st Qu.:0.0000  
##  Median :0.0000   Median :0.0000             Median :1.0000  
##  Mean   :0.1096   Mean   :0.1723             Mean   :0.7427  
##  3rd Qu.:0.0000   3rd Qu.:0.0000             3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :1.0000             Max.   :1.0000  
##                                                              
##  tem_bens_financeiros pct_bens_imoveis pct_bens_imoveis_rurais
##  Min.   :0.0000       Min.   :  0.00   Min.   :  0.000        
##  1st Qu.:0.0000       1st Qu.:  0.00   1st Qu.:  0.000        
##  Median :0.0000       Median : 38.46   Median :  0.000        
##  Mean   :0.3751       Mean   : 42.55   Mean   :  3.125        
##  3rd Qu.:1.0000       3rd Qu.: 86.54   3rd Qu.:  0.000        
##  Max.   :1.0000       Max.   :100.00   Max.   :100.000        
##                       NA's   :41       NA's   :41             
##  pct_bens_veiculos pct_bens_participacoes pct_bens_liquidez 
##  Min.   :  0.000   Min.   :  0.000        Min.   : -0.7963  
##  1st Qu.:  0.000   1st Qu.:  0.000        1st Qu.:  0.0000  
##  Median :  2.451   Median :  0.000        Median :  0.0000  
##  Mean   : 20.070   Mean   :  5.214        Mean   :  6.8146  
##  3rd Qu.: 21.875   3rd Qu.:  0.000        3rd Qu.:  0.8123  
##  Max.   :100.531   Max.   :100.000        Max.   :100.0000  
##  NA's   :41        NA's   :41             NA's   :41        
##  pct_bens_aplicacoes pct_bens_creditos pct_bens_outros  
##  Min.   :  0.000     Min.   :  0.00    Min.   :  0.000  
##  1st Qu.:  0.000     1st Qu.:  0.00    1st Qu.:  0.000  
##  Median :  0.000     Median :  0.00    Median :  0.000  
##  Mean   :  4.171     Mean   :  1.09    Mean   :  2.847  
##  3rd Qu.:  0.000     3rd Qu.:  0.00    3rd Qu.:  0.000  
##  Max.   :100.000     Max.   :100.00    Max.   :100.000  
##  NA's   :41          NA's   :41        NA's   :41       
##  pct_bens_nao_identificados pct_bens_reais   pct_bens_financeiros
##  Min.   :  0.00             Min.   :  0.00   Min.   : -0.5313    
##  1st Qu.:  0.00             1st Qu.:  0.00   1st Qu.:  0.0000    
##  Median :  0.00             Median : 87.36   Median :  0.0000    
##  Mean   : 17.24             Mean   : 62.62   Mean   : 12.0753    
##  3rd Qu.:  0.00             3rd Qu.:100.00   3rd Qu.:  8.2626    
##  Max.   :100.00             Max.   :100.53   Max.   :100.0000    
##  NA's   :41                 NA's   :41       NA's   :41
```

``` r
names(bens_candidatos_gerais)
```

```
##  [1] "ano"                         "sigla_uf"                   
##  [3] "sequencial_candidato"        "n_bens"                     
##  [5] "patrimonio_total"            "soma_bens_imoveis"          
##  [7] "soma_bens_imoveis_rurais"    "soma_bens_veiculos"         
##  [9] "soma_bens_participacoes"     "soma_bens_liquidez"         
## [11] "soma_bens_aplicacoes"        "soma_bens_creditos"         
## [13] "soma_bens_outros"            "soma_bens_nao_identificados"
## [15] "soma_bens_reais"             "soma_bens_financeiros"      
## [17] "tem_bens_imoveis"            "tem_bens_imoveis_rurais"    
## [19] "tem_bens_veiculos"           "tem_bens_participacoes"     
## [21] "tem_bens_liquidez"           "tem_bens_aplicacoes"        
## [23] "tem_bens_creditos"           "tem_bens_outros"            
## [25] "tem_bens_nao_identificados"  "tem_bens_reais"             
## [27] "tem_bens_financeiros"        "pct_bens_imoveis"           
## [29] "pct_bens_imoveis_rurais"     "pct_bens_veiculos"          
## [31] "pct_bens_participacoes"      "pct_bens_liquidez"          
## [33] "pct_bens_aplicacoes"         "pct_bens_creditos"          
## [35] "pct_bens_outros"             "pct_bens_nao_identificados" 
## [37] "pct_bens_reais"              "pct_bens_financeiros"
```


## Exportação



``` r
dir.create(pasta$prod_candidatos, showWarnings = FALSE, recursive = TRUE)



write.xlsx(
  bens_candidatos_gerais,
  file      = file.path(pasta$prod_candidatos, "bens_candidatos_gerais.xlsx"),
  rowNames  = FALSE,
  overwrite = TRUE
)
```





