### 18 Dicionários dos produtos finais (Etapa 3) ----------------------------------
# Bruno Schaefer (IESP-UERJ)
#
# Objetivo: um dicionário por banco final (3 Produtos finais/bancos), com, para
# cada variável:
#   variavel           nome da coluna
#   fonte              origem do dado (fonte primária e código que o produz)
#   operacionalizacao  o que a variável mede e como foi construída
#   cobertura_anos     anos em que a variável tem pelo menos um valor preenchido
#                      (calculado nos próprios dados)
#   pct_preenchido     % de linhas com valor não ausente
#   tipo               classe da coluna (numérica, texto, data)
#
# Saída: 3 Produtos finais/dicionarios/dicionario_<banco>.xlsx (abas "banco" e
#        "variaveis") e 0_indice_dicionarios.xlsx (todos os bancos).
#
# Fonte e operacionalização vêm das regras abaixo (expressões regulares
# aplicadas ao nome da variável, na ordem; a primeira que casa vale). Variáveis
# com prefixo (ce_, diretorio_, partido_) usam a regra da variável sem o prefixo,
# precedida do contexto do prefixo. Ao criar uma variável nova nos códigos,
# acrescente uma regra: o código lista as variáveis sem descrição ao final.

library(here)
library(data.table)
library(openxlsx)

source(here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))

bancos_dir <- pasta$produtos_finais
saida <- file.path(dirname(pasta$produtos_finais), "dicionarios")
dir.create(saida, showWarnings = FALSE, recursive = TRUE)


### 1. Fontes ----------------------------------------------------------------------

F <- list(
  org   = "TSE, Órgãos partidários (dados abertos, 29/11/2025) — códigos 1_1 a 1_3",
  orgc  = "Calculada a partir de TSE, Órgãos partidários — códigos 1_1 a 1_3",
  org4  = "Calculada a partir de TSE, Órgãos partidários — código 4",
  desp  = "TSE, Prestação de contas partidárias: despesas (2017–2025) — códigos 2_1 a 2_3 e 4",
  rec   = "TSE, Prestação de contas partidárias: receitas (2017–2025) — códigos 3_1 a 3_3 e 4",
  bls   = "Zucco (2023), Brazilian Legislative Surveys (Harvard Dataverse) — código 5",
  bol   = "Bolognesi, Ribeiro e Codato (2023); Bolognesi, Ribeiro, Codato e Silva (2025) — código 5",
  sal   = "Salgado (2020), Revista de Investigações Constitucionais — código 5",
  rib   = "Ribeiro, Locatelli e Assis (2022), Dados — código 5",
  cand  = "TSE via basedosdados (br_tse_eleicoes.candidatos) — código 7",
  fin   = "TSE via basedosdados (br_tse_eleicoes.receitas_candidato e despesas_candidato) — código 6",
  bens  = "TSE via basedosdados (br_tse_eleicoes.bens_candidato) — código 8",
  res   = "TSE via basedosdados (br_tse_eleicoes.resultados_candidato) — código 9",
  vag   = "TSE via basedosdados (br_tse_eleicoes.vagas e detalhes_votacao_municipio) — código 10",
  perf  = "TSE via basedosdados (br_tse_eleicoes.perfil_eleitorado_municipio_zona) — código 10",
  censo = "IBGE, Censos Demográficos 2010 e 2022, via basedosdados (br_ibge_censo_2022.populacao_grupo_idade_sexo_raca) — código 11",
  pop   = "IBGE, estimativas populacionais, via basedosdados (br_ibge_populacao.municipio) — código 11",
  pib   = "IBGE, PIB dos Municípios, via basedosdados (br_ibge_pib.municipio) — código 11",
  cruz  = "Cruzamento dirigentes (TSE, Órgãos partidários) × candidaturas (código 12) pelo título eleitoral — código 14",
  c12   = "Calculada a partir das candidaturas (códigos 7 a 10) — código 12",
  c13   = "Calculada — código 13",
  c15   = "Calculada — código 15",
  c16   = "Calculada a partir das candidaturas (código 12) — código 16",
  c16d  = "Calculada a partir dos diretórios (código 13) — código 16",
  c17   = "Calculada a partir das candidaturas (código 12) — código 17",
  c17d  = "Cruzamento dirigentes (TSE, Órgãos partidários) × candidaturas pelo título eleitoral — código 17",
  bd    = "basedosdados (br_bd_diretorios_brasil.municipio)",
  nd    = "—"
)

# Tradução de partes de nomes usadas nos textos ({1}, {2}...)
termos <- c(
  camara = "deputado federal (câmara)", assembleia = "deputado estadual e distrital (assembleia)",
  vereanca = "vereador (vereança)", governador = "governador", senado = "senador",
  prefeitura = "prefeito", presidente = "presidente da República", governo = "governador",
  mulheres = "mulheres", negros = "pessoas negras (pretas e pardas)",
  candidatos = "candidaturas", votos = "votos nominais do 1º turno", receitas = "receitas de campanha",
  receitas_publicas = "recursos públicos de campanha (FEFC e Fundo Partidário)",
  receitas_partidarias = "receitas vindas de partidos políticos",
  receitas_direcao = "receitas vindas de direções partidárias", eleitos = "eleitos",
  genero = "de gênero (mulheres)", racial = "racial (pessoas negras)",
  imoveis = "imóveis", imoveis_rurais = "imóveis rurais", veiculos = "veículos",
  participacoes = "participações societárias", liquidez = "dinheiro em espécie e depósitos (liquidez)",
  aplicacoes = "aplicações financeiras", creditos = "créditos", outros = "outros bens",
  nao_identificados = "bens sem tipo informado", reais = "bens reais (imóveis e veículos)",
  financeiros = "bens financeiros (liquidez, aplicações e créditos)",
  pretos = "pessoas pretas", pardos = "pessoas pardas", brancos = "pessoas brancas",
  amarelos = "pessoas amarelas", indigenas = "pessoas indígenas",
  pessoas_pretas = "pessoas pretas", pessoas_pardas = "pessoas pardas", pessoas_brancas = "pessoas brancas",
  pessoas_amarelas = "pessoas amarelas", pessoas_indigenas = "pessoas indígenas",
  adm = "administração, defesa, educação e saúde públicas e seguridade social", industria = "indústria",
  agro = "agropecuária", servicos = "serviços", adespss = "administração, defesa, educação e saúde públicas e seguridade social",
  agropecuaria = "agropecuária",
  analfabeto = "analfabetos", le_escreve = "que leem e escrevem", fund_incompleto = "com fundamental incompleto",
  fund_completo = "com fundamental completo", medio_incompleto = "com médio incompleto",
  medio_completo = "com médio completo", sup_incompleto = "com superior incompleto",
  sup_completo = "com superior completo", homens = "homens"
)
tr <- function(x) if (!is.na(termos[x])) unname(termos[x]) else gsub("_", " ", x)


### 2. Regras ------------------------------------------------------------------------

R <- list()
regra <- function(padrao, fonte, oper) R[[length(R) + 1]] <<- list(padrao = padrao, fonte = fonte, oper = oper)

# Chaves e identificação -----------------------------------------------------------
regra("^(ano|ANO)$", F$nd, "Ano de referência: ano civil (direções partidárias, diretórios) ou ano da eleição (candidaturas, listas, distritos)")
regra("^SG_PARTIDO$", F$org, "Sigla do partido registrada no TSE (sem acentos)")
regra("^SG_PARTIDO_PAD$", "Calculada — função sigla_atual() (0_funcoes_auxiliares.R)", "Sigla atual padronizada do partido (PMDB = MDB, PR = PL, PRB = REPUBLICANOS etc.); chave de ligação entre as fontes")
regra("^NR_PARTIDO$", F$org, "Número do partido")
regra("^NM_PARTIDO$", F$org, "Nome do partido")
regra("^numero_partido$", F$cand, "Número do partido na eleição")
regra("^sigla_partido$", F$cand, "Sigla do partido na eleição, como registrada no TSE")
regra("^(SG_UF|sigla_uf)$", F$nd, "Sigla da Unidade da Federação (\"BR\" para o nível nacional e para presidente)")
regra("^SG_UF_REGISTRO$", F$org, "UF em que a direção nacional foi registrada no TSE (no banco nacional a UF é padronizada para \"BR\")")
regra("^CD_MUNICIPIO$", F$org, "Código TSE do município (no DF, número da zona eleitoral)")
regra("^NM_MUNICIPIO$", F$org, "Nome do município (no DF, da zona eleitoral)")
regra("^SG_UE$", F$org, "Código da unidade eleitoral no TSE")
regra("^NM_UE$", F$org, "Nome da unidade eleitoral no TSE")
regra("^id_municipio$", F$bd, "Código IBGE do município (7 dígitos)")
regra("^id_municipio_tse$", F$bd, "Código TSE do município")
regra("^cargo$", F$cand, "Cargo disputado")
regra("^tipo_eleicao$", F$cand, "Tipo de eleição (somente eleições ordinárias; inclui a eleição municipal de Macapá de 2020, adiada)")

# Órgão partidário e dirigente -----------------------------------------------------------
regra("^NM_TIPO_ORGAO_PARTIDARIO$", F$org, "Tipo de órgão partidário (definitivo, provisório, comissão interventora; comissão executiva no nível nacional). Nos diretórios, quando mais de um tipo vigorou no ano, fica o de maior tempo de atividade")
regra("^n_tipos_orgao_ano$", F$org4, "Número de tipos de órgão vigentes no partido-distrito no ano")
regra("^tipos_orgao_ano$", F$org4, "Tipos de órgão vigentes no partido-distrito no ano")
regra("^SQ_ORGAO_PARTIDARIO$", F$org, "Identificador (sequencial) do órgão partidário no TSE")
regra("^DS_TIPO_ABRANGENCIA$", F$org, "Abrangência do órgão (nacional; estadual ou regional-DF; municipal ou zonal-DF)")
regra("^DT_INICIO_VIGENCIA_ORGAO$", F$org, "Data de início da vigência do órgão")
regra("^DT_FIM_VIGENCIA_ORGAO$", F$org, "Data de fim da vigência do órgão")
regra("^DS_SITU_EXERC_ORGAO_PARTIDARIO$", F$org, "Situação de exercício do órgão")
regra("^DS_CARGO_MEMBRO$", F$org, "Cargo do dirigente no órgão (sem acentos)")
regra("^SQ_MEMBRO$", F$org, "Identificador (sequencial) do membro no TSE")
regra("^NM_MEMBRO$", F$org, "Nome do dirigente")
regra("^NR_TITULO_ELEITORAL_MEMBRO$", F$org, "Título eleitoral do dirigente")
regra("^DT_INICIO_EXERCICIO_MEMBRO$", F$org, "Data de início do exercício do cargo")
regra("^DT_FIM_EXERCICIO_MEMBRO$", F$org, "Data de fim do exercício do cargo (vazia: em exercício, considerado até 31/12/2025)")
regra("^CD_GENERO$", F$org, "Código do gênero do dirigente")
regra("^DS_GENERO$", F$org, "Gênero do dirigente (FEMININO, MASCULINO, não informado)")
regra("^DS_SITU_EXERC_MEMBRO$", F$org, "Situação de exercício do membro")
regra("^id_membro$", F$orgc, "Identificador da pessoa: título eleitoral; sem título, sequencial do membro no TSE")
regra("^dias_ativos$", F$orgc, "Dias em que a pessoa exerceu o cargo no ano (interseção do mandato com o ano civil). Versão restritiva: ≥ 183 dias; ampla: ≥ 1 dia")
regra("^setorial_mulher$", F$orgc, "1 = cargo em setorial de mulheres (cargo contém \"MULH\")")
regra("^presidente$", F$orgc, "1 = presidente do órgão (exclui vices, honorários, conselhos, fundações, juventude, setoriais, interinos, licenciados e, no nacional, presidentes estaduais com assento no diretório)")
regra("^tesoureiro$", F$orgc, "1 = tesoureiro do órgão (exclui adjuntos, suplentes, vices, vogais, licenciados, segundos e terceiros tesoureiros)")
regra("^cargo_decisao$", F$orgc, "1 = cargo de decisão (presidente ou tesoureiro)")

# Trajetória e candidaturas do dirigente -------------------------------------------------
regra("^presente_ano_anterior$", "Calculada — código 14", "1 = a pessoa estava no mesmo partido e distrito no ano anterior (NA no primeiro ano, 2015)")
regra("^mesmo_cargo_ano_anterior$", "Calculada — código 14", "1 = ocupava o mesmo cargo no ano anterior (NA em 2015)")
regra("^mudou_cargo$", "Calculada — código 14", "1 = estava no partido-distrito no ano anterior em outro cargo (NA em 2015)")
regra("^passou_a_decisao$", "Calculada — código 14", "1 = passou a cargo de decisão (presidente ou tesoureiro) vindo de cargo sem decisão no ano anterior (NA em 2015)")
regra("^foi_candidato$", F$cruz, "1 = foi candidato(a) em alguma eleição ordinária de 2010 até o ano (qualquer cargo)")
regra("^n_candidaturas$", F$cruz, "Número de candidaturas de 2010 até o ano")
regra("^ja_eleito$", F$cruz, "1 = foi eleito(a) em alguma eleição de 2010 até o ano")
regra("^candidato_no_ano$", F$cruz, "1 = candidato(a) em eleição realizada no próprio ano")
regra("^ultimo_ano_candidatura$", F$cruz, "Ano da candidatura mais recente até o ano")
regra("^ultimo_cargo_candidatura$", F$cruz, "Cargo da candidatura mais recente até o ano")
regra("^eleito_ultima_candidatura$", F$cruz, "1 = eleito(a) na candidatura mais recente")
regra("^cruzamento_candidatura$", F$cruz, "Como o dirigente foi ligado às candidaturas: \"titulo\" (título eleitoral) ou \"nome\" (nome normalizado único entre os candidatos da UF, quando falta o título)")

# Composição do diretório (códigos 1_x) ------------------------------------------------
regra("^total_membros$", F$orgc, "Número de pessoas distintas na direção no ano")
regra("^total_mulheres$", F$orgc, "Número de mulheres distintas na direção")
regra("^total_genero_ni$", F$orgc, "Número de pessoas sem gênero informado")
regra("^total_setorial_mulher$", F$orgc, "Número de pessoas em setoriais de mulheres")
regra("^setorial$", F$orgc, "1 = a direção tem setorial de mulheres")
regra("^total_presidente$", F$orgc, "Número de presidentes no ano")
regra("^mulher_presidente$", F$orgc, "Número de mulheres presidentes no ano")
regra("^total_tesoureiro$", F$orgc, "Número de tesoureiros no ano")
regra("^mulher_tesoureiro$", F$orgc, "Número de mulheres tesoureiras no ano")
regra("^total_decisao$", F$orgc, "Número de pessoas em cargos de decisão (presidente ou tesoureiro)")
regra("^mulher_decisao$", F$orgc, "Número de mulheres em cargos de decisão")
regra("^n_orgaos$", F$orgc, "Número de órgãos (SQ_ORGAO_PARTIDARIO) do tipo vigentes no ano")
regra("^dias_atividade_ano$", F$orgc, "Maior número de dias de exercício de um membro no ano (por quanto tempo o órgão funcionou)")
regra("^proporcao_mulheres$", F$orgc, "total_mulheres / total_membros")
regra("^proporcao_mulheres_decisao$", F$orgc, "mulher_decisao / total_decisao")
regra("^tem_mulher_presidente$", F$orgc, "1 = há mulher na presidência")
regra("^tem_mulher_tesoureiro$", F$orgc, "1 = há mulher na tesouraria")
regra("^tem_mulher_decisao$", F$orgc, "1 = há mulher em cargo de decisão")

# Turnover (códigos 1_x) --------------------------------------------------------------
tv <- "Turnover (anos consecutivos t-1 e t, mesmo partido, tipo de órgão e distrito; vazio no primeiro ano, 2015): "
regra("^n_membros_t$", F$orgc, paste0(tv, "pessoas na direção em t"))
regra("^n_membros_t1$", F$orgc, paste0(tv, "pessoas na direção em t-1"))
regra("^delta_membros$", F$orgc, paste0(tv, "n_membros_t - n_membros_t1"))
regra("^n_entradas$", F$orgc, paste0(tv, "pessoas presentes em t e ausentes em t-1"))
regra("^n_saidas$", F$orgc, paste0(tv, "pessoas presentes em t-1 e ausentes em t"))
regra("^n_permanencias$", F$orgc, paste0(tv, "pessoas presentes em t-1 e em t"))
regra("^turnover_abs$", F$orgc, paste0(tv, "entradas + saídas"))
regra("^turnover_prop$", F$orgc, paste0(tv, "(entradas + saídas) / pessoas presentes em t-1 ou em t"))
regra("^entradas_(mulheres|homens)$", F$orgc, paste0(tv, "entradas de {1}"))
regra("^saidas_(mulheres|homens)$", F$orgc, paste0(tv, "saídas de {1}"))
regra("^turnover_prop_(mulheres|homens)$", F$orgc, paste0(tv, "(entradas + saídas de {1}) / {1} presentes em t-1 ou em t"))
regra("^n_decisao_t$", F$orgc, paste0(tv, "pessoas em cargos de decisão em t"))
regra("^n_decisao_t1$", F$orgc, paste0(tv, "pessoas em cargos de decisão em t-1"))
regra("^delta_decisao$", F$orgc, paste0(tv, "n_decisao_t - n_decisao_t1"))
regra("^n_entradas_decisao$", F$orgc, paste0(tv, "entradas em cargos de decisão"))
regra("^n_saidas_decisao$", F$orgc, paste0(tv, "saídas de cargos de decisão"))
regra("^turnover_abs_decisao$", F$orgc, paste0(tv, "entradas + saídas nos cargos de decisão"))
regra("^turnover_prop_decisao$", F$orgc, paste0(tv, "(entradas + saídas) / ocupantes de cargos de decisão em t-1 ou em t"))
regra("^entradas_(mulher|homem)_decisao$", F$orgc, paste0(tv, "entradas de pessoas do gênero {1} em cargos de decisão"))
regra("^saidas_(mulher|homem)_decisao$", F$orgc, paste0(tv, "saídas de pessoas do gênero {1} de cargos de decisão"))
regra("^turnover_prop_(mulheres|homens)_decisao$", F$orgc, paste0(tv, "turnover proporcional dos cargos de decisão entre {1}"))
regra("^perdeu_(mulher|homem)_decisao$", F$orgc, paste0(tv, "1 = havia pessoa do gênero {1} em cargo de decisão em t-1 e não há em t"))
regra("^manteve_(mulher|homem)_decisao$", F$orgc, paste0(tv, "1 = havia pessoa do gênero {1} em cargo de decisão em t-1 e em t"))

# Candidaturas de dirigentes (diretórios, código 14) -----------------------------------------
regra("^n_dirigentes_ja_candidatos$", F$cruz, "Dirigentes (pessoas) que foram candidatos de 2010 até o ano")
regra("^n_dirigentes_ja_eleitos$", F$cruz, "Dirigentes que foram eleitos de 2010 até o ano")
regra("^n_dirigentes_candidatos_no_ano$", F$cruz, "Dirigentes candidatos em eleição do próprio ano")
regra("^n_mulheres_ja_candidatas$", F$cruz, "Mulheres dirigentes que foram candidatas de 2010 até o ano")
regra("^n_decisao_ja_candidatos$", F$cruz, "Dirigentes em cargos de decisão que foram candidatos de 2010 até o ano")

# Financiamento partidário (códigos 2_x, 3_x, 4) ----------------------------------------------
dv <- "Despesas do órgão partidário no ano (soma de VR_GASTO, R$ de dez/2025): "
regra("^desp_presente$", F$desp, "1 = o órgão tem prestação de contas de despesas no ano")
regra("^desp_valor_total_bruto$", F$desp, paste0(dv, "total bruto (inclui transferências a outras instâncias e candidatos: não somar entre níveis)"))
regra("^desp_valor_total_tipologia$", F$desp, paste0(dv, "total classificado na tipologia de Roeder (2019): aparato + eleições"))
regra("^desp_valor_mulheres$", F$desp, paste0(dv, "despesas marcadas como destinadas à promoção da participação das mulheres"))
regra("^desp_valor_aparato$", F$desp, paste0(dv, "tipologia de Roeder (2019), aparato (pessoal + sedes + política)"))
regra("^desp_valor_aparato_(pessoal|sedes|politica)$", F$desp, paste0(dv, "tipologia de Roeder (2019), aparato — {1}"))
regra("^desp_valor_eleicoes$", F$desp, paste0(dv, "tipologia de Roeder (2019), eleições (comunicação + publicidade)"))
regra("^desp_valor_eleicoes_(comunicacao|publicidade)$", F$desp, paste0(dv, "tipologia de Roeder (2019), eleições — {1}"))
regra("^desp_valor_transferencias$", F$desp, paste0(dv, "transferências a outras instâncias partidárias e a candidatos"))
regra("^desp_valor_transfer_(ordinaria|eleitoral|mulheres)$", F$desp, paste0(dv, "transferências com finalidade {1}"))
regra("^desp_valor_transfer_(.+)$", F$desp, paste0(dv, "transferências — {1}"))
regra("^desp_valor_financeiro_contabil$", F$desp, paste0(dv, "despesas financeiras e contábeis (fora da tipologia)"))
regra("^desp_valor_(.+)$", F$desp, paste0(dv, "categoria descritiva \"{1}\" (plano de contas do TSE)"))
regra("^desp_pct_mulheres$", F$desp, "desp_valor_mulheres / desp_valor_total_bruto × 100")
regra("^desp_pct_transfer_(.+)$", F$desp, "desp_valor_transfer_{r1} / desp_valor_transferencias × 100")
regra("^desp_pct_(transferencias|financeiro_contabil)$", F$desp, "desp_valor_{r1} / desp_valor_total_bruto × 100")
regra("^desp_pct_(.+)$", F$desp, "desp_valor_{r1} / desp_valor_total_tipologia × 100 (tipologia de Roeder, 2019)")
regra("^desp_check_soma_tipologia$", F$desp, "Verificação: desp_pct_aparato + desp_pct_eleicoes (deve ser 100)")
rv <- "Receitas do órgão partidário no ano (soma de VR_RECEITA, R$ de dez/2025): "
regra("^rec_presente$", F$rec, "1 = o órgão tem prestação de contas de receitas no ano")
regra("^rec_valor_total_bruto$", F$rec, paste0(rv, "total bruto"))
regra("^rec_valor_total_identificado$", F$rec, paste0(rv, "total com origem identificada"))
regra("^rec_valor_fonte_(ordinaria|eleitoral)$", F$rec, paste0(rv, "finalidade {1}"))
regra("^rec_valor_fonte_(.+)$", F$rec, paste0(rv, "fonte do recurso — {1}"))
regra("^rec_valor_origem_(.+)$", F$rec, paste0(rv, "origem da doação — {1}"))
regra("^rec_valor_recursos_(publicos|intrapartidarios|privados)$", F$rec, paste0(rv, "dimensão da origem — recursos {1}"))
regra("^rec_valor_nao_identificada$", F$rec, paste0(rv, "dimensão da origem não identificada"))
regra("^rec_pct_fonte_(.+)$", F$rec, "rec_valor_fonte_{r1} / rec_valor_total_bruto × 100")
regra("^rec_pct_(.+)$", F$rec, "rec_valor_{r1} / rec_valor_total_identificado × 100")
regra("^rec_check_soma_origens$", F$rec, "Verificação: soma dos percentuais das dimensões de origem (deve ser 100)")

# Características nacionais do partido (código 5) -----------------------------------------
regra("^bls_onda$", F$bls, "Onda do BLS aplicada ao ano (a mais recente até ele: 2009 para 2010–2012; 2013 para 2013–2016; 2017; 2018 em diante)")
regra("^bls_ideologia$", F$bls, "Ideologia do partido no BLS (IDEO_IMPUTED; -1 esquerda a 1 direita)")
regra("^bls_ideologia_estimada$", F$bls, "Ideologia estimada pelo BLS para partidos sem avaliação direta")
regra("^bls_criterio$", F$bls, "Critério da estimativa no BLS")
regra("^bolognesi_onda$", F$bol, "Onda de Bolognesi et al. aplicada ao ano (2018 até 2018; 2022 a partir de 2019)")
regra("^bolognesi_ideologia$", F$bol, "Posição ideológica do partido (0 esquerda a 10 direita)")
regra("^bolognesi_categoria$", F$bol, "Categoria ideológica (só para a onda de 2022)")
regra("^salgado_democracia_interna$", F$sal, "Índice de democracia interna do partido (constante no tempo)")
regra("^salgado_(.+)$", F$sal, "Componente do índice de democracia interna: {1} (constante no tempo)")
regra("^ribeiro_psi$", F$rib, "Índice de institucionalização do sistema partidário (PSI), média de 2011–2014, repetida em todos os anos (17 partidos)")
regra("^ribeiro_ipd$", F$rib, "Índice IPD, média de 2011–2014, repetida em todos os anos (17 partidos)")

# Distrito: eleitorado, vagas e comparecimento (código 10, regra do código 13) -----------------------
dd <- "Distrito (UF ou município) na eleição de referência: "
regra("^distrito_ano_eleicao$", F$c13, "Eleição de referência das informações de distrito: a do próprio ano, se houver, ou a do ano anterior")
regra("^distrito_aptos$", F$vag, paste0(dd, "eleitores aptos"))
regra("^distrito_comparecimento$", F$vag, paste0(dd, "comparecimento no 1º turno"))
regra("^distrito_abstencoes$", F$vag, paste0(dd, "abstenções no 1º turno"))
regra("^distrito_pct_comparecimento$", F$vag, paste0(dd, "comparecimento / aptos × 100"))
regra("^distrito_pct_abstencao$", F$vag, paste0(dd, "abstenções / aptos × 100"))
regra("^distrito_total_eleitores$", F$perf, paste0(dd, "eleitores no perfil do eleitorado"))
regra("^distrito_eleitores_mulheres$", F$perf, paste0(dd, "eleitoras no perfil do eleitorado"))
regra("^distrito_prop_(mulheres|homens)$", F$perf, paste0(dd, "proporção de {1} no eleitorado"))
regra("^distrito_prop_(.+)$", F$perf, paste0(dd, "proporção de eleitores {1}"))
regra("^distrito_vagas_(.+)$", F$vag, paste0(dd, "vagas para {1}"))
regra("^distrito_votos_validos_(.+)$", F$vag, paste0(dd, "votos válidos para {1} (1º turno)"))
regra("^distrito_pct_validos_(.+)$", F$vag, paste0(dd, "votos válidos / comparecimento × 100, {1}"))
regra("^distrito_pct_brancos_(.+)$", F$vag, paste0(dd, "votos brancos / comparecimento × 100, {1}"))
regra("^distrito_pct_nulos_(.+)$", F$vag, paste0(dd, "votos nulos / comparecimento × 100, {1}"))
regra("^distrito_eleitores_por_vaga_(.+)$", F$vag, paste0(dd, "aptos / vagas, {1}"))
regra("^distrito_vagas$", F$vag, "Vagas do cargo no distrito na eleição")
regra("^distrito_votos_validos$", F$vag, "Votos válidos do cargo no distrito (1º turno)")
regra("^distrito_pct_validos$", F$vag, "Votos válidos / comparecimento × 100 (cargo, 1º turno)")
regra("^distrito_pct_brancos$", F$vag, "Votos brancos / comparecimento × 100 (cargo, 1º turno)")
regra("^distrito_pct_nulos$", F$vag, "Votos nulos / comparecimento × 100 (cargo, 1º turno)")
regra("^distrito_eleitores_por_vaga$", F$vag, "Eleitores aptos / vagas do cargo")

# Socioeconômico (código 11) ---------------------------------------------------------------
regra("^ano_censo$", F$censo, "Censo usado na composição por sexo e cor/raça: 2010 para 2010–2015; 2022 para 2016–2025")
regra("^populacao$", F$pop, "População estimada do distrito no ano (nos anos de censo, a contagem)")
regra("^censo_populacao$", F$censo, "População no censo de referência (ano_censo)")
regra("^censo_mulheres$", F$censo, "Mulheres no censo de referência")
regra("^censo_(pessoas_.+)$", F$censo, "Número de {1} no censo de referência")
regra("^prop_negros_populacao$", F$censo, "(pessoas pretas + pardas) / população, censo de referência; parâmetro p do DRI racial")
regra("^prop_mulheres_populacao$", F$censo, "Mulheres / população, censo de referência")
regra("^prop_(pretos|pardos|brancos|amarelos|indigenas)_populacao$", F$censo, "Proporção de {1} na população, censo de referência")
regra("^pib$", F$pib, "PIB a preços correntes deflacionado pelo IPCA (R$ de dez/2025); até 2023")
regra("^impostos_liquidos$", F$pib, "Impostos líquidos de subsídios (R$ de dez/2025); até 2021")
regra("^va$", F$pib, "Valor adicionado bruto total (R$ de dez/2025); até 2021")
regra("^va_(.+)$", F$pib, "Valor adicionado bruto — {1} (R$ de dez/2025); até 2021")
regra("^pib_per_capita$", F$pib, "pib / populacao (R$ de dez/2025)")
regra("^impostos_pib$", F$pib, "impostos_liquidos / pib; até 2021")
regra("^dependencia_(.+)$", F$pib, "Participação de {1} no valor adicionado (va do setor / va); até 2021")

# Candidaturas (códigos 7 a 9 e 12) -----------------------------------------------------------
regra("^numero_candidato$", F$cand, "Número do candidato na urna")
regra("^sequencial_candidato$", F$cand, "Sequencial da candidatura no TSE (liga as fontes candidato a candidato)")
regra("^nome$", F$cand, "Nome do candidato")
regra("^nome_urna$", F$cand, "Nome de urna do candidato")
regra("^titulo_eleitoral$", F$cand, "Título eleitoral do candidato")
regra("^cpf$", F$cand, "CPF do candidato")
regra("^situacao$", F$cand, "Situação da candidatura (somente deferidas, inclusive com recurso)")
regra("^data_nascimento$", F$cand, "Data de nascimento")
regra("^idade$", F$cand, "Idade na data da eleição")
regra("^genero$", F$cand, "Gênero do candidato")
regra("^instrucao$", F$cand, "Grau de instrução")
regra("^ocupacao$", F$cand, "Ocupação declarada")
regra("^estado_civil$", F$cand, "Estado civil")
regra("^nacionalidade$", F$cand, "Nacionalidade")
regra("^sigla_uf_nascimento$", F$cand, "UF de nascimento")
regra("^municipio_nascimento$", F$cand, "Município de nascimento")
regra("^email$", F$cand, "E-mail do candidato (vazio na maior parte da base)")
regra("^raca$", F$cand, "Cor/raça autodeclarada (informada pelo TSE a partir de 2014)")
regra("^mulher$", F$c15, "1 = genero feminino")
regra("^negro$", F$c15, "1 = cor/raça preta ou parda; NA quando não informada (2010 e 2012)")
regra("^votos_1turno$", F$res, "Votos nominais no 1º turno")
regra("^resultado_1turno$", F$res, "Resultado no 1º turno (eleito, eleito por QP, eleito por média, suplente, não eleito, 2º turno)")
regra("^eleito_1turno$", F$res, "Eleito no 1º turno (\"eleito\" inclui eleito por QP e por média)")
regra("^votos_2turno$", F$res, "Votos no 2º turno (presidente, governador e prefeito)")
regra("^eleito_2turno$", F$res, "Eleito no 2º turno")
regra("^disputou_2turno$", F$res, "1 = disputou o 2º turno")
regra("^eleito_final$", F$res, "Resultado final da eleição (após o 2º turno; quando falta, o mais votado)")
regra("^incumbente$", F$res, "1 = eleito(a) para o mesmo cargo e distrito na eleição anterior (Senado: 4 ou 8 anos antes); pelo título eleitoral; NA sem título")
regra("^votos_nominais_distrito$", F$c12, "Soma dos votos nominais do cargo no distrito (1º turno)")
regra("^votos_partido_distrito$", F$c12, "Soma dos votos nominais dos candidatos do mesmo partido no cargo-distrito (1º turno; sem votos de legenda)")
regra("^pct_votos_distrito$", F$c12, "Votos do candidato / votos válidos do cargo-distrito × 100 (presidente: sobre os votos nominais)")
regra("^pct_votos_partido$", F$c12, "Votos do candidato / votos_partido_distrito × 100 (votos de legenda não entram)")

# Bens (código 8) --------------------------------------------------------------------------
bv <- "Bens declarados pelo candidato (R$ de dez/2025): "
regra("^n_bens$", F$bens, "Número de bens declarados")
regra("^patrimonio_total$", F$bens, paste0(bv, "soma dos bens"))
regra("^tem_declaracao_bens$", F$bens, "1 = declarou bens")
regra("^soma_bens_(.+)$", F$bens, paste0(bv, "valor em {1} (o TSE não informa o tipo do bem em 2010)"))
regra("^tem_bens_(.+)$", F$bens, "1 = declarou {1}")
regra("^pct_bens_(.+)$", F$bens, "soma_bens_{r1} / patrimonio_total × 100")

# Financiamento eleitoral (código 6) ---------------------------------------------------------
fv <- "Receitas de campanha do candidato (R$ de dez/2025): "
regra("^soma_receitas$", F$fin, paste0(fv, "total"))
regra("^soma_recursos_publicos$", F$fin, paste0(fv, "recursos públicos (fonte FEFC ou Fundo Partidário)"))
regra("^soma_recursos_pessoa_fisica$", F$fin, paste0(fv, "origem pessoas físicas"))
regra("^soma_recursos_pessoa_juridica$", F$fin, paste0(fv, "origem pessoas jurídicas (até 2014)"))
regra("^soma_recursos_proprios$", F$fin, paste0(fv, "recursos próprios"))
regra("^soma_recursos_partidos$", F$fin, paste0(fv, "origem partido político"))
regra("^soma_recursos_outros_candidatos$", F$fin, paste0(fv, "origem outros candidatos/comitês"))
regra("^soma_recursos_partidos_candidatos$", F$fin, paste0(fv, "partidos ou outros candidatos"))
regra("^soma_recursos_direcao_(nacional|estadual|municipal)$", F$fin, paste0(fv, "doações da direção partidária {1} (identificada pelo nome do doador)"))
regra("^soma_recursos_direcao_partidaria$", F$fin, paste0(fv, "doações de qualquer direção partidária (identificada pelo nome do doador)"))
regra("^soma_recursos_publicos_(nacional|estadual|municipal)$", F$fin, paste0(fv, "recursos públicos repassados pela direção {1}"))
regra("^pct_recursos_(.+)$", F$fin, "soma_recursos_{r1} / soma_receitas × 100")
regra("^tem_financiamento$", F$fin, "1 = o candidato tem receitas ou despesas declaradas")
dsv <- "Despesas de campanha do candidato (R$ de dez/2025): "
regra("^despesas_total_can$", F$fin, paste0(dsv, "total contratado"))
regra("^despesas_internet_can$", F$fin, paste0(dsv, "internet (páginas e impulsionamento de conteúdo)"))
regra("^despesas_(tradicional|moderno|organizacao|outras)_can$", F$fin, paste0(dsv, "tipo de gasto {1} (classificação do código 6)"))
regra("^pct_despesas_(.+)$", F$fin, "despesas_{r1}_can / despesas_total_can × 100")

# Distritos (código 16) -------------------------------------------------------------------------
regra("^n_candidatos_(camara|assembleia|vereanca)$", F$c16, "Número de candidaturas deferidas para {1} no distrito")
regra("^n_eleitos_(camara|assembleia|vereanca)$", F$c16, "Número de eleitos para {1} no distrito")
regra("^prop_(candidatos|receitas|receitas_partidarias|receitas_direcao|eleitos)_(mulheres|negros)_(camara|assembleia|vereanca)$",
      F$c16, "Participação de {2} em {1} para {3} (entre candidaturas com gênero/raça informados)")
regra("^denominador_mulheres$", F$perf, "Parâmetro p do DRI de gênero: proporção de mulheres no eleitorado do distrito")
regra("^denominador_negros$", F$censo, "Parâmetro p do DRI racial: proporção de pessoas negras (pretas e pardas) na população do distrito")
regra("^indice_(genero|racial)_(candidatos|receitas|receitas_partidarias|receitas_direcao|eleitos)_(camara|assembleia|vereanca)$",
      F$c16, "Índice de equidade {1} em {2} para {3}. DRI(b, p) = (b - p) / sqrt((b - p)^2 + b(1 - b)), b = participação do grupo, p = denominador; -1 a 1, 0 = paridade")
regra("^tem_mulher_eleita_(governo|prefeitura)$", F$c16, "1 = mulher eleita para {1} no distrito")
regra("^tem_negro_eleito_(governo|prefeitura)$", F$c16, "1 = pessoa negra eleita para {1} no distrito (NA sem cor/raça)")
regra("^partidos_n_diretorios$", F$c16d, "Partidos com direção no distrito no ano da eleição")
regra("^partidos_total_dirigentes$", F$c16d, "Soma dos dirigentes das direções partidárias do distrito")
regra("^partidos_total_decisao$", F$c16d, "Soma dos cargos de decisão das direções do distrito")
regra("^partidos_total_mulheres$", F$c16d, "Soma das mulheres nas direções do distrito")
regra("^partidos_mulheres_decisao$", F$c16d, "Soma das mulheres em cargos de decisão nas direções do distrito")
regra("^partidos_prop_mulheres_decisao$", F$c16d, "partidos_mulheres_decisao / partidos_total_decisao")

# Listas partidárias (código 17) -----------------------------------------------------------------
lv <- "Lista do partido no distrito: "
regra("^n_candidatos$", F$c17, paste0(lv, "candidaturas deferidas"))
regra("^n_mulheres$", F$c17, paste0(lv, "candidaturas de mulheres"))
regra("^n_negros$", F$c17, paste0(lv, "candidaturas de pessoas negras (NA sem cor/raça)"))
regra("^n_raca_informada$", F$c17, paste0(lv, "candidaturas com cor/raça informada"))
regra("^n_eleitos$", F$c17, paste0(lv, "eleitos"))
regra("^n_eleitas_mulheres$", F$c17, paste0(lv, "mulheres eleitas"))
regra("^n_eleitos_negros$", F$c17, paste0(lv, "pessoas negras eleitas"))
regra("^n_incumbentes$", F$c17, paste0(lv, "candidaturas de incumbentes"))
regra("^n_mulheres_incumbentes$", F$c17, paste0(lv, "candidaturas de mulheres incumbentes"))
regra("^n_dirigentes_candidatos$", F$c17d, paste0(lv, "candidaturas de dirigentes do partido no distrito no ano (diretório estadual; municipal na vereança); NA antes de 2016"))
regra("^n_mulheres_dirigentes_candidatas$", F$c17d, paste0(lv, "candidaturas de mulheres dirigentes do partido no distrito"))
regra("^n_dirigentes_decisao_candidatos$", F$c17d, paste0(lv, "candidaturas de dirigentes em cargos de decisão"))
regra("^n_dirigentes_eleitos$", F$c17d, paste0(lv, "dirigentes do partido no distrito eleitos"))
regra("^n_dirigentes_nacionais_candidatos$", F$c17d, paste0(lv, "candidaturas de membros da direção nacional do partido"))
regra("^idade_media$", F$c17, paste0(lv, "idade média das candidaturas"))
regra("^prop_superior_completo$", F$c17, paste0(lv, "proporção de candidaturas com ensino superior completo"))
regra("^cumpre_cota_genero$", F$c17, paste0(lv, "1 = mulheres são ≥ 30% das candidaturas deferidas (Lei 9.504/1997, art. 10, § 3º)"))
regra("^votos_nominais$", F$c17, paste0(lv, "soma dos votos nominais no 1º turno (sem votos de legenda)"))
regra("^pct_votos_nominais_distrito$", F$c17, paste0(lv, "votos_nominais / votos_nominais_distrito × 100"))
regra("^candidatos_por_vaga$", F$c17, paste0(lv, "n_candidatos / distrito_vagas"))
regra("^pct_cadeiras$", F$c17, paste0(lv, "n_eleitos / distrito_vagas × 100"))
regra("^pct_candidatos_distrito$", F$c17, paste0(lv, "n_candidatos / candidaturas do cargo no distrito × 100"))
regra("^n_partidos_distrito$", F$c17, "Número de partidos com lista no cargo-distrito")
regra("^n_com_financiamento$", F$c17, paste0(lv, "candidaturas com receitas ou despesas declaradas"))
regra("^despesas_total$", F$c17, paste0(lv, "soma das despesas de campanha (R$ de dez/2025)"))
regra("^patrimonio_mediano$", F$c17, paste0(lv, "mediana do patrimônio declarado"))
regra("^receitas$", F$c17, paste0(lv, "soma das receitas de campanha (R$ de dez/2025)"))
regra("^recursos_(.+)$", F$c17, paste0(lv, "soma de soma_recursos_{r1} dos candidatos (R$ de dez/2025)"))
regra("^prop_(candidatos|votos|receitas|receitas_publicas|receitas_partidarias|receitas_direcao|eleitos)_(mulheres|negros)$",
      F$c17, paste0(lv, "participação de {2} em {1} (entre candidaturas com gênero/raça informados; NA se o total é zero)"))
regra("^indice_(genero|racial)_(candidatos|votos|receitas|receitas_publicas|receitas_partidarias|receitas_direcao|eleitos)$",
      F$c17, paste0(lv, "índice de equidade {1} em {2}. DRI(b, p) = (b - p) / sqrt((b - p)^2 + b(1 - b)), b = participação do grupo, p = denominador; -1 a 1, 0 = paridade"))
mj <- "Candidatura majoritária própria do partido ({1}) no mesmo distrito e eleição: "
regra("^(governador|senado|presidente|prefeitura)_tem_candidato$", F$c17, paste0(mj, "1 = lançou candidatura (cabeça de chapa); apoio a outro partido não é identificado"))
regra("^(governador|senado|presidente|prefeitura)_n_candidatos$", F$c17, paste0(mj, "número de candidaturas (Senado: até 2 quando há duas vagas)"))
regra("^(governador|senado|presidente|prefeitura)_nome_urna$", F$cand, paste0(mj, "nome de urna (separados por \";\")"))
regra("^(governador|senado|presidente|prefeitura)_genero$", F$cand, paste0(mj, "gênero (separados por \";\")"))
regra("^(governador|senado|presidente|prefeitura)_raca$", F$cand, paste0(mj, "cor/raça (separadas por \";\"; não informada em 2010 e 2012)"))
regra("^(governador|senado|presidente|prefeitura)_n_mulheres$", F$c17, paste0(mj, "número de mulheres"))
regra("^(governador|senado|presidente|prefeitura)_n_negros$", F$c17, paste0(mj, "número de pessoas negras"))
regra("^(governador|senado|presidente|prefeitura)_n_eleitos$", F$res, paste0(mj, "número de eleitos"))
regra("^(governador|senado|presidente|prefeitura)_eleita_mulher$", F$res, paste0(mj, "1 = mulher eleita"))
regra("^(governador|senado|presidente|prefeitura)_eleito_negro$", F$res, paste0(mj, "1 = pessoa negra eleita"))
regra("^(governador|senado|presidente|prefeitura)_incumbente$", F$res, paste0(mj, "1 = candidato incumbente"))
regra("^(governador|senado|presidente|prefeitura)_disputou_2turno$", F$res, paste0(mj, "1 = disputou o 2º turno"))
regra("^(governador|senado|presidente|prefeitura)_pct_votos$", F$c12, paste0(mj, "participação nos votos válidos do cargo-distrito no 1º turno (soma, se mais de uma)"))

# Prefixos: contexto por banco
contexto <- function(prefixo, banco) {
  switch(prefixo,
    ce_        = "Comissão Executiva nacional do partido no ano — ",
    partido_   = "Característica nacional do partido no ano — ",
    diretorio_ = if (grepl("^bd_dirigentes", banco)) "Direção (órgão) do dirigente no ano — "
                 else if (grepl("^bd_candidatos", banco)) "Direção do partido no distrito da candidatura no ano da eleição (estadual; municipal nas municipais) — "
                 else "Diretório do partido no distrito no ano da eleição (bd_diretorios_*) — ")
}

descrever_base <- function(v) {
  for (r in R) {
    m <- regmatches(v, regexec(r$padrao, v))[[1]]
    if (length(m)) {
      oper <- r$oper
      fonte <- r$fonte
      if (length(m) > 1) for (i in 2:length(m)) {
        oper <- gsub(paste0("{r", i - 1, "}"), m[i], oper, fixed = TRUE)
        oper <- gsub(paste0("{", i - 1, "}"), tr(m[i]), oper, fixed = TRUE)
      }
      return(list(fonte = fonte, oper = oper))
    }
  }
  NULL
}

descrever <- function(v, banco) {
  d <- descrever_base(v)
  if (!is.null(d)) return(d)
  for (p in c("ce_", "diretorio_", "partido_")) if (startsWith(v, p)) {
    d <- descrever_base(substring(v, nchar(p) + 1))
    if (!is.null(d)) return(list(fonte = d$fonte, oper = paste0(contexto(p, banco), d$oper)))
  }
  list(fonte = NA_character_, oper = NA_character_)
}


### 3. Cobertura -------------------------------------------------------------------------

# Contagem de valores preenchidos por ano e variável
contar <- function(x, col_ano) {
  x[, lapply(.SD, \(z) if (is.character(z)) sum(!is.na(z) & z != "") else sum(!is.na(z))), by = c(col_ano)]
}

fmt_anos <- function(a) {
  a <- sort(unique(a))
  if (!length(a)) return("sem dados")
  g <- cumsum(c(1, diff(a) != 1))
  paste(vapply(split(a, g), \(z) if (length(z) == 1) as.character(z) else paste0(z[1], "–", z[length(z)]), ""),
        collapse = ", ")
}

tipo_coluna <- function(z) {
  if (inherits(z, c("Date", "POSIXct", "IDate"))) "data"
  else if (is.numeric(z) || is.logical(z)) "numérica"
  else "texto"
}

# Lê um banco (xlsx, csv ou pasta de csv) e devolve contagens, linhas e tipos
ler_contagens <- function(caminho) {
  arquivos <- if (dir.exists(caminho)) list.files(caminho, pattern = "\\.csv$", full.names = TRUE) else caminho
  partes <- lapply(arquivos, function(a) {
    if (grepl("\\.xlsx$", a)) {
      x <- as.data.table(read.xlsx(a, detectDates = TRUE))
      col_ano <- intersect(c("ano", "ANO"), names(x))[1]
      list(cont = contar(x, col_ano), tipos = vapply(x, tipo_coluna, ""), col_ano = col_ano, cols = names(x), n = nrow(x))
    } else {
      cols <- names(fread(a, sep = ";", nrows = 0))
      col_ano <- intersect(c("ano", "ANO"), cols)[1]
      lotes <- split(setdiff(cols, col_ano), ceiling(seq_along(setdiff(cols, col_ano)) / 40))
      conts <- list(); tipos <- c(); n <- 0L
      for (l in lotes) {
        x <- fread(a, sep = ";", select = c(col_ano, l), na.strings = c("", "NA"), showProgress = FALSE)
        n <- nrow(x)
        conts[[length(conts) + 1]] <- contar(x, col_ano)
        tipos <- c(tipos, vapply(x[, ..l], tipo_coluna, ""))
        rm(x); gc()
      }
      cont <- Reduce(\(a, b) merge(a, b, by = col_ano), conts)
      list(cont = cont, tipos = c(setNames("numérica", col_ano), tipos), col_ano = col_ano, cols = cols, n = n)
    }
  })
  col_ano <- partes[[1]]$col_ano
  cols <- unique(unlist(lapply(partes, `[[`, "cols")))
  cont <- rbindlist(lapply(partes, `[[`, "cont"), use.names = TRUE, fill = TRUE)
  tipos <- unlist(lapply(partes, `[[`, "tipos")); tipos <- tipos[!duplicated(names(tipos))]
  list(cont = cont, col_ano = col_ano, cols = cols, linhas = sum(vapply(partes, `[[`, 0, "n")), tipos = tipos)
}


### 4. Bancos ----------------------------------------------------------------------------

bancos <- rbindlist(list(
  list("bd_dirigentes_nacional", "bd_dirigentes_nacional.xlsx", "ano-dirigente-cargo (direção nacional; versão restritiva)", "14"),
  list("bd_dirigentes_estadual", "bd_dirigentes_estadual.xlsx", "ano-dirigente-cargo-UF (direções estaduais e regional do DF; versão restritiva)", "14"),
  list("bd_dirigentes_municipal", "bd_dirigentes_municipal", "ano-dirigente-cargo-município (direções municipais e zonais do DF; versão ampla; um arquivo por ano)", "14"),
  list("bd_diretorios_nacional", "bd_diretorios_nacional.xlsx", "ano-partido (órgão de direção nacional; Comissão Executiva com prefixo ce_)", "14"),
  list("bd_diretorios_estadual", "bd_diretorios_estadual.xlsx", "ano-partido-UF", "14"),
  list("bd_diretorios_municipal", "bd_diretorios_municipal.csv", "ano-partido-município", "14"),
  list("bd_candidatos_governadoria", "bd_candidatos_governadoria.xlsx", "candidatura a governador (eleições gerais)", "15"),
  list("bd_candidatos_senado", "bd_candidatos_senado.xlsx", "candidatura a senador (eleições gerais)", "15"),
  list("bd_candidatos_camara", "bd_candidatos_camara.xlsx", "candidatura a deputado federal (eleições gerais)", "15"),
  list("bd_candidatos_assembleia", "bd_candidatos_assembleia.xlsx", "candidatura a deputado estadual ou distrital (eleições gerais)", "15"),
  list("bd_candidatos_prefeitura", "bd_candidatos_prefeitura.xlsx", "candidatura a prefeito (eleições municipais)", "15"),
  list("bd_candidatos_vereanca", "bd_candidatos_vereanca.csv", "candidatura a vereador (eleições municipais)", "15"),
  list("bd_partidos_camara", "bd_partidos_camara.xlsx", "ano-partido-UF: lista do partido para deputado federal", "17"),
  list("bd_partidos_assembleias", "bd_partidos_assembleias.xlsx", "ano-partido-UF: lista do partido para deputado estadual ou distrital", "17"),
  list("bd_partidos_vereanca", "bd_partidos_vereanca.csv", "ano-partido-município: lista do partido para vereador", "17"),
  list("bd_distritos_estadual", "bd_distritos_estadual.xlsx", "ano-UF (eleições gerais)", "16"),
  list("bd_distritos_municipal", "bd_distritos_municipal.xlsx", "ano-município (eleições municipais)", "16")
))
setnames(bancos, c("banco", "arquivo", "unidade", "codigo"))

estilo_cab <- createStyle(textDecoration = "bold", fgFill = "#DDEBF7", border = "bottom", wrapText = TRUE)
estilo_txt <- createStyle(wrapText = TRUE, valign = "top")

indice <- list()
sem_regra <- list()

for (i in seq_len(nrow(bancos))) {
  b <- bancos[i]
  caminho <- file.path(bancos_dir, b$arquivo)
  if (!file.exists(caminho)) { message("Banco não encontrado: ", b$arquivo); next }
  cat("Dicionário:", b$banco, "\n")
  cc <- ler_contagens(caminho)
  anos_banco <- sort(cc$cont[[cc$col_ano]])
  total <- colSums(cc$cont[, setdiff(names(cc$cont), cc$col_ano), with = FALSE], na.rm = TRUE)
  total[cc$col_ano] <- cc$linhas

  dic <- rbindlist(lapply(cc$cols, function(v) {
    d <- descrever(v, b$banco)
    anos_v <- if (v == cc$col_ano) anos_banco else {
      z <- cc$cont[[v]]; cc$cont[[cc$col_ano]][!is.na(z) & z > 0]
    }
    data.table(variavel = v, fonte = d$fonte, operacionalizacao = d$oper,
               cobertura_anos = fmt_anos(anos_v),
               pct_preenchido = round(100 * unname(total[v]) / cc$linhas, 1),
               tipo = unname(cc$tipos[v]))
  }))
  if (any(is.na(dic$operacionalizacao))) sem_regra[[b$banco]] <- dic[is.na(operacionalizacao), variavel]

  meta <- data.table(
    campo = c("banco", "arquivo", "unidade de análise", "linhas", "anos", "variáveis", "código que produz",
              "documentação", "gerado em"),
    valor = c(b$banco, file.path("3 Produtos finais/bancos", b$arquivo), b$unidade, format(cc$linhas, big.mark = "."),
              fmt_anos(anos_banco), nrow(dic), paste0("3 Produtos finais/", b$codigo, " Produtos finais"),
              "0 Funções, estrutura e passo-a-passo/0 Estrutura de Montagem do Banco de Dados.Rmd",
              format(Sys.Date(), "%d/%m/%Y"))
  )

  wb <- createWorkbook()
  addWorksheet(wb, "banco"); addWorksheet(wb, "variaveis")
  writeData(wb, "banco", meta, headerStyle = estilo_cab)
  setColWidths(wb, "banco", 1:2, c(22, 100))
  writeData(wb, "variaveis", dic, headerStyle = estilo_cab)
  addStyle(wb, "variaveis", estilo_txt, rows = 2:(nrow(dic) + 1), cols = 1:6, gridExpand = TRUE)
  setColWidths(wb, "variaveis", 1:6, c(38, 45, 90, 22, 14, 10))
  freezePane(wb, "variaveis", firstRow = TRUE, firstCol = TRUE)
  saveWorkbook(wb, file.path(saida, paste0("dicionario_", b$banco, ".xlsx")), overwrite = TRUE)

  indice[[b$banco]] <- data.table(banco = b$banco, arquivo = b$arquivo, unidade = b$unidade,
                                  linhas = cc$linhas, anos = fmt_anos(anos_banco), variaveis = nrow(dic),
                                  codigo = b$codigo, dicionario = paste0("dicionario_", b$banco, ".xlsx"))
  rm(cc, dic); gc()
}

indice <- rbindlist(indice)
wb <- createWorkbook(); addWorksheet(wb, "bancos")
writeData(wb, "bancos", indice, headerStyle = estilo_cab)
setColWidths(wb, "bancos", 1:8, c(28, 34, 70, 12, 26, 10, 8, 40))
saveWorkbook(wb, file.path(saida, "0_indice_dicionarios.xlsx"), overwrite = TRUE)
print(indice[, .(banco, linhas, anos, variaveis)])

if (length(sem_regra)) {
  cat("\nVariáveis sem regra de descrição (acrescentar em 18 Dicionários.R):\n")
  for (n in names(sem_regra)) cat(" ", n, ":", paste(sem_regra[[n]], collapse = ", "), "\n")
} else cat("\nTodas as variáveis têm descrição.\n")

cat("\nDicionários salvos em:", saida, "\n")
