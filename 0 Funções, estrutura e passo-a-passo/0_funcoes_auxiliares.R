# Funções auxiliares compartilhadas pelos códigos de montagem do banco
# Bruno Schaefer (IESP-UERJ)
#
# Uso: source(here::here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))
# Carrega também as pastas do projeto (objeto `pasta`, ver 0_pastas.R).

library(data.table)
library(stringi)

source(here::here("0 Funções, estrutura e passo-a-passo", "0_pastas.R"))

# Leitura robusta dos CSVs do TSE ---------------------------------------------
#
# Os arquivos de prestação de contas partidárias do TSE trazem registros com
# aspas internas não escapadas (ex.: "NELSON DALL" AGNESE") e com quebras de
# linha dentro de campos de nome. O readr::read_delim interpreta essas aspas
# como abertura de campo e engole centenas de milhares de linhas sem erro
# (ex.: receitas 2017 — 640.084 linhas no arquivo, 291.899 lidas). O fread
# também para no meio de alguns arquivos.
#
# Como todos os campos vêm entre aspas, o separador real é a sequência literal
# ";" (aspas-ponto e vírgula-aspas). A função:
#   1. lê as linhas cruas em Latin-1;
#   2. remonta os registros quebrados por quebra de linha até que tenham o
#      número de separadores do cabeçalho;
#   3. quebra cada registro pelo separador literal ";";
#   4. remove aspas remanescentes e espaços nas pontas.
# Registros com separadores a mais (campo contendo ";" literal) são
# descartados e contabilizados na mensagem de diagnóstico.

ler_csv_tse <- function(arquivo) {

  linhas <- readLines(arquivo, encoding = "latin1", warn = FALSE)
  linhas <- enc2utf8(linhas)
  linhas <- linhas[nzchar(linhas)]

  # Arquivos regravados no Excel perdem as aspas (e os CNPJs viram notação
  # científica). A leitura depende das aspas, então o arquivo é recusado.
  if (!startsWith(linhas[1], '"')) {
    stop(basename(arquivo), " não está no formato original do TSE (campos sem aspas; ",
         "provavelmente foi regravado no Excel). Extraia de novo o arquivo do .zip do TSE.")
  }

  cabecalho <- stri_split_fixed(stri_replace_all_regex(linhas[1], '^"|"$', ""), '";"')[[1]]
  n_sep     <- length(cabecalho) - 1L
  linhas    <- linhas[-1]

  # Remontar registros quebrados ----------------------------------------------
  cont <- stri_count_fixed(linhas, '";"')
  n_remontados <- 0L

  if (any(cont < n_sep)) {
    manter <- rep(TRUE, length(linhas))
    for (i in which(cont < n_sep)) {
      if (!manter[i]) next
      j <- i
      s <- cont[i]
      while (s < n_sep && j < length(linhas)) {
        j <- j + 1L
        linhas[i] <- paste(linhas[i], linhas[j])
        s <- s + cont[j]
        manter[j] <- FALSE
      }
      cont[i] <- s
      n_remontados <- n_remontados + 1L
    }
    linhas <- linhas[manter]
    cont   <- cont[manter]
  }

  # Descartar registros com separadores a mais --------------------------------
  n_descartados <- sum(cont != n_sep)
  linhas <- linhas[cont == n_sep]

  # Quebrar pelos separadores -------------------------------------------------
  m  <- stri_split_fixed(linhas, '";"', simplify = TRUE)
  dt <- as.data.table(m)
  setnames(dt, cabecalho)

  for (col in cabecalho) {
    set(dt, j = col, value = stri_trim_both(stri_replace_all_fixed(dt[[col]], '"', "")))
  }

  message(
    basename(arquivo), ": ", nrow(dt), " registros lidos",
    " | remontados: ", n_remontados,
    " | descartados: ", n_descartados
  )

  dt[]
}

# Conversão de valores monetários do TSE ---------------------------------------
# Os valores vêm com vírgula decimal e sem separador de milhar ("1234,56").

converter_valor_tse <- function(x) {
  as.numeric(stri_replace_all_fixed(stri_replace_all_fixed(x, ".", ""), ",", "."))
}

# Padronização de nomes de municípios ------------------------------------------
# Maiúsculas, sem acentos, sem pontuação e com espaços simples. Usada para
# casar os municípios das prestações de contas com os do banco de dirigentes.

normalizar_nome <- function(x) {
  x <- toupper(stri_trans_general(x, "Latin-ASCII"))
  x <- stri_replace_all_regex(x, "[^A-Z0-9 ]", " ")
  stri_trim_both(stri_replace_all_regex(x, "\\s+", " "))
}

# Padronização de siglas partidárias -------------------------------------------
#
# Os bancos de dirigentes (códigos 1_x) removem acentos de todas as colunas de
# texto; os bancos de financiamento (códigos 2_x e 3_x) mantêm a grafia do TSE.
# Além disso, algumas siglas são grafadas de forma diferente nas duas fontes.
# Esta função produz uma sigla comum para as junções do código 4.

padronizar_sigla <- function(x) {
  x <- toupper(stri_trans_general(x, "Latin-ASCII"))
  x <- stri_trim_both(x)
  fcase(
    x == "PC DO B",      "PCDOB",
    x == "SDD",          "SOLIDARIEDADE",
    x == "O DEMOCRATA",  "PMB",
    default = x
  )
}

# Sigla atual do partido ---------------------------------------------------------
# As fontes usam a sigla da época (PMDB, PR, PRB, PPS...). Os bancos de
# dirigentes e de financiamento partidário usam a sigla atual em todos os
# anos. Esta função converte qualquer sigla para a sigla atual padronizada.
# Partidos que se fundiram (DEM, PSL, PHS, PPL, PRP, PTB, Patriota, PROS, PSC)
# mantêm a própria sigla.

sigla_atual <- function(x) {
  x <- padronizar_sigla(x)   # remove acentos; PC DO B -> PCDOB; SDD -> SOLIDARIEDADE
  fcase(
    x %in% c("PMDB"),             "MDB",
    x %in% c("PR"),               "PL",
    x %in% c("PRB", "REP"),       "REPUBLICANOS",
    x %in% c("PPS", "CDD"),       "CIDADANIA",
    x %in% c("PEN", "PATRI"),     "PATRIOTA",
    x %in% c("PTN"),              "PODE",
    x %in% c("PT DO B", "PTDOB"), "AVANTE",
    x %in% c("PSDC"),             "DC",
    x %in% c("SD"),               "SOLIDARIEDADE",
    x %in% c("PTC"),              "AGIR",
    x %in% c("PROGRE"),           "PP",
    x %in% c("PMN"),              "MOBILIZA",
    default = x
  )
}

# Validação dos bancos da basedosdados contra os arquivos do TSE -----------------
#
# Usadas nos códigos "<n> ... validação.Rmd". Cada validação compara, para uma
# amostra (um ano e uma UF ou cargo), o banco produzido a partir da
# basedosdados com o arquivo bruto do Portal de Dados Abertos do TSE.

# Lê um arquivo de dentro de um .zip do TSE (Latin-1, ";"). Os arquivos das
# eleições têm campos numéricos sem aspas, por isso a leitura é com fread.
ler_zip_tse <- function(zip, arquivo, select = NULL) {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  utils::unzip(zip, files = arquivo, exdir = tmp)
  fread(file.path(tmp, arquivo), sep = ";", encoding = "Latin-1",
        colClasses = "character", select = select, showProgress = FALSE)
}

# Texto no formato da basedosdados: minúsculas, sem acentos, espaços simples
texto_bd <- function(x) {
  stri_trim_both(stri_replace_all_regex(tolower(stri_trans_general(x, "Latin-ASCII")), "\\s+", " "))
}

# Compara uma variável numérica entre o banco produzido e o TSE.
#   dados: tabela já juntada, com as colunas `saida` e `tse` (NA = ausente)
#   tolerancia: diferença relativa aceita
# Retorna invisivelmente a tabela com a diferença relativa e imprime o resumo.
comparar_valores <- function(dados, rotulo, tolerancia = 0.01) {
  d <- as.data.table(dados)
  d[, dif_rel := fifelse(pmax(abs(saida), abs(tse)) > 0,
                         abs(saida - tse) / pmax(abs(saida), abs(tse)), 0)]
  so_saida <- d[!is.na(saida) & is.na(tse), .N]
  so_tse   <- d[is.na(saida) & !is.na(tse), .N]
  ambos    <- d[!is.na(saida) & !is.na(tse)]
  dentro   <- ambos[dif_rel <= tolerancia, .N]
  pct      <- if (nrow(ambos) > 0) 100 * dentro / nrow(ambos) else NA_real_
  total_s  <- sum(ambos$saida)
  total_t  <- sum(ambos$tse)

  cat("\n---", rotulo, "---\n")
  cat("Casos nos dois bancos:", nrow(ambos),
      "| só no banco produzido:", so_saida, "| só no TSE:", so_tse, "\n")
  cat("Casos com diferença <= ", 100 * tolerancia, "%: ", dentro, " (", round(pct, 2), "%)\n", sep = "")
  cat("Total produzido:", format(total_s, big.mark = "."), "| total TSE:", format(total_t, big.mark = "."),
      "| diferença:", round(100 * (total_s - total_t) / total_t, 3), "%\n")
  cat("Veredito:", if (!is.na(pct) && pct >= 99 && abs(total_s - total_t) / total_t <= tolerancia)
    "OK" else "ATENÇÃO — verificar os casos discrepantes", "\n")

  invisible(d[order(-dif_rel)])
}

# Leitura dos bancos produzidos ----------------------------------------------------
# Lê um banco salvo em .csv (separador ";") ou .xlsx. No .csv, códigos e
# identificadores são lidos como texto para preservar zeros à esquerda.
ler_banco <- function(caminho) {
  if (grepl("\\.csv$", caminho)) {
    cols <- names(fread(caminho, sep = ";", nrows = 0))
    chr  <- intersect(c("id_municipio", "id_municipio_tse", "numero", "numero_candidato",
                        "numero_partido", "sequencial", "sequencial_candidato",
                        "titulo_eleitoral", "cpf", "CD_MUNICIPIO"), cols)
    # fwrite grava valores ausentes como campo vazio: ler de volta como NA
    fread(caminho, sep = ";", colClasses = list(character = chr),
          na.strings = c("", "NA"), showProgress = FALSE)
  } else {
    as.data.table(openxlsx::read.xlsx(caminho))
  }
}

# Índice de Representação Descritiva (DRI) ------------------------------------------
# DRI(b, p) = (b - p) / sqrt((b - p)^2 + b (1 - b)), modificação do índice de
# Ransom e Sutch (2001) (ver https://sites.google.com/alumni.usp.br/michael/currículo).
#   b: participação do grupo no cargo/recurso (candidatos, receitas, eleitos);
#   p: participação do grupo na população de referência (eleitorado ou população).
# Varia de -1 (grupo ausente) a 1 (grupo detém tudo), com 0 na paridade (b = p).
dri <- function(b, p) {
  fifelse(is.na(b) | is.na(p), NA_real_,
          fifelse(b == p, 0, (b - p) / sqrt((b - p)^2 + b * (1 - b))))
}
