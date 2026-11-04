"""
Gera a versão pública (pseudonimizada) dos bancos do bd_representa.

Para cada banco (.csv ou .xlsx) das pastas listadas em FONTES:
  * CPF e título eleitoral  -> HMAC-SHA256 com chave secreta (16 hex), em colunas *_hash
  * e-mails, telefones, endereço e CEP -> removidos
  * data de nascimento      -> só o ano (ano_nascimento)
e grava <nome>.csv.gz (separador ';', UTF-8) em DESTINO, além de MANIFESTO.csv.

Uso:
  python publicar_bancos.py <raiz_do_projeto> <pasta_destino> <arquivo_chave> [n_processos]

A chave (arquivo_chave) NUNCA deve ir para o repositório. O mesmo documento
gera sempre o mesmo hash em todos os bancos, o que preserva os cruzamentos.
"""
import csv, datetime as dt, gzip, hashlib, hmac, os, re, sys
from multiprocessing import Pool
from pathlib import Path

csv.field_size_limit(2**31 - 1)

FONTES = [  # (pasta relativa à raiz, prefixo no nome publicado)
    ("3 Produtos finais/bancos", ""),
    ("1 Pesquisas individuais/4a Produtos", "etapa1_"),
    ("2 Agregação dos dados/1b Partidos/bancos", "etapa2_"),
    ("2 Agregação dos dados/2b Eleições/bancos", "etapa2_"),
]

CPF = re.compile(r"^(cpf|nr_cpf.*|cpf_candidato)$", re.I)
TITULO = re.compile(r"titulo_eleitoral", re.I)
REMOVER = re.compile(r"(e_?mail|fone|telefone|endereco|^nr_cep|^cep$|^nr_fax)", re.I)
NASC = re.compile(r"^(data_nascimento|dt_nascimento)$", re.I)

KEY = None


def _hash(valor: str, tipo: str) -> str:
    dig = re.sub(r"\D", "", valor or "")
    if not dig or set(dig) == {"0"}:
        return ""
    dig = dig.lstrip("0")  # ignora zeros à esquerda (11 ou 12 dígitos)
    return hmac.new(KEY, f"{tipo}:{dig}".encode(), hashlib.sha256).hexdigest()[:16]


def _ano(valor) -> str:
    if valor is None:
        return ""
    if isinstance(valor, (dt.date, dt.datetime)):
        return str(valor.year)
    s = str(valor).strip()
    if isinstance(valor, (int, float)) or re.fullmatch(r"\d{1,5}(\.0+)?", s):
        # número serial do Excel (dias desde 1899-12-30)
        try:
            return str((dt.date(1899, 12, 30) + dt.timedelta(days=int(float(s)))).year)
        except (ValueError, OverflowError):
            return ""
    m = re.match(r"(\d{4})-\d{1,2}-\d{1,2}", s)                 # aaaa-mm-dd
    if not m:
        m = re.match(r"\d{1,2}[/-]\d{1,2}[/-](\d{4})", s)       # dd/mm/aaaa
    if not m:
        m = re.fullmatch(r"\d{4}(\d{4})", s)                     # ddmmaaaa
        if m and not 1880 <= int(m.group(1)) <= 2030:
            m = re.fullmatch(r"(\d{4})\d{4}", s)                 # aaaammdd
    if m and 1880 <= int(m.group(1)) <= 2030:
        return m.group(1)
    return ""


def plano_colunas(cab):
    """Retorna (novo_cabecalho, lista de (indice, acao))."""
    novo, plano = [], []
    for i, c in enumerate(cab):
        if REMOVER.search(c):
            continue
        if CPF.match(c):
            novo.append(c + "_hash"); plano.append((i, "cpf"))
        elif TITULO.search(c):
            novo.append(c + "_hash"); plano.append((i, "titulo"))
        elif NASC.match(c):
            novo.append("ano_nascimento"); plano.append((i, "ano"))
        else:
            novo.append(c); plano.append((i, None))
    return novo, plano


def _fmt(v):
    if v is None:
        return ""
    if isinstance(v, float):
        return str(int(v)) if v.is_integer() else repr(v)
    if isinstance(v, dt.datetime):
        return v.date().isoformat() if v.time() == dt.time(0) else v.isoformat(sep=" ")
    if isinstance(v, dt.date):
        return v.isoformat()
    return str(v)


def aplicar(linha, plano):
    out = []
    for i, acao in plano:
        v = linha[i] if i < len(linha) else ""
        if acao is None:
            out.append(_fmt(v))
        elif acao == "ano":
            out.append(_ano(v))
        else:
            out.append(_hash(_fmt(v), acao))
    return out


def linhas_csv(caminho):
    with open(caminho, "rb") as f:
        primeira = f.readline(200000).decode("utf-8", "replace")
    sep = ";" if primeira.count(";") >= primeira.count(",") else ","
    f = open(caminho, newline="", encoding="utf-8-sig", errors="surrogateescape")
    return csv.reader(f, delimiter=sep), f


def linhas_xlsx(caminho):
    import openpyxl
    wb = openpyxl.load_workbook(caminho, read_only=True, data_only=True)
    ws = wb.worksheets[0]
    ws.reset_dimensions()
    return ws.iter_rows(values_only=True), wb


def processar(tarefa):
    origem, destino, chave = tarefa
    global KEY
    KEY = chave
    tmp = destino.with_suffix(".tmp")
    if origem.suffix.lower() == ".xlsx":
        it, h = linhas_xlsx(origem)
    else:
        it, h = linhas_csv(origem)
    n = 0
    sens = []
    try:
        cab = [_fmt(c).strip() for c in next(it)]
        novo, plano = plano_colunas(cab)
        sens = [c for c in cab if c not in novo]
        with gzip.open(tmp, "wt", encoding="utf-8", errors="surrogateescape", newline="", compresslevel=6) as g:
            w = csv.writer(g, delimiter=";", lineterminator="\n")
            w.writerow(novo)
            for linha in it:
                if linha is None or all(v in (None, "") for v in linha):
                    continue
                w.writerow(aplicar(linha, plano))
                n += 1
    finally:
        h.close()
    os.replace(tmp, destino)
    print(f"OK {destino.name}: {n} linhas, tratadas={sens}", flush=True)
    return destino.name, n


def main():
    raiz, dest, arq_chave = Path(sys.argv[1]), Path(sys.argv[2]), Path(sys.argv[3])
    nproc = int(sys.argv[4]) if len(sys.argv) > 4 else 4
    dest.mkdir(parents=True, exist_ok=True)
    chave = arq_chave.read_text().strip().encode()
    tarefas, origem_de = [], {}
    for pasta, prefixo in FONTES:
        for p in sorted((raiz / pasta).rglob("*")):
            if p.suffix.lower() not in (".csv", ".xlsx") or not p.is_file() or "~$" in p.name:
                continue
            nome = prefixo + p.stem + ".csv.gz"
            if nome in origem_de:
                raise SystemExit(f"Nome duplicado: {nome}")
            origem_de[nome] = str(p.relative_to(raiz)).replace("\\", "/")
            d = dest / nome
            if d.exists():  # retomada: pula o que já foi gerado
                continue
            tarefas.append((p, d, chave))
    tarefas.sort(key=lambda t: -t[0].stat().st_size)  # maiores primeiro
    print(f"{len(tarefas)} arquivos a processar ({len(origem_de)} no total)", flush=True)
    with Pool(nproc) as pool:
        for _ in pool.imap_unordered(processar, tarefas):
            pass
    # manifesto (recalcula linhas/hash de tudo que existe)
    with open(dest / "MANIFESTO.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["arquivo", "origem", "bytes", "sha256"])
        for nome, origem in sorted(origem_de.items()):
            d = dest / nome
            if d.exists():
                h = hashlib.sha256()
                with open(d, "rb") as fh:
                    for bloco in iter(lambda: fh.read(1 << 24), b""):
                        h.update(bloco)
                w.writerow([nome, origem, d.stat().st_size, h.hexdigest()])
    print("MANIFESTO gravado.", flush=True)


if __name__ == "__main__":
    main()
