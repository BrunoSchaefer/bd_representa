"""
Gera os agregados (JSON pequenos) que alimentam a página site/.

Uso: python gerar_agregados_site.py <pasta_com_csv.gz> <pasta_site/dados>

Saídas:
  direcoes.json      nivel x ano x partido x UF: membros, mulheres, decisão, presidência
  candidaturas.json  cargo x ano x partido x UF: candidaturas, mulheres, negros, eleitos
  arquivos.json      lista de arquivos da release (do MANIFESTO.csv)
"""
import csv, json, sys
from pathlib import Path
import pandas as pd

src, out = Path(sys.argv[1]), Path(sys.argv[2])
out.mkdir(parents=True, exist_ok=True)


def ler(nome, cols):
    if not (src / f"{nome}.csv.gz").exists():
        print("AVISO: faltando", nome); return None
    return pd.read_csv(src / f"{nome}.csv.gz", sep=";", usecols=lambda c: c in cols,
                       dtype=str, low_memory=False)


def num(s):
    return pd.to_numeric(s, errors="coerce").fillna(0)


# ---------------------------------------------------------------- direções
NUMS = ["total_membros", "total_mulheres", "total_decisao", "mulher_decisao",
        "total_presidente", "mulher_presidente"]
partes = []
for nivel in ["nacional", "estadual", "municipal"]:
    d = ler(f"bd_diretorios_{nivel}", ["ANO", "SG_PARTIDO_PAD", "SG_UF"] + NUMS)
    if d is None:
        continue
    if "SG_UF" not in d or nivel == "nacional":
        d["SG_UF"] = "BR"
    for c in NUMS:
        d[c] = num(d[c])
    d["orgaos"] = 1
    g = d.groupby(["ANO", "SG_PARTIDO_PAD", "SG_UF"], as_index=False)[NUMS + ["orgaos"]].sum()
    g[NUMS] = g[NUMS].round().astype(int)
    g.insert(0, "nivel", nivel)
    partes.append(g)
    print(nivel, len(d), "->", len(g))
dir_ = pd.concat(partes)
cols_dir = ["nivel", "ano", "partido", "uf", "membros", "mulheres", "decisao",
            "mulheres_decisao", "presidentes", "mulheres_presidentes", "orgaos"]
dir_.columns = cols_dir
dir_["ano"] = dir_["ano"].astype(int)
json.dump({"colunas": cols_dir, "linhas": dir_.astype(object).values.tolist()},
          open(out / "direcoes.json", "w", encoding="utf-8"), ensure_ascii=False,
          separators=(",", ":"), default=int)

# ------------------------------------------------------------ candidaturas
CARGOS = {"vereanca": "Vereança", "prefeitura": "Prefeitura", "assembleia": "Deputado(a) estadual/distrital",
          "camara": "Deputado(a) federal", "senado": "Senado", "governadoria": "Governo estadual"}
partes = []
for arq, rotulo in CARGOS.items():
    d = ler(f"bd_candidatos_{arq}", ["ano", "sigla_uf", "SG_PARTIDO_PAD", "mulher", "negro", "eleito_final"])
    if d is None:
        continue
    eleito = d["eleito_final"].fillna("").str.lower().str.startswith("eleit").astype(int)
    m = num(d["mulher"]); n = num(d["negro"]); r = d["negro"].notna().astype(int)
    t = pd.DataFrame({"cargo": rotulo, "ano": d["ano"].astype(int), "partido": d["SG_PARTIDO_PAD"].fillna("?"),
                      "uf": d["sigla_uf"].fillna("?"), "candidaturas": 1, "mulheres": m,
                      "raca_informada": r, "negros": n, "eleitos": eleito, "eleitas": eleito * m,
                      "eleitos_raca_informada": eleito * r, "eleitos_negros": eleito * n})
    g = t.groupby(["cargo", "ano", "partido", "uf"], as_index=False).sum()
    nums = g.columns[4:]
    g[nums] = g[nums].round().astype(int)
    partes.append(g)
    print(arq, len(d), "->", len(g))
cand = pd.concat(partes)
json.dump({"colunas": list(cand.columns), "linhas": cand.astype(object).values.tolist()},
          open(out / "candidaturas.json", "w", encoding="utf-8"), ensure_ascii=False,
          separators=(",", ":"), default=int)

# ------------------------------------------------------------ mapa (listas)
# Um arquivo por cargo-ano. Por lista partidária no distrito: totais (T) e
# participações (b) de mulheres e de pessoas negras; por distrito: o
# denominador p do DRI. A página calcula DRI(b, p) para um partido ou para o
# conjunto das listas (b ponderado por T).
MEDIDAS = [  # (chave, total, participação de mulheres, participação de negros)
    ("candidaturas", "n_candidatos", "prop_candidatos_mulheres", "prop_candidatos_negros"),
    ("eleitos", "n_eleitos", "prop_eleitos_mulheres", "prop_eleitos_negros"),
    ("votos", "votos_nominais", "prop_votos_mulheres", "prop_votos_negros"),
    ("receitas", "receitas", "prop_receitas_mulheres", "prop_receitas_negros"),
    ("receitas_publicas", "recursos_publicos", "prop_receitas_publicas_mulheres", "prop_receitas_publicas_negros"),
]
MAPAS = [("vereanca", "bd_partidos_vereanca", "id_municipio"),
         ("assembleia", "bd_partidos_assembleias", "sigla_uf"),
         ("camara", "bd_partidos_camara", "sigla_uf")]
(out / "mapa").mkdir(exist_ok=True)
indice = {}
for chave, banco, col_dist in MAPAS:
    cols = ["ano", col_dist, "SG_PARTIDO_PAD", "denominador_mulheres", "denominador_negros"] + \
           [c for m in MEDIDAS for c in m[1:]]
    d = ler(banco, cols)
    if d is None:
        continue
    for c in cols[3:]:
        d[c] = pd.to_numeric(d[c], errors="coerce")
    sem = d[col_dist].isna().sum()
    if sem:
        print(f"  {banco}: {sem} linhas sem distrito descartadas")
    d = d.dropna(subset=[col_dist]).copy()
    d["ano"] = d["ano"].astype(int)
    if col_dist == "id_municipio":
        d[col_dist] = d[col_dist].str.replace(r"\.0$", "", regex=True)
    indice[chave] = sorted(d["ano"].unique().tolist())
    for ano, g in d.groupby("ano"):
        partidos = sorted(g["SG_PARTIDO_PAD"].fillna("?").unique().tolist())
        pidx = {p: i for i, p in enumerate(partidos)}
        den = g.groupby(col_dist)[["denominador_mulheres", "denominador_negros"]].first()
        distritos = {k: [None if pd.isna(v) else round(float(v), 4) for v in row]
                     for k, row in zip(den.index, den.values)}
        linhas = []
        for r in g.itertuples(index=False):
            r = r._asdict()
            l = [r[col_dist], pidx[r["SG_PARTIDO_PAD"] if isinstance(r["SG_PARTIDO_PAD"], str) else "?"]]
            for _, tot, pm, pn in MEDIDAS:
                t = r[tot]
                l.append(0 if pd.isna(t) else int(round(float(t))))
                l.append(None if pd.isna(r[pm]) else round(float(r[pm]), 4))
                l.append(None if pd.isna(r[pn]) else round(float(r[pn]), 4))
            linhas.append(l)
        doc = {"cargo": chave, "ano": int(ano), "medidas": [m[0] for m in MEDIDAS],
               "partidos": partidos, "distritos": distritos, "linhas": linhas}
        json.dump(doc, open(out / "mapa" / f"{chave}_{ano}.json", "w", encoding="utf-8"),
                  ensure_ascii=False, separators=(",", ":"), default=int, allow_nan=False)
    print("mapa", chave, indice[chave])
json.dump(indice, open(out / "mapa" / "indice.json", "w"), separators=(",", ":"))

# ---------------------------------------------------------------- arquivos
arqs = []
if (src / "MANIFESTO.csv").exists():
    with open(src / "MANIFESTO.csv", encoding="utf-8") as f:
        for r in csv.DictReader(f, delimiter=";"):
            arqs.append({"arquivo": r["arquivo"], "origem": r["origem"], "bytes": int(r["bytes"])})
json.dump(arqs, open(out / "arquivos.json", "w", encoding="utf-8"), ensure_ascii=False, indent=0)
print("ok")
