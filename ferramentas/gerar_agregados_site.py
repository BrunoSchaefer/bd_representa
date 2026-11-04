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

# ---------------------------------------------------------------- arquivos
arqs = []
if (src / "MANIFESTO.csv").exists():
    with open(src / "MANIFESTO.csv", encoding="utf-8") as f:
        for r in csv.DictReader(f, delimiter=";"):
            arqs.append({"arquivo": r["arquivo"], "origem": r["origem"], "bytes": int(r["bytes"])})
json.dump(arqs, open(out / "arquivos.json", "w", encoding="utf-8"), ensure_ascii=False, indent=0)
print("ok")
