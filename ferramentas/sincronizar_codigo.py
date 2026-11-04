"""
Copia o código, a documentação e os dicionários da pasta de trabalho (Drive)
para o repositório, removendo informações privadas.

  * set_billing_id("<projeto>")  ->  set_billing_id("seu-projeto") ## substituir por seu projeto
  * caminhos locais (G:/Drives compartilhados/...) -> <raiz do projeto>

Uso: python sincronizar_codigo.py <pasta_do_projeto_no_drive> <pasta_do_repositorio>
"""
import re, shutil, sys
from pathlib import Path

src, dst = Path(sys.argv[1]), Path(sys.argv[2])
EXT_TEXTO = {".R", ".Rmd", ".md", ".log", ".txt", ".Rproj"}
IGNORAR = ("_arquivo", ".Rproj.user", "dados_brutos", "bancos")

BILLING = re.compile(r'set_billing_id\([ \t]*"[^"\r\n]*"[ \t]*\)[ \t]*(##[^\r\n]*)?')
CAMINHO = re.compile(re.escape(str(src).replace("\\", "/")), re.I)


def limpar(texto: str) -> str:
    texto = BILLING.sub('set_billing_id("seu-projeto") ## substituir por seu projeto', texto)
    texto = CAMINHO.sub("<raiz do projeto>", texto)
    return texto


n = 0
for p in src.rglob("*"):
    rel = p.relative_to(src)
    if not p.is_file() or any(part in IGNORAR for part in rel.parts):
        continue
    destino = dst / rel
    if p.suffix in EXT_TEXTO:
        destino.parent.mkdir(parents=True, exist_ok=True)
        texto = p.read_text(encoding="utf-8", errors="surrogateescape")
        destino.write_text(limpar(texto), encoding="utf-8", errors="surrogateescape", newline="")
        n += 1
    elif rel.parts[:2] == ("3 Produtos finais", "dicionarios") and p.suffix == ".xlsx":
        destino.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(p, destino)
        n += 1
print(f"{n} arquivos sincronizados")

# conferência: nada do projeto privado pode sobrar
sobras = [str(f) for f in dst.rglob("*") if f.is_file() and f.suffix in EXT_TEXTO
          and ".git" not in f.parts
          and re.search(r'set_billing_id\("(?!seu-projeto")', f.read_text(encoding="utf-8", errors="ignore"))]
if sobras:
    raise SystemExit("ATENÇÃO, billing id ainda presente em: " + "; ".join(sobras))
