/* bd_representa — página de visualização (sem dependências) */
(() => {
  const REPO = "https://github.com/BrunoSchaefer/bd_representa";
  const NIVEIS = [
    { k: "nacional", rotulo: "Nacional", cor: "--s1" },
    { k: "estadual", rotulo: "Estadual", cor: "--s2" },
    { k: "municipal", rotulo: "Municipal", cor: "--s3" },
  ];
  const METRICAS = {
    membros: ["mulheres", "membros", "dos membros"],
    decisao: ["mulheres_decisao", "decisao", "dos cargos de presidência e tesouraria"],
    presidentes: ["mulheres_presidentes", "presidentes", "das presidências"],
  };
  const UFS_NOMES = {AC:"Acre",AL:"Alagoas",AM:"Amazonas",AP:"Amapá",BA:"Bahia",CE:"Ceará",DF:"Distrito Federal",ES:"Espírito Santo",GO:"Goiás",MA:"Maranhão",MG:"Minas Gerais",MS:"Mato Grosso do Sul",MT:"Mato Grosso",PA:"Pará",PB:"Paraíba",PE:"Pernambuco",PI:"Piauí",PR:"Paraná",RJ:"Rio de Janeiro",RN:"Rio Grande do Norte",RO:"Rondônia",RR:"Roraima",RS:"Rio Grande do Sul",SC:"Santa Catarina",SE:"Sergipe",SP:"São Paulo",TO:"Tocantins"};

  const $ = (s) => document.querySelector(s);
  const css = (v) => getComputedStyle(document.documentElement).getPropertyValue(v).trim();
  const pct = (x, d = 1) => (x == null || !isFinite(x)) ? "–" : (x * 100).toLocaleString("pt-BR", { minimumFractionDigits: d, maximumFractionDigits: d }) + "%";
  const int = (x) => Math.round(x).toLocaleString("pt-BR");
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));

  const estado = { partido: "", uf: "", dMetrica: "membros", cCargo: "Vereança", cGrupo: "mulheres", pNivel: "municipal", pAno: null, aGrupo: "finais",
                   mCargo: "vereanca", mAno: null, mMedida: "candidaturas", mGrupo: "mulheres",
                   pDim: "direcoes", pCargo: "vereanca", pMedida: "receitas", pGrupo: "mulheres" };
  let DIR = [], CAND = [], ARQ = [];

  const objetos = (j) => j.linhas.map((l) => Object.fromEntries(j.colunas.map((c, i) => [c, l[i]])));

  Promise.all(["dados/direcoes.json", "dados/candidaturas.json", "dados/arquivos.json"].map((u) => fetch(u).then((r) => r.json())))
    .then(([d, c, a]) => { DIR = objetos(d); CAND = objetos(c); ARQ = a; iniciar(); })
    .catch((e) => { document.querySelector("main").insertAdjacentHTML("afterbegin", `<p class="vazio">Não foi possível carregar os dados (${esc(e.message)}).</p>`); });

  /* ------------------------------------------------------------ controles */
  function opcoes(sel, valores, todos) {
    sel.innerHTML = (todos ? `<option value="">${todos}</option>` : "") + valores.map(([v, t]) => `<option value="${esc(v)}">${esc(t)}</option>`).join("");
  }
  function segmentado(id, chave) {
    const g = $(id);
    g.addEventListener("click", (e) => {
      const b = e.target.closest("button"); if (!b) return;
      g.querySelectorAll("button").forEach((x) => x.setAttribute("aria-pressed", x === b));
      estado[chave] = b.dataset.v; desenhar();
    });
  }

  function iniciar() {
    const partidos = [...new Set([...DIR, ...CAND].map((r) => r.partido))].filter((p) => p && p !== "?").sort((a, b) => a.localeCompare(b, "pt-BR"));
    opcoes($("#f-partido"), partidos.map((p) => [p, p]), "Todos os partidos");
    opcoes($("#f-uf"), Object.keys(UFS_NOMES).map((u) => [u, `${u} · ${UFS_NOMES[u]}`]), "Brasil");
    const cargos = ["Vereança", "Prefeitura", "Deputado(a) estadual/distrital", "Deputado(a) federal", "Senado", "Governo estadual"].filter((c) => CAND.some((r) => r.cargo === c));
    opcoes($("#c-cargo"), cargos.map((c) => [c, c]));
    if (!cargos.includes(estado.cCargo)) estado.cCargo = cargos[0];
    const anos = [...new Set(DIR.map((r) => r.ano))].sort();
    estado.pAno = anos[anos.length - 1];
    opcoes($("#p-ano"), anos.slice().reverse().map((a) => [a, a]));

    $("#f-partido").onchange = (e) => { estado.partido = e.target.value; desenhar(); };
    $("#f-uf").onchange = (e) => { estado.uf = e.target.value; desenhar(); };
    $("#c-cargo").onchange = (e) => { estado.cCargo = e.target.value; desenhar(); };
    $("#p-nivel").onchange = (e) => { estado.pNivel = e.target.value; desenhar(); };
    $("#p-ano").onchange = (e) => { estado.pAno = +e.target.value; desenhar(); };
    segmentado("#d-metrica", "dMetrica");
    segmentado("#c-grupo", "cGrupo");
    segmentado("#a-grupo", "aGrupo");
    segmentado("#m-grupo", "mGrupo");
    segmentado("#p-dim", "pDim");
    segmentado("#p-grupo", "pGrupo");
    $("#p-cargo").onchange = (e) => { estado.pCargo = e.target.value; desenhar(); };
    $("#p-medida").onchange = (e) => { estado.pMedida = e.target.value; desenhar(); };
    $("#m-cargo").onchange = (e) => { estado.mCargo = e.target.value; desenhar(); };
    $("#m-ano").onchange = (e) => { estado.mAno = +e.target.value; desenhar(); };
    $("#m-medida").onchange = (e) => { estado.mMedida = e.target.value; desenhar(); };
    matchMedia("(prefers-color-scheme: dark)").addEventListener("change", desenhar);
    let t; addEventListener("resize", () => { clearTimeout(t); t = setTimeout(desenhar, 150); });
    desenhar();
  }

  const filtra = (rows, { partido = true, uf = true } = {}) => rows.filter((r) =>
    (!partido || !estado.partido || r.partido === estado.partido) && (!uf || !estado.uf || r.uf === estado.uf));

  function soma(rows, chaveGrupo, campos) {
    const m = new Map();
    for (const r of rows) {
      const k = chaveGrupo(r);
      let o = m.get(k); if (!o) { o = {}; campos.forEach((c) => (o[c] = 0)); m.set(k, o); }
      campos.forEach((c) => (o[c] += r[c] || 0));
    }
    return m;
  }

  function desenhar() {
    const rec = [estado.partido || "todos os partidos", estado.uf ? UFS_NOMES[estado.uf] : "Brasil"].join(" · ");
    $("#f-resumo").textContent = `Mostrando: ${rec}`;
    destaques(); direcoes(); candidaturas(); mapa(); partidos(); arquivos();
  }

  /* ------------------------------------------------------------ destaques */
  function destaques() {
    const d = filtra(DIR).filter((r) => r.nivel === "municipal");
    const anosD = DIR.filter((r) => r.nivel === "municipal").map((r) => r.ano);
    const ultimo = anosD.length ? Math.max(...anosD) : null;
    const dm = soma(d.filter((r) => r.ano === ultimo), () => 1, ["mulheres", "membros", "mulheres_presidentes", "presidentes"]).get(1);
    const c = filtra(CAND).filter((r) => r.cargo === "Vereança");
    const anosC = CAND.filter((r) => r.cargo === "Vereança").map((r) => r.ano);
    const ultC = anosC.length ? Math.max(...anosC) : null;
    const cm = soma(c.filter((r) => r.ano === ultC), () => 1, ["mulheres", "candidaturas", "eleitas", "eleitos"]).get(1);
    const q = (a, b) => (b > 0 ? pct(a / b) : null);
    const tiles = [
      ["Mulheres nas direções municipais", dm && q(dm.mulheres, dm.membros), dm ? `${ultimo} · ${int(dm.membros)} membros` : "sem dados"],
      ["Presidências municipais ocupadas por mulheres", dm && q(dm.mulheres_presidentes, dm.presidentes), dm ? `${ultimo} · ${int(dm.presidentes)} presidências` : "sem dados"],
      ["Mulheres entre candidaturas à vereança", cm && q(cm.mulheres, cm.candidaturas), cm ? `${ultC} · ${int(cm.candidaturas)} candidaturas` : "sem dados"],
      ["Mulheres entre eleitos à vereança", cm && q(cm.eleitas, cm.eleitos), cm ? `${ultC} · ${int(cm.eleitos)} eleitos(as)` : "sem dados"],
    ];
    $("#destaques").innerHTML = tiles.map(([r, v, c]) =>
      `<div class="tile"><div class="rotulo">${r}</div><div class="valor">${v || "–"}</div><div class="contexto">${esc(c)}</div></div>`).join("");
  }

  /* ------------------------------------------------------------ gráfico de linhas */
  function linhas(el, { series, xs, tabela, rotuloY = "%" }) {
    const ativos = series.filter((s) => s.pontos.some((p) => p.y != null));
    if (!ativos.length) { el.innerHTML = `<p class="vazio">Sem dados para este recorte.</p>`; tabela.innerHTML = ""; return; }
    const W = Math.max(320, el.clientWidth), H = W < 560 ? 260 : 320;
    const m = { t: 12, r: W < 560 ? 12 : 128, b: 28, l: 44 };
    const iw = W - m.l - m.r, ih = H - m.t - m.b;
    const maxY = Math.min(1, Math.max(0.1, ...ativos.flatMap((s) => s.pontos.map((p) => p.y || 0))) * 1.12);
    const passo = maxY > 0.5 ? 0.2 : maxY > 0.25 ? 0.1 : 0.05;
    const topo = Math.ceil(maxY / passo) * passo;
    const x = (v) => m.l + (xs.length === 1 ? iw / 2 : (xs.indexOf(v) / (xs.length - 1)) * iw);
    const y = (v) => m.t + ih - (v / topo) * ih;
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Gráfico de linhas">`;
    s += `<g class="grade">`;
    for (let v = 0; v <= topo + 1e-9; v += passo) s += `<line x1="${m.l}" x2="${m.l + iw}" y1="${y(v)}" y2="${y(v)}"/><text x="${m.l - 8}" y="${y(v) + 4}" text-anchor="end">${Math.round(v * 100)}${rotuloY}</text>`;
    s += `</g><line class="base" x1="${m.l}" x2="${m.l + iw}" y1="${y(0)}" y2="${y(0)}"/>`;
    const cada = W < 560 && xs.length > 6 ? 2 : 1;
    xs.forEach((v, i) => { if (i % cada === 0 || i === xs.length - 1) s += `<text x="${x(v)}" y="${H - 8}" text-anchor="middle">${v}</text>`; });
    for (const se of ativos) {
      const cor = css(se.cor);
      const segs = []; let atual = [];
      se.pontos.forEach((p) => { if (p.y == null) { if (atual.length) segs.push(atual); atual = []; } else atual.push(p); });
      if (atual.length) segs.push(atual);
      segs.forEach((sg) => { s += `<path d="${sg.map((p, i) => (i ? "L" : "M") + x(p.x) + " " + y(p.y)).join("")}" fill="none" stroke="${cor}" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>`; });
      se.pontos.filter((p) => p.y != null).forEach((p) => { s += `<circle cx="${x(p.x)}" cy="${y(p.y)}" r="4" fill="${cor}" stroke="${css("--surface")}" stroke-width="2"/>`; });
    }
    // rótulos diretos no fim das linhas, afastados para não colidir
    if (W >= 560) {
      const rot = ativos.map((se) => { const u = [...se.pontos].reverse().find((p) => p.y != null); return { se, u, yy: y(u.y) }; }).sort((a, b) => a.yy - b.yy);
      for (let i = 1; i < rot.length; i++) if (rot[i].yy - rot[i - 1].yy < 15) rot[i].yy = rot[i - 1].yy + 15;
      rot.forEach(({ se, u, yy }) => { s += `<text x="${x(u.x) + 10}" y="${yy + 4}" style="fill:var(--text)">${esc(se.rotulo)} ${pct(u.y, 0)}</text>`; });
    }
    s += `<line id="mira" x1="0" x2="0" y1="${m.t}" y2="${m.t + ih}" stroke="${css("--muted")}" stroke-dasharray="3 3" visibility="hidden"/>`;
    s += `<rect x="${m.l - 10}" y="0" width="${iw + 20}" height="${H}" fill="transparent" class="alvo"/></svg>`;
    s += `<div class="legenda">${ativos.map((se) => `<span><i style="background:${css(se.cor)}"></i>${esc(se.rotulo)}</span>`).join("")}</div>`;
    el.innerHTML = s;

    const svg = el.querySelector("svg"), mira = svg.querySelector("#mira"), tip = $("#tooltip");
    const mover = (ev) => {
      const r = svg.getBoundingClientRect(), px = ((ev.clientX - r.left) / r.width) * W;
      let melhor = xs[0]; xs.forEach((v) => { if (Math.abs(x(v) - px) < Math.abs(x(melhor) - px)) melhor = v; });
      mira.setAttribute("x1", x(melhor)); mira.setAttribute("x2", x(melhor)); mira.setAttribute("visibility", "visible");
      tip.innerHTML = `<b>${melhor}</b>` + ativos.map((se) => {
        const p = se.pontos.find((q) => q.x === melhor);
        return `<div class="linha"><span><i style="background:${css(se.cor)}"></i>${esc(se.rotulo)}</span><span>${p && p.y != null ? pct(p.y) : "–"}</span></div>` +
          (p && p.det ? `<div class="linha"><span class="dica">${esc(p.det)}</span><span></span></div>` : "");
      }).join("");
      tip.hidden = false;
      const tx = Math.min(ev.clientX + 14, innerWidth - tip.offsetWidth - 8);
      tip.style.left = tx + "px"; tip.style.top = Math.max(8, ev.clientY - tip.offsetHeight - 12) + "px";
    };
    svg.querySelector(".alvo").addEventListener("pointermove", mover);
    svg.querySelector(".alvo").addEventListener("pointerleave", () => { tip.hidden = true; mira.setAttribute("visibility", "hidden"); });

    tabela.innerHTML = `<table><thead><tr><th>Ano</th>${ativos.map((se) => `<th class="n">${esc(se.rotulo)}</th>`).join("")}</tr></thead><tbody>` +
      xs.map((v) => `<tr><td>${v}</td>${ativos.map((se) => { const p = se.pontos.find((q) => q.x === v); return `<td class="n">${p && p.y != null ? pct(p.y) : "–"}${p && p.det ? ` <span class="dica">(${esc(p.det)})</span>` : ""}</td>`; }).join("")}</tr>`).join("") + `</tbody></table>`;
  }

  /* ------------------------------------------------------------ 1. direções */
  function direcoes() {
    const [num, den, txt] = METRICAS[estado.dMetrica];
    const rows = filtra(DIR);
    const xs = [...new Set(DIR.map((r) => r.ano))].sort();
    const series = NIVEIS.filter((n) => !(estado.uf && n.k === "nacional")).map((n) => {
      const m = soma(rows.filter((r) => r.nivel === n.k), (r) => r.ano, [num, den]);
      return { rotulo: n.rotulo, cor: n.cor, pontos: xs.map((a) => { const o = m.get(a); return { x: a, y: o && o[den] >= 5 ? o[num] / o[den] : null, det: o && o[den] ? `${int(o[num])} de ${int(o[den])}` : "" }; }) };
    });
    $("#direcoes .sub").textContent = `Percentual de mulheres ${txt}, por nível da direção e ano${estado.uf ? " (o nível nacional não tem recorte por UF)" : ""}.`;
    linhas($("#g-direcoes"), { series, xs, tabela: $("#t-direcoes") });
  }

  /* ------------------------------------------------------------ 2. candidaturas */
  function candidaturas() {
    const g = estado.cGrupo;
    const rows = filtra(CAND).filter((r) => r.cargo === estado.cCargo);
    const xs = [...new Set(CAND.filter((r) => r.cargo === estado.cCargo).map((r) => r.ano))].sort();
    const campos = ["candidaturas", "mulheres", "raca_informada", "negros", "eleitos", "eleitas", "eleitos_raca_informada", "eleitos_negros"];
    const m = soma(rows, (r) => r.ano, campos);
    const def = g === "mulheres"
      ? [["Candidaturas", "mulheres", "candidaturas", "--s1"], ["Eleitos(as)", "eleitas", "eleitos", "--s2"]]
      : [["Candidaturas", "negros", "raca_informada", "--s1"], ["Eleitos(as)", "eleitos_negros", "eleitos_raca_informada", "--s2"]];
    const series = def.map(([rot, n, d, cor]) => ({ rotulo: rot, cor, pontos: xs.map((a) => { const o = m.get(a); return { x: a, y: o && o[d] >= 1 ? o[n] / o[d] : null, det: o && o[d] ? `${int(o[n])} de ${int(o[d])}` : "" }; }) }));
    $("#c-nota").textContent = g === "negros"
      ? "Pessoas negras = cor/raça preta ou parda. O TSE só informa cor/raça a partir de 2014, e o denominador inclui apenas candidaturas com cor/raça informada."
      : "Candidaturas deferidas em eleições ordinárias, no 1º turno. Eleitos inclui eleitos por quociente partidário e por média.";
    linhas($("#g-candidaturas"), { series, xs, tabela: $("#t-candidaturas") });
  }

  /* ------------------------------------------------------------ mapa da equidade */
  const FAIXAS = [ // [limite superior, token de cor, rótulo]
    [-0.6, "--div-n4", "−1 a −0,6"], [-0.35, "--div-n3", "−0,6 a −0,35"], [-0.15, "--div-n2", "−0,35 a −0,15"],
    [-0.05, "--div-n1", "−0,15 a −0,05"], [0.05, "--div-0", "−0,05 a 0,05 (paridade)"], [0.15, "--div-p1", "0,05 a 0,15"],
    [0.35, "--div-p2", "0,15 a 0,35"], [0.6, "--div-p3", "0,35 a 0,6"], [1.0001, "--div-p4", "0,6 a 1"],
  ];
  const corDri = (v) => v == null ? css("--sem-dado") : css(FAIXAS.find((f) => v < f[0])[1]);
  const dri = (b, p) => (b == null || p == null) ? null : b === p ? 0 : (b - p) / Math.sqrt((b - p) ** 2 + b * (1 - b));
  const fmtDri = (v) => v == null ? "–" : v.toLocaleString("pt-BR", { minimumFractionDigits: 2, maximumFractionDigits: 2, signDisplay: "exceptZero" });
  const MED_ROT = { candidaturas: "candidaturas", eleitos: "eleitos(as)", votos: "votos nominais", receitas: "receitas", receitas_publicas: "recursos públicos" };
  const cache = {};
  const json = (u) => (cache[u] ||= fetch(u).then((r) => { if (!r.ok) throw new Error(r.status + " " + u); return r.json(); }));
  let IND_MAPA = null, tokenMapa = 0;

  async function mapa() {
    const meu = ++tokenMapa, el = $("#g-mapa");
    try {
      IND_MAPA ||= await json("dados/mapa/indice.json");
      const anos = IND_MAPA[estado.mCargo] || [];
      if (!anos.includes(estado.mAno)) estado.mAno = anos[anos.length - 1];
      opcoes($("#m-ano"), anos.slice().reverse().map((a) => [a, a])); $("#m-ano").value = estado.mAno;
      const municipal = estado.mCargo === "vereanca";
      const [geoUf, geoMun, nomes, doc] = await Promise.all([
        json("dados/geo/ufs.json"), municipal ? json("dados/geo/municipios.json") : null,
        municipal ? json("dados/geo/nomes.json") : null, json(`dados/mapa/${estado.mCargo}_${estado.mAno}.json`)]);
      if (meu !== tokenMapa) return;

      // b ponderado por T sobre as listas (todas ou do partido escolhido)
      const k = doc.medidas.indexOf(estado.mMedida), g = estado.mGrupo === "mulheres" ? 0 : 1;
      const pi = estado.partido ? doc.partidos.indexOf(estado.partido) : -1;
      const acc = new Map();
      for (const l of doc.linhas) {
        if (estado.partido && l[1] !== pi) continue;
        const T = l[2 + 3 * k], b = l[3 + 3 * k + g];
        let o = acc.get(l[0]); if (!o) acc.set(l[0], (o = { num: 0, den: 0, listas: 0, total: 0 }));
        o.listas++; o.total += T;
        if (b != null && T > 0) { o.num += b * T; o.den += T; }
      }
      const val = new Map();
      for (const [d, o] of acc) {
        const p = doc.distritos[d] ? doc.distritos[d][g] : null, b = o.den > 0 ? o.num / o.den : null;
        val.set(d, { ...o, b, p, v: dri(b, p) });
      }
      const ufDe = (d) => municipal ? (nomes[d] || "").slice(-3, -1) : d;
      const visiveis = [...val.entries()].filter(([d]) => !estado.uf || ufDe(d) === estado.uf);

      // geometria
      const [x0, y0, x1, y1] = estado.uf ? geoUf.caixas_uf[estado.uf] : geoUf.caixa, pad = (x1 - x0) * 0.03;
      const vb = `${x0 - pad} ${y0 - pad} ${x1 - x0 + 2 * pad} ${y1 - y0 + 2 * pad}`;
      const nomeDe = (d) => municipal ? (nomes[d] || d) : `${UFS_NOMES[d]} (${d})`;
      let s = `<svg viewBox="${vb}" role="img" aria-label="Mapa do índice de equidade por ${municipal ? "município" : "UF"}">`;
      if (municipal) {
        for (const [cod, path] of Object.entries(geoMun)) {
          if (estado.uf && ufDe(cod) !== estado.uf) continue;
          const o = val.get(cod);
          s += `<path class="d" data-d="${cod}" d="${path}" fill="${corDri(o && o.v)}"/>`;
        }
        for (const [uf, path] of Object.entries(geoUf.ufs)) if (!estado.uf || uf === estado.uf) s += `<path class="uf" d="${path}"/>`;
      } else {
        for (const [uf, path] of Object.entries(geoUf.ufs)) {
          if (estado.uf && uf !== estado.uf) continue;
          const o = val.get(uf);
          s += `<path class="d" data-d="${uf}" d="${path}" fill="${corDri(o && o.v)}"/>`;
        }
      }
      el.innerHTML = s + `</svg>`;

      // tooltip
      const tip = $("#tooltip"), svg = el.querySelector("svg");
      svg.addEventListener("pointermove", (ev) => {
        const t = ev.target.closest("path.d"); if (!t) { tip.hidden = true; return; }
        const d = t.dataset.d, o = val.get(d);
        tip.innerHTML = `<b>${esc(nomeDe(d))}</b>` + (o ? `
          <div class="linha"><span>DRI</span><span><strong>${fmtDri(o.v)}</strong></span></div>
          <div class="linha"><span>${estado.mGrupo === "mulheres" ? "Mulheres" : "Pessoas negras"} em ${MED_ROT[estado.mMedida]}</span><span>${pct(o.b)}</span></div>
          <div class="linha"><span>No ${estado.mGrupo === "mulheres" ? "eleitorado" : "população"} (p)</span><span>${pct(o.p)}</span></div>
          <div class="linha"><span>Listas</span><span>${int(o.listas)}</span></div>` : `<div class="linha"><span>Sem lista${estado.partido ? " do " + esc(estado.partido) : ""}</span><span></span></div>`);
        tip.hidden = false;
        tip.style.left = Math.min(ev.clientX + 14, innerWidth - tip.offsetWidth - 8) + "px";
        tip.style.top = Math.max(8, ev.clientY - tip.offsetHeight - 12) + "px";
      });
      svg.addEventListener("pointerleave", () => (tip.hidden = true));

      // resumo, legenda e tabela
      const vs = visiveis.map(([, o]) => o.v).filter((v) => v != null).sort((a, b) => a - b);
      const med = vs.length ? (vs.length % 2 ? vs[(vs.length - 1) / 2] : (vs[vs.length / 2 - 1] + vs[vs.length / 2]) / 2) : null;
      const abaixo = vs.length ? vs.filter((v) => v < -0.05).length / vs.length : null;
      const unid = municipal ? "municípios" : "UFs";
      $("#m-resumo").innerHTML = `<div class="rotulo">DRI mediano (${unid})</div><div class="valor">${fmtDri(med)}</div>
        <div class="contexto">${vs.length ? `${pct(abaixo, 0)} ${municipal ? "dos" : "das"} ${int(vs.length)} ${unid} com dados abaixo da paridade` : "sem dados para este recorte"}</div>`;
      $("#m-legenda").innerHTML = `<div class="titulo">Índice de equidade (DRI)</div>` +
        FAIXAS.slice().reverse().map(([, c, r]) => `<div class="faixa"><i style="background:${css(c)}"></i>${r}</div>`).join("") +
        `<div class="faixa"><i style="background:${css("--sem-dado")};box-shadow:inset 0 0 0 1px ${css("--line")}"></i>sem dados</div>
         <p class="dica">Vermelho: sub-representação. Azul: sobre-representação.</p>`;
      $("#m-sub").textContent = `${estado.mGrupo === "mulheres" ? "Mulheres" : "Pessoas negras"} em ${MED_ROT[estado.mMedida]}, ${estado.mAno}${estado.partido ? `, lista do ${estado.partido}` : ", todas as listas"}. DRI: −1 = grupo ausente; 0 = paridade com ${estado.mGrupo === "mulheres" ? "o eleitorado" : "a população"} do distrito; 1 = grupo detém tudo.`;
      $("#m-nota").textContent = (estado.mGrupo === "negros" && estado.mAno < 2014 ? "O TSE só informa cor/raça a partir de 2014. " : "") +
        "Sem filtro de partido, o índice usa o conjunto das listas do distrito (participação ponderada pelo total da medida). Denominador p: mulheres no eleitorado (TSE) e pessoas pretas e pardas na população (Censo).";
      let linhasTab;
      if (municipal) {
        const porUf = new Map();
        for (const [d, o] of visiveis) { if (o.v == null) continue; const u = ufDe(d); (porUf.get(u) || porUf.set(u, []).get(u)).push(o.v); }
        linhasTab = [...porUf].sort().map(([u, a]) => { a.sort((x, y) => x - y); const m = a.length % 2 ? a[(a.length - 1) / 2] : (a[a.length / 2 - 1] + a[a.length / 2]) / 2;
          return `<tr><td>${u}</td><td class="n">${int(a.length)}</td><td class="n">${fmtDri(m)}</td><td class="n">${pct(a.filter((v) => v < -0.05).length / a.length, 0)}</td></tr>`; });
        $("#t-mapa").innerHTML = `<table><thead><tr><th>UF</th><th class="n">Municípios com dados</th><th class="n">DRI mediano</th><th class="n">Abaixo da paridade</th></tr></thead><tbody>${linhasTab.join("")}</tbody></table>`;
      } else {
        linhasTab = visiveis.sort((a, b) => (a[1].v ?? 9) - (b[1].v ?? 9)).map(([d, o]) =>
          `<tr><td>${esc(nomeDe(d))}</td><td class="n">${fmtDri(o.v)}</td><td class="n">${pct(o.b)}</td><td class="n">${pct(o.p)}</td><td class="n">${int(o.listas)}</td></tr>`);
        $("#t-mapa").innerHTML = `<table><thead><tr><th>UF</th><th class="n">DRI</th><th class="n">Participação (b)</th><th class="n">Referência (p)</th><th class="n">Listas</th></tr></thead><tbody>${linhasTab.join("")}</tbody></table>`;
      }
    } catch (e) {
      if (meu === tokenMapa) el.innerHTML = `<p class="vazio">Não foi possível carregar o mapa (${esc(e.message)}).</p>`;
    }
  }

  /* ------------------------------------------------------------ 3. partidos (barras) */
  // Barras horizontais por partido. modo "pct": 0 a máx, com linha de referência opcional;
  // modo "dri": divergente a partir de 0 (−1 a 1), cores da escala do mapa.
  function barras(el, dados, { modo = "pct", ref = null, refRot = "", detalhe }) {
    if (!dados.length) { el.innerHTML = `<p class="vazio">Sem dados para este recorte.</p>`; return; }
    const W = Math.max(320, el.clientWidth), bh = 18, gap = 6, m0 = { t: ref != null ? 22 : 8, r: 56, b: 24, l: 108 };
    const H = m0.t + m0.b + dados.length * (bh + gap), iw = W - m0.l - m0.r;
    let x, ticks;
    if (modo === "dri") {
      const lim = Math.min(1, Math.max(0.2, Math.ceil(Math.max(...dados.map((d) => Math.abs(d.v))) * 5) / 5));
      x = (v) => m0.l + ((v + lim) / (2 * lim)) * iw;
      ticks = []; for (let v = -lim; v <= lim + 1e-9; v += lim / 2) ticks.push([v, fmtDri(Math.round(v * 100) / 100)]);
    } else {
      const topo = Math.min(1, Math.ceil(Math.max(ref || 0, ...dados.map((d) => d.v)) * 10) / 10 || 0.1);
      x = (v) => m0.l + (v / topo) * iw;
      ticks = []; for (let v = 0; v <= topo + 1e-9; v += topo > 0.5 ? 0.2 : 0.1) ticks.push([v, Math.round(v * 100) + "%"]);
    }
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Barras por partido"><g class="grade">`;
    for (const [v, t] of ticks) s += `<line x1="${x(v)}" x2="${x(v)}" y1="${m0.t}" y2="${H - m0.b}"/><text x="${x(v)}" y="${H - 6}" text-anchor="middle">${t}</text>`;
    s += `</g>`;
    const x0 = modo === "dri" ? x(0) : m0.l;
    dados.forEach((d, i) => {
      const yy = m0.t + i * (bh + gap), sel = estado.partido === d.p, apagado = estado.partido && !sel;
      const cor = modo === "dri" ? corDri(d.v) : css(apagado ? "--bar-dim" : "--bar");
      const xa = x(d.v), w = Math.max(2, Math.abs(xa - x0)), esq = xa < x0, r = Math.min(4, w / 2);
      const forma = esq
        ? `M${x0} ${yy}h${-(w - r)}a${r} ${r} 0 0 0 ${-r} ${r}v${bh - 2 * r}a${r} ${r} 0 0 0 ${r} ${r}h${w - r}z`
        : `M${x0} ${yy}h${w - r}a${r} ${r} 0 0 1 ${r} ${r}v${bh - 2 * r}a${r} ${r} 0 0 1 ${-r} ${r}h${-(w - r)}z`;
      const rotulo = modo === "dri" ? fmtDri(d.v) : pct(d.v, 0);
      s += `<g class="barra" data-i="${i}" opacity="${apagado && modo === "dri" ? 0.45 : 1}"><rect x="0" y="${yy - gap / 2}" width="${W}" height="${bh + gap}" fill="transparent"/>` +
        `<text x="${m0.l - 8}" y="${yy + bh / 2 + 4}" text-anchor="end" style="fill:var(${sel ? "--text" : "--text-2"});font-weight:${sel ? 700 : 400}">${esc(d.p)}</text>` +
        `<path d="${forma}" fill="${cor}"/>` +
        `<text x="${esq ? xa - 6 : xa + 6}" y="${yy + bh / 2 + 4}" text-anchor="${esq ? "end" : "start"}">${rotulo}</text></g>`;
    });
    if (modo === "dri") s += `<line x1="${x0}" x2="${x0}" y1="${m0.t - 4}" y2="${H - m0.b}" stroke="${css("--muted")}"/>`;
    if (ref != null) s += `<line x1="${x(ref)}" x2="${x(ref)}" y1="${m0.t - 6}" y2="${H - m0.b}" stroke="${css("--text")}" stroke-dasharray="4 3"/>` +
      `<text x="${x(ref)}" y="${m0.t - 9}" text-anchor="middle" style="fill:var(--text)">${esc(refRot)} ${pct(ref, 0)}</text>`;
    el.innerHTML = s + `</svg>`;
    const tip = $("#tooltip");
    el.querySelectorAll(".barra").forEach((g) => {
      g.addEventListener("pointermove", (ev) => {
        tip.innerHTML = detalhe(dados[+g.dataset.i]); tip.hidden = false;
        tip.style.left = Math.min(ev.clientX + 14, innerWidth - tip.offsetWidth - 8) + "px";
        tip.style.top = Math.max(8, ev.clientY - tip.offsetHeight - 12) + "px";
      });
      g.addEventListener("pointerleave", () => (tip.hidden = true));
    });
  }
  const linhaTip = (a, b) => `<div class="linha"><span>${a}</span><span>${b}</span></div>`;

  const DIMS = {
    direcoes: { sub: "Percentual de mulheres nas direções partidárias." },
    candidaturas: { medida: "candidaturas", sub: "Participação do grupo nas candidaturas da lista do partido." },
    eleitos: { medida: "eleitos", sub: "Participação do grupo entre os eleitos da lista do partido." },
    financiamento: { sub: "Participação do grupo nas receitas de campanha dos candidatos do partido." },
    equidade: { sub: "Índice de equidade (DRI) do partido: participação do grupo comparada ao seu peso no eleitorado (mulheres) ou na população (pessoas negras)." },
  };
  let tokenPart = 0;

  async function partidos() {
    const el = $("#g-partidos"), dim = estado.pDim, eleitoral = dim !== "direcoes";
    // mostra só os controles da dimensão
    $("#p-nivel-l").hidden = eleitoral; $("#p-cargo-l").hidden = !eleitoral; $("#p-grupo").hidden = !eleitoral;
    $("#p-medida-l").hidden = !(dim === "financiamento" || dim === "equidade");
    const medOpts = dim === "financiamento" ? ["receitas", "receitas_publicas"] : ["candidaturas", "eleitos", "votos", "receitas", "receitas_publicas"];
    if (!medOpts.includes(estado.pMedida)) estado.pMedida = medOpts[0];
    opcoes($("#p-medida"), medOpts.map((m) => [m, MED_ROT[m][0].toUpperCase() + MED_ROT[m].slice(1)])); $("#p-medida").value = estado.pMedida;
    const grupoTxt = estado.pGrupo === "mulheres" ? "Mulheres" : "Pessoas negras";

    if (!eleitoral) {
      const anos = [...new Set(DIR.map((r) => r.ano))].sort();
      if (!anos.includes(estado.pAno)) estado.pAno = anos[anos.length - 1];
      opcoes($("#p-ano"), anos.slice().reverse().map((a) => [a, a])); $("#p-ano").value = estado.pAno;
      $("#p-sub").textContent = DIMS.direcoes.sub;
      $("#p-nota").textContent = "Só aparecem partidos com pelo menos 20 membros no recorte. O partido escolhido no filtro fica destacado.";
      const rows = filtra(DIR, { partido: false }).filter((r) => r.nivel === estado.pNivel && r.ano === estado.pAno);
      const m = soma(rows, (r) => r.partido, ["mulheres", "membros"]);
      const dados = [...m].filter(([, o]) => o.membros >= 20).map(([p, o]) => ({ p, v: o.mulheres / o.membros, n: o.membros, mu: o.mulheres })).sort((a, b) => b.v - a.v);
      barras(el, dados, { detalhe: (d) => `<b>${esc(d.p)}</b>` + linhaTip("Mulheres", pct(d.v)) + linhaTip("Membros", `${int(d.mu)} de ${int(d.n)}`) });
      return;
    }

    const meu = ++tokenPart;
    try {
      IND_MAPA ||= await json("dados/mapa/indice.json");
      const anos = IND_MAPA[estado.pCargo] || [];
      if (!anos.includes(estado.pAno)) estado.pAno = anos[anos.length - 1];
      opcoes($("#p-ano"), anos.slice().reverse().map((a) => [a, a])); $("#p-ano").value = estado.pAno;
      const municipal = estado.pCargo === "vereanca";
      const [doc, nomes] = await Promise.all([json(`dados/mapa/${estado.pCargo}_${estado.pAno}.json`), municipal ? json("dados/geo/nomes.json") : null]);
      if (meu !== tokenPart) return;
      const ufDe = (d) => municipal ? (nomes[d] || "").slice(-3, -1) : d;
      const med = dim === "candidaturas" || dim === "eleitos" ? DIMS[dim].medida : estado.pMedida;
      const k = doc.medidas.indexOf(med), g = estado.pGrupo === "mulheres" ? 0 : 1;
      // por partido: b = Σ b·T / Σ T e p = Σ p·T / Σ T (p do distrito ponderado pelo peso da lista)
      const acc = new Map();
      for (const l of doc.linhas) {
        if (estado.uf && ufDe(l[0]) !== estado.uf) continue;
        const p = doc.partidos[l[1]], T = l[2 + 3 * k], b = l[3 + 3 * k + g], ref = doc.distritos[l[0]]?.[g];
        let o = acc.get(p); if (!o) acc.set(p, (o = { num: 0, den: 0, pnum: 0, cand: 0, listas: 0 }));
        o.cand += l[2]; o.listas++;
        if (b != null && ref != null && T > 0) { o.num += b * T; o.den += T; o.pnum += ref * T; }
      }
      const dados = [...acc].filter(([p, o]) => p !== "?" && o.cand >= 20 && o.den > 0).map(([p, o]) => {
        const b = o.num / o.den, ref = o.pnum / o.den;
        return { p, b, ref, v: dim === "equidade" ? dri(b, ref) : b, T: o.den, cand: o.cand, listas: o.listas };
      }).sort((a, b) => b.v - a.v);
      // referência geral (todas as listas): peso do grupo ponderado
      const tot = dados.reduce((s, d) => ((s.r += d.ref * d.T), (s.t += d.T), s), { r: 0, t: 0 });
      const refGeral = tot.t ? tot.r / tot.t : null;
      const quem = estado.pGrupo === "mulheres" ? "eleitorado" : "população";
      const fmtT = (v) => med === "receitas" || med === "receitas_publicas" ? "R$ " + int(v) : int(v);
      $("#p-sub").textContent = `${DIMS[dim].sub} ${grupoTxt}, ${$("#p-cargo").selectedOptions[0].text.replace(/ \(.*\)/, "")}, ${estado.pAno}${estado.uf ? ", " + UFS_NOMES[estado.uf] : ""}${dim === "equidade" || dim === "financiamento" ? " (" + MED_ROT[med] + ")" : ""}.`;
      $("#p-nota").textContent = (dim === "equidade"
        ? "DRI: −1 = grupo ausente; 0 = paridade; 1 = grupo detém tudo. O peso de referência de cada partido é o do " + quem + " dos distritos onde ele lançou lista, ponderado pelo tamanho da lista. "
        : `Linha tracejada: peso do grupo no ${quem} dos distritos (ponderado). `) +
        "Só aparecem partidos com pelo menos 20 candidaturas no recorte." + (estado.pGrupo === "negros" && estado.pAno < 2014 ? " O TSE só informa cor/raça a partir de 2014." : "");
      barras(el, dados, {
        modo: dim === "equidade" ? "dri" : "pct", ref: dim === "equidade" ? null : refGeral, refRot: quem === "eleitorado" ? "Eleitorado" : "População",
        detalhe: (d) => `<b>${esc(d.p)}</b>` + (dim === "equidade" ? linhaTip("DRI", `<strong>${fmtDri(d.v)}</strong>`) : "") +
          linhaTip(`${grupoTxt} em ${MED_ROT[med]}`, pct(d.b)) + linhaTip(`No ${quem} (p)`, pct(d.ref)) +
          linhaTip(MED_ROT[med][0].toUpperCase() + MED_ROT[med].slice(1), fmtT(d.T)) + linhaTip("Listas", int(d.listas)),
      });
    } catch (e) {
      if (meu === tokenPart) el.innerHTML = `<p class="vazio">Não foi possível carregar os dados (${esc(e.message)}).</p>`;
    }
  }

  /* ------------------------------------------------------------ 4. arquivos */
  function arquivos() {
    const grupo = estado.aGrupo;
    if (!ARQ.length) { $("#t-arquivos").innerHTML = `<p class="vazio">Os arquivos estão sendo enviados para a <a href="${REPO}/releases">release</a> e aparecem aqui em breve.</p>`; return; }
    const lista = ARQ.filter((a) => grupo === "finais" ? !/^etapa[12]_/.test(a.arquivo) : a.arquivo.startsWith(grupo + "_"));
    const mb = (b) => b >= 1e9 ? (b / 1e9).toLocaleString("pt-BR", { maximumFractionDigits: 2 }) + " GB" : (b / 1e6).toLocaleString("pt-BR", { maximumFractionDigits: 1 }) + " MB";
    const dic = (n) => { const base = n.replace(/\.csv\.gz$/, "").replace(/_\d{4}$/, ""); return grupo === "finais" ? `${REPO}/blob/main/3%20Produtos%20finais/dicionarios/dicionario_${base}_variaveis.csv` : null; };
    $("#t-arquivos").innerHTML = `<table><thead><tr><th>Arquivo</th><th class="n">Tamanho (.gz)</th>${grupo === "finais" ? "<th>Dicionário</th>" : "<th>Origem</th>"}</tr></thead><tbody>` +
      lista.map((a) => `<tr><td><a href="${REPO}/releases/latest/download/${encodeURIComponent(a.arquivo)}">${esc(a.arquivo)}</a></td><td class="n">${mb(a.bytes)}</td>` +
        (grupo === "finais" ? `<td><a href="${dic(a.arquivo)}">variáveis</a></td>` : `<td class="dica">${esc(a.origem)}</td>`) + `</tr>`).join("") + `</tbody></table>`;
  }
})();
