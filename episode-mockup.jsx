import React, { useState, useEffect } from "react";
import {
  ComposedChart, Bar, Line, Area, XAxis, YAxis, ReferenceArea, ReferenceLine,
  Tooltip, ResponsiveContainer, Cell, LineChart, AreaChart, Legend
} from "recharts";
import {
  AlertTriangle, Building2, MapPin, Check, Clock, FileText, Wand2, Info
} from "lucide-react";

/* ------------------------------------------------------------------ *
 *  EpiSODE - Epidemiological Signal Observation and Detection Engine
 *  Cluster dossier mockup. Synthetic data only.
 * ------------------------------------------------------------------ */

const C = {
  /* certestyle::certe.colours */
  blauw:  "#4A647D", blauw0: "#3A4D5D", blauw2: "#69849C", blauw3: "#97AABB",
  blauw4: "#C5D0DB", blauw5: "#E2E7EC", blauw6: "#F6F7F8",
  groen:  "#93984C", groen0: "#5A5D33", groen3: "#C9CCA5", groen5: "#EEEFE4",
  roze:   "#B4527F", roze0:  "#7F3C5B", roze3:  "#D5ACBF", roze5:  "#F2E6EB",
  geel:   "#E4D559", geel0:  "#D4C230", geel3:  "#ECE6B1", geel5:  "#F9F7E8",
  lila:   "#CEB9D6", lila0:  "#BEA5C7", lila3:  "#E6DDE9",
  bruin:  "#998961", bruin0: "#675D45",

  /* semantische rollen */
  ink: "#3A4D5D", muted: "#4A647D", faint: "#97AABB",
  rule: "#C5D0DB", track: "#E2E7EC", paper: "#F6F7F8", surface: "#FFFFFF",
  petrol: "#4A647D", petrolD: "#3A4D5D", petrolL: "#69849C",
  carmine: "#7F3C5B", pink: "#B4527F", pinkTint: "#F2E6EB",
  olive: "#93984C", oliveD: "#5A5D33",
  yellow: "#E4D559", yellowD: "#675D45",
  lilac: "#CEB9D6",
  band: "#C5D0DB", hatch: "#C5D0DB", tint: "#E2E7EC", soft: "#F6F7F8",
};

const BRAND_BAR = ["#E4D559", "#93984C", "#B4527F", "#4A647D", "#CEB9D6"];

const CLASSES = [
  { key: "artefact",    label: "Artefact",                colour: C.muted,   term: true,
    hint: "Detectiefout of registratie-effect. Sluit direct." },
  { key: "variation",   label: "Normale variatie",        colour: C.muted,   term: true,
    hint: "Binnen verwachting voor dit seizoen. Sluit direct." },
  { key: "no_epidemic", label: "Cluster, nog geen epidemie", colour: C.oliveD, term: false,
    hint: "Echte clustering. Volgen, geen uitbraakmaatregelen." },
  { key: "possible",    label: "Mogelijke epidemie",      colour: C.yellowD, term: false,
    hint: "Opvolging en bronopsporing gestart." },
  { key: "confirmed",   label: "Bevestigde epidemie",     colour: C.carmine, term: false,
    hint: "Uitbraakbeheersing gestart. Meldingsplicht toetsen." },
];

const TERMINAL = CLASSES.filter((x) => x.term).map((x) => x.key);

const STATES = {
  new:        { label: "Nieuw",                 colour: C.blauw },
  assessing:  { label: "In beoordeling",        colour: C.blauw },
  monitoring: { label: "Monitoring",            colour: C.roze },
  closable:   { label: "Af te sluiten",         colour: C.geel0 },
  closed:     { label: "Afgesloten",            colour: C.groen0 },
  reassess:   { label: "Herbeoordeling nodig",  colour: C.bruin0 },
};

/* Status wordt afgeleid, nooit gekozen. */
function derivedState(c) {
  if (c.closedByHuman) return "closed";
  if (!c.klass) return c.timeline.length ? "assessing" : "new";
  if (TERMINAL.includes(c.klass)) return "closed";
  if (c.changed) return "reassess";
  return c.caseFree.since >= c.caseFree.need ? "closable" : "monitoring";
}

const MONTHS = ["januari","februari","maart","april","mei","juni","juli",
                "augustus","september","oktober","november","december"];
function longDate(ddmm) {
  const [d, m] = ddmm.split(" ")[0].split("-");
  return `${parseInt(d, 10)} ${MONTHS[parseInt(m, 10) - 1]} 2026`;
}

/* Niveaus lopen op met aggregatie, conform de conventie in multilevelmodellen.
   Opschaling is dus een hoger nummer. Zorglijn is geen niveau maar een filter. */
const LEVELS = {
  ward:        "L1 · afdeling",
  institution: "L2 · instelling",
  area:        "L3 · gebied",
  province:    "L4 · provincie",
  region:      "L5 · regio",
};

/* ---------------- synthetic data ---------------- */

function weekly(years, base, amp, phase, spikeWeeks, spikeSize) {
  const n = years * 52;
  const out = [];
  for (let i = 0; i < n; i++) {
    const seasonal = base * (1 + amp * Math.sin(((i % 52) / 52) * 2 * Math.PI + phase));
    const noise = 1 + Math.sin(i * 3.7) * 0.22 + Math.cos(i * 1.31) * 0.15;
    let obs = Math.max(0, Math.round(seasonal * noise));
    const fromEnd = n - i;
    if (fromEnd <= spikeWeeks) obs += Math.round(spikeSize * ((spikeWeeks - fromEnd + 1) / spikeWeeks));
    out.push({
      i,
      label: `${2022 + Math.floor(i / 52)}-w${String((i % 52) + 1).padStart(2, "0")}`,
      obs,
      expected: Math.round(seasonal * 10) / 10,
      upper: Math.round((seasonal * 1.85 + 2.2) * 10) / 10,
      incomplete: fromEnd <= 2,
    });
  }
  return out;
}

function daily(nDays, cases) {
  const out = [];
  for (let i = 0; i < nDays; i++) {
    const d = new Date(2026, 6, 20 + i);
    const key = `${String(d.getDate()).padStart(2, "0")}-${String(d.getMonth() + 1).padStart(2, "0")}`;
    out.push({ label: key, obs: cases[i] || 0, incomplete: i >= nDays - 9 });
  }
  return out;
}

const CLUSTERS = [
  {
    id: 1041,
    mo: "Norovirus GII",
    italic: false,
    level: "ward",
    place: "Martini Ziekenhuis · afdeling B4 Interne",
    careLine: "second",
    detectors: ["same_place", "clusters"],
    lifecycle: "new",
    klass: null,
    changed: false,
    first: "14-08-2026", last: "17-08-2026",
    obs: 11, exp: 1.4, ratio: 7.9, priority: 91,
    density: { value: 2.41, baseline: 0.31 },
    doubling: 2.8, rt: [1.9, 2.4, 2.2],
    rtApplicable: true,
    hist: weekly(4, 2.2, 0.55, 1.2, 3, 9),
    days: daily(29, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,3,2,2]),
    tests: [{ w: "w29", n: 210, pos: 1.0 }, { w: "w30", n: 198, pos: 1.5 },
            { w: "w31", n: 205, pos: 1.0 }, { w: "w32", n: 214, pos: 3.7 },
            { w: "w33", n: 221, pos: 5.0 }],
    demo: [{ band: "0-19", m: 0, v: 0, bm: 0.4, bv: 0.5 }, { band: "20-39", m: 0, v: 1, bm: 0.3, bv: 0.6 },
           { band: "40-59", m: 1, v: 0, bm: 0.5, bv: 0.5 }, { band: "60-79", m: 2, v: 3, bm: 0.8, bv: 0.9 },
           { band: "80+", m: 2, v: 2, bm: 0.6, bv: 1.1 }],
    geo: [{ k: "9713", n: 3 }, { k: "9714", n: 2 }, { k: "9711", n: 2 }, { k: "9721", n: 2 }, { k: "9726", n: 2 }],
    places: [{ k: "B4 Interne", n: 9 }, { k: "B3 Chirurgie", n: 1 }, { k: "SEH", n: 1 }],
    abx: null,
    cases: [
      { id: "N-8821", date: "14-08", sex: "V", age: 81, pc4: "9713", ward: "B4 Interne", mat: "Faeces" },
      { id: "N-8830", date: "14-08", sex: "M", age: 76, pc4: "9714", ward: "B4 Interne", mat: "Faeces" },
      { id: "N-8844", date: "15-08", sex: "V", age: 88, pc4: "9711", ward: "B4 Interne", mat: "Faeces" },
      { id: "N-8851", date: "16-08", sex: "V", age: 79, pc4: "9721", ward: "B4 Interne", mat: "Faeces" },
      { id: "N-8866", date: "16-08", sex: "M", age: 84, pc4: "9713", ward: "B4 Interne", mat: "Faeces" },
      { id: "N-8871", date: "17-08", sex: "V", age: 90, pc4: "9726", ward: "B4 Interne", mat: "Faeces" },
    ],
    traject: [{ s: "new", from: "14-08", d: 1 }],
    unique: { patients: 9, isolates: 11 },
    caseFree: { since: 1, need: 4, rule: "2 × maximale incubatietijd (norovirus, 2 dagen)" },
    closureRule: "casusvrije termijn van 4 dagen",
    shape: "Puntbron: alle gevallen binnen één maximale incubatietijd",
    analogues: [
      { id: 1012, mo: "Norovirus GII", place: "Ommelander · A2", when: "feb 2026", klass: "confirmed", sim: 0.91 },
      { id: 884, mo: "Norovirus GII", place: "Martini · B4", when: "jan 2025", klass: "confirmed", sim: 0.87 },
      { id: 812, mo: "Norovirus GI", place: "Refaja · C1", when: "dec 2024", klass: "no_epidemic", sim: 0.64 },
    ],
    duiding: [
      "Elf gevallen in vier dagen tegen een verwachting van 1,4 per week: een ratio van 7,9. Negen van de elf zijn afkomstig van één afdeling, B4 Interne, wat de regel voor plaatsgebonden clustering ruim overschrijdt (drempel 3 binnen 14 dagen).",
      "De positiviteit stijgt mee met het aantal, van 1,0% naar 5,0% over vier weken bij vrijwel constante testaantallen. De toename is dus niet toe te schrijven aan testvolume.",
      "De incidentiedichtheid bedraagt 2,41 per 1.000 verpleegdagen tegen een basislijn van 0,31, waarmee bezettingsgraad als verklaring afvalt.",
      "De leeftijdsverdeling is sterk verschoven naar 60 jaar en ouder, passend bij de afdelingspopulatie en niet bij een bredere introductie in het gebied.",
      "Let op: de laatste negen dagen zijn onvolledig door de rapportagevertraging. Het werkelijke aantal ligt vermoedelijk hoger.",
    ],
    advies: "Overweeg classificatie als mogelijke epidemie en afstemming met infectiepreventie van het Martini Ziekenhuis over afdelingssluiting en cohortverpleging.",
    timeline: [],
  },
  {
    id: 1038,
    mo: "Campylobacter jejuni",
    italic: true,
    level: "area",
    place: "Gebied Noord-Drenthe · eerste lijn",
    careLine: "first",
    detectors: ["farrington", "clusters"],
    lifecycle: "assessing",
    klass: null,
    changed: true,
    first: "05-08-2026", last: "16-08-2026",
    obs: 24, exp: 11.2, ratio: 2.1, priority: 74,
    density: null,
    doubling: 9.4, rt: [1.2, 1.3, 1.1],
    rtApplicable: true,
    hist: weekly(4, 11, 0.42, 0.4, 4, 12),
    days: daily(29, [1,0,1,2,1,0,1,1,2,1,0,2,1,1,2,1,0,1,2,1,1,0,1,2,1,1,2,1,0]),
    tests: [{ w: "w29", n: 640, pos: 1.7 }, { w: "w30", n: 655, pos: 1.8 },
            { w: "w31", n: 810, pos: 2.0 }, { w: "w32", n: 1180, pos: 2.0 },
            { w: "w33", n: 1210, pos: 2.0 }],
    demo: [{ band: "0-19", m: 3, v: 2, bm: 0.9, bv: 0.8 }, { band: "20-39", m: 5, v: 6, bm: 1.0, bv: 1.1 },
           { band: "40-59", m: 3, v: 2, bm: 0.9, bv: 0.9 }, { band: "60-79", m: 2, v: 1, bm: 0.8, bv: 0.7 },
           { band: "80+", m: 0, v: 0, bm: 0.3, bv: 0.4 }],
    geo: [{ k: "9401", n: 9 }, { k: "9403", n: 6 }, { k: "9411", n: 5 }, { k: "9405", n: 4 }],
    places: [{ k: "Huisartsen Assen", n: 14 }, { k: "Huisartsen Roden", n: 6 }, { k: "HAP Assen", n: 4 }],
    abx: {
      abx: ["CIP", "ERY", "TET", "GEN"],
      isolates: [
        { id: "C-4410", p: ["R", "S", "R", "S"] },
        { id: "C-4418", p: ["R", "S", "R", "S"] },
        { id: "C-4425", p: ["R", "S", "R", "S"] },
        { id: "C-4431", p: ["S", "S", "S", "S"] },
        { id: "C-4440", p: ["R", "S", "R", "S"] },
      ],
    },
    cases: [
      { id: "C-4410", date: "09-08", sex: "M", age: 34, pc4: "9401", ward: null, mat: "Faeces" },
      { id: "C-4418", date: "10-08", sex: "V", age: 29, pc4: "9403", ward: null, mat: "Faeces" },
      { id: "C-4425", date: "12-08", sex: "M", age: 51, pc4: "9401", ward: null, mat: "Faeces" },
      { id: "C-4431", date: "13-08", sex: "V", age: 22, pc4: "9411", ward: null, mat: "Faeces" },
      { id: "C-4440", date: "15-08", sex: "M", age: 41, pc4: "9401", ward: null, mat: "Faeces" },
    ],
    traject: [{ s: "new", from: "09-08", d: 8 }, { s: "assessing", from: "17-08", d: 1 }, { s: "reassess", from: "18-08", d: 0 }],
    unique: { patients: 22, isolates: 24 },
    caseFree: { since: 2, need: 14, rule: "case_free_days = 14" },
    closureRule: "casusvrije termijn van 14 dagen",
    shape: "Propagerend of aanhoudende gemeenschappelijke bron: spreiding over meer dan één incubatietijd",
    analogues: [
      { id: 962, mo: "Campylobacter coli", place: "Noord-Drenthe", when: "mrt 2025", klass: "artefact", sim: 0.83 },
      { id: 903, mo: "Campylobacter jejuni", place: "Westerkwartier", when: "aug 2024", klass: "possible", sim: 0.79 },
      { id: 845, mo: "Campylobacter jejuni", place: "Noord-Drenthe", when: "jul 2023", klass: "no_epidemic", sim: 0.71 },
    ],
    duiding: [
      "Vierentwintig gevallen tegen een verwachting van 11,2: een ratio van 2,1, boven de signaalgrens van 18,4 in twee opeenvolgende weken.",
      "Waarschuwing bij de noemer: het testvolume steeg van 655 naar 1.210 per week, terwijl de positiviteit vrijwel constant bleef op circa 2,0%. Een groot deel van de toename is daarmee toe te schrijven aan testvolume, niet aan incidentie.",
      "Vier van de vijf getypeerde isolaten delen een identiek fenotypisch resistentieprofiel (CIP-R, TET-R), wat clonaliteit niet bewijst maar wel een gemeenschappelijke bron plausibel maakt.",
      "De geografische spreiding concentreert zich rond PC4 9401 en 9403, met een piek in de leeftijdsgroep 20 tot 39 jaar. Het cluster blijft daarmee op gebiedsniveau (L3) en is niet opgeschaald naar de provincie.",
      "Dit cluster is na de vorige beoordeling gewijzigd: drie nagekomen gevallen met afnamedatum in week 32.",
    ],
    advies: "De noemerbeweging en het gedeelde resistentieprofiel wijzen twee kanten op. Bronopsporing via de GGD Drenthe is de aangewezen volgende stap voordat een classificatie wordt vastgelegd.",
    timeline: [
      { who: "Matthijs Berends", when: "17-08 08:40", klass: null, text: "Positiviteit vlak bij stijgend testvolume. Navraag GGD Drenthe over gemeenschappelijke horecabron." },
    ],
  },
  {
    id: 1029,
    mo: "Legionella pneumophila",
    italic: true,
    level: "area",
    place: "Gebied Groningen-stad · eerste lijn",
    careLine: "first",
    detectors: ["rare_trigger"],
    lifecycle: "new",
    klass: null,
    changed: false,
    first: "15-08-2026", last: "17-08-2026",
    obs: 3, exp: 0.3, ratio: 10.0, priority: 63,
    density: null,
    doubling: null, rt: null,
    rtApplicable: false,
    hist: weekly(4, 0.35, 0.6, 2.0, 2, 2.4),
    days: daily(29, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1]),
    tests: [{ w: "w29", n: 92, pos: 0 }, { w: "w30", n: 88, pos: 0 },
            { w: "w31", n: 95, pos: 1.1 }, { w: "w32", n: 90, pos: 1.1 },
            { w: "w33", n: 97, pos: 2.1 }],
    demo: [{ band: "40-59", m: 1, v: 0, bm: 0.5, bv: 0.2 }, { band: "60-79", m: 1, v: 1, bm: 0.9, bv: 0.5 }],
    geo: [{ k: "9718", n: 2 }, { k: "9712", n: 1 }],
    places: [{ k: "Huisartsen Groningen-centrum", n: 3 }],
    abx: null,
    cases: [
      { id: "L-0071", date: "15-08", sex: "M", age: 63, pc4: "9718", ward: null, mat: "Sputum" },
      { id: "L-0074", date: "16-08", sex: "M", age: 57, pc4: "9718", ward: null, mat: "Urine (antigeen)" },
      { id: "L-0079", date: "17-08", sex: "V", age: 71, pc4: "9712", ward: null, mat: "Sputum" },
    ],
    traject: [{ s: "new", from: "17-08", d: 1 }],
    unique: { patients: 3, isolates: 3 },
    caseFree: { since: 1, need: 21, rule: "case_free_days = 21 (Legionella)" },
    closureRule: "casusvrije termijn van 21 dagen",
    shape: "Puntbron: alle gevallen binnen één maximale incubatietijd (2 tot 10 dagen)",
    analogues: [
      { id: 871, mo: "Legionella pneumophila", place: "Groningen-stad", when: "sep 2024", klass: "possible", sim: 0.88 },
      { id: 793, mo: "Legionella pneumophila", place: "Assen", when: "jun 2023", klass: "confirmed", sim: 0.74 },
    ],
    duiding: [
      "Drie gevallen binnen drie dagen. Voor deze verwekker geldt geen statistische signaalgrens: de detectie komt van de curatieve triggerlijst, waarbij elk cluster van drie of meer binnen veertien dagen wordt opgepakt ongeacht basislijn.",
      "Twee van de drie patiënten wonen in hetzelfde PC4-gebied 9718, wat een gemeenschappelijke omgevingsbron plausibel maakt.",
      "Geen Rt: Legionella kent geen mens-op-mens transmissie, dus een reproductiegetal is hier betekenisloos en wordt niet berekend.",
      "Bij deze verwekker is de meldingsplicht op grond van de Wet publieke gezondheid van toepassing (groep C).",
    ],
    advies: "Directe afstemming met GGD Groningen voor bronopsporing naar koeltorens, whirlpools en leidingwaterinstallaties in het gebied 9718.",
    timeline: [],
  },
];


const ARCHIVE = [
  { id: 1012, mo: "Norovirus GII", italic: false, level: "ward", place: "Ommelander Ziekenhuis · A2", n: 14,
    klass: "confirmed", opened: "03-02-2026", closed: "24-02-2026", by: "M. Berends", dur: 21 },
  { id: 1007, mo: "Influenza A(H3N2)", italic: false, level: "region", place: "Noord-Nederland · eerste lijn", n: 412,
    klass: "confirmed", opened: "12-12-2025", closed: "18-03-2026", by: "J. Vissering", dur: 96 },
  { id: 998, mo: "Salmonella Enteritidis", italic: true, level: "area", place: "Gebied Westerkwartier", n: 9,
    klass: "possible", opened: "18-09-2025", closed: "20-10-2025", by: "M. Berends", dur: 32 },
  { id: 991, mo: "Klebsiella pneumoniae (OXA-48)", italic: true, level: "institution", place: "UMCG", n: 5,
    klass: "confirmed", opened: "02-07-2025", closed: "29-08-2025", by: "A. de Wit", dur: 58 },
  { id: 977, mo: "Streptococcus pyogenes", italic: true, level: "region", place: "Noord-Nederland · eerste lijn", n: 31,
    klass: "variation", opened: "14-05-2025", closed: "02-06-2025", by: "J. Vissering", dur: 19 },
  { id: 962, mo: "Campylobacter coli", italic: true, level: "area", place: "Gebied Noord-Drenthe", n: 18,
    klass: "artefact", opened: "08-03-2025", closed: "11-03-2025", by: "A. de Wit", dur: 3 },
  { id: 950, mo: "Clostridioides difficile", italic: true, level: "institution", place: "Martini Ziekenhuis", n: 11,
    klass: "no_epidemic", opened: "21-01-2025", closed: "19-02-2025", by: "H. Bosma", dur: 29 },
];

const ACTIVITY = [
  { day: "18-08-2026", items: [
    { t: "09:41", who: "M. Berends", act: "Beoordeling vastgelegd", tgt: "#1038 Campylobacter jejuni", det: "In beoordeling · nog geen classificatie" },
    { t: "08:52", who: "H. Bosma",   act: "Cluster geopend",        tgt: "#1041 Norovirus GII", det: "" },
    { t: "04:12", who: "systeem",    act: "Detectierun geslaagd",   tgt: "1.284 streams", det: "3 clusters, 1 nieuw" },
  ]},
  { day: "17-08-2026", items: [
    { t: "16:20", who: "A. de Wit",  act: "Rapport gegenereerd",    tgt: "#1029 Legionella pneumophila", det: "versie 1, verstuurd naar medische staf" },
    { t: "08:40", who: "M. Berends", act: "Beoordeling vastgelegd", tgt: "#1038 Campylobacter jejuni", det: "In beoordeling" },
    { t: "04:11", who: "systeem",    act: "Detectierun geslaagd",   tgt: "1.281 streams", det: "2 clusters" },
  ]},
  { day: "14-08-2026", items: [
    { t: "11:05", who: "J. Vissering", act: "Stream gedempt",       tgt: "Rotavirus · eerste lijn", det: "seizoensmatig, tot 01-11-2026" },
    { t: "04:12", who: "systeem",      act: "Detectierun geslaagd", tgt: "1.279 streams", det: "1 cluster, 1 nieuw" },
  ]},
];

/* ---------------- primitives ---------------- */

const Mo = ({ c }) => <span style={{ fontStyle: c.italic ? "italic" : "normal" }}>{c.mo}</span>;

function Chip({ children, colour = C.muted, filled }) {
  return <span style={{
    fontSize: 10.5, letterSpacing: 0.3, textTransform: "uppercase",
    padding: "2px 7px", borderRadius: 2, whiteSpace: "nowrap",
    color: filled ? "#fff" : colour, background: filled ? colour : "transparent",
    border: filled ? "none" : `1px solid ${colour}44`,
  }}>{children}</span>;
}

function Panel({ title, note, children, aside }) {
  return (
    <section style={{ background: C.surface, border: `1px solid ${C.rule}`, borderRadius: 2, marginBottom: 16 }}>
      <div style={{ padding: "12px 18px", borderBottom: `1px solid ${C.rule}` }}
           className="flex items-baseline justify-between">
        <h2 style={{ fontSize: 13.5, fontWeight: 600, letterSpacing: 0.2 }}>{title}</h2>
        {aside && <span style={{ fontSize: 11.5, color: C.muted }}>{aside}</span>}
      </div>
      <div style={{ padding: 18 }}>
        {children}
        {note && <p style={{ fontSize: 12, color: C.muted, lineHeight: 1.6, marginTop: 14 }}>{note}</p>}
      </div>
    </section>
  );
}

function Stat({ label, value, sub, colour }) {
  return (
    <div>
      <div style={{ fontSize: 10, letterSpacing: 0.6, textTransform: "uppercase", color: C.muted }}>{label}</div>
      <div style={{ fontSize: 24, fontWeight: 600, fontVariantNumeric: "tabular-nums", color: colour || C.ink, lineHeight: 1.15 }}>{value}</div>
      {sub && <div style={{ fontSize: 11.5, color: C.muted, fontVariantNumeric: "tabular-nums" }}>{sub}</div>}
    </div>
  );
}

/* ---------------- charts ---------------- */

function DailyCurve({ c }) {
  const firstInc = c.days.findIndex((d) => d.incomplete);
  return (
    <div style={{ height: 210 }}>
      <ResponsiveContainer>
        <ComposedChart data={c.days} margin={{ top: 6, right: 10, bottom: 0, left: -22 }}>
          {firstInc > -1 && (
            <ReferenceArea x1={c.days[firstInc].label} x2={c.days[c.days.length - 1].label}
                           fill={C.hatch} fillOpacity={0.5} stroke="none" />
          )}
          <XAxis dataKey="label" tick={{ fontSize: 9.5, fill: C.faint }} interval={3}
                 axisLine={{ stroke: C.rule }} tickLine={false} />
          <YAxis tick={{ fontSize: 10, fill: C.faint }} axisLine={false} tickLine={false} allowDecimals={false} />
          <Tooltip contentStyle={{ fontSize: 12, borderRadius: 2, border: `1px solid ${C.rule}` }} />
          <Bar dataKey="obs" name="Gevallen" barSize={9}>
            {c.days.map((d, i) => <Cell key={i} fill={C.petrol} fillOpacity={d.incomplete ? 0.45 : 1} />)}
          </Bar>
        </ComposedChart>
      </ResponsiveContainer>
    </div>
  );
}

function LongTrend({ c }) {
  const d = c.hist.slice(-156);
  return (
    <div style={{ height: 230 }}>
      <ResponsiveContainer>
        <ComposedChart data={d} margin={{ top: 6, right: 10, bottom: 0, left: -22 }}>
          <defs>
            <linearGradient id="bandg" x1="0" y1="0" x2="0" y2="1">
              <stop offset="0%" stopColor={C.band} stopOpacity={0.9} />
              <stop offset="100%" stopColor={C.band} stopOpacity={0.25} />
            </linearGradient>
          </defs>
          <XAxis dataKey="label" tick={{ fontSize: 9.5, fill: C.faint }} interval={25}
                 axisLine={{ stroke: C.rule }} tickLine={false} />
          <YAxis tick={{ fontSize: 10, fill: C.faint }} axisLine={false} tickLine={false} />
          <Tooltip contentStyle={{ fontSize: 12, borderRadius: 2, border: `1px solid ${C.rule}` }} />
          <Area type="monotone" dataKey="upper" name="Signaalgrens" stroke="none" fill="url(#bandg)" />
          <Line type="monotone" dataKey="expected" name="Verwacht" stroke={C.petrolL}
                strokeWidth={1.2} strokeDasharray="4 3" dot={false} />
          <Line type="monotone" dataKey="obs" name="Waargenomen" stroke={C.ink} strokeWidth={1.6} dot={false} />
        </ComposedChart>
      </ResponsiveContainer>
    </div>
  );
}

function Denominator({ c }) {
  return (
    <div style={{ height: 200 }}>
      <ResponsiveContainer>
        <ComposedChart data={c.tests} margin={{ top: 6, right: 6, bottom: 0, left: -22 }}>
          <XAxis dataKey="w" tick={{ fontSize: 10, fill: C.faint }} axisLine={{ stroke: C.rule }} tickLine={false} />
          <YAxis yAxisId="l" tick={{ fontSize: 10, fill: C.faint }} axisLine={false} tickLine={false} />
          <YAxis yAxisId="r" orientation="right" tick={{ fontSize: 10, fill: C.carmine }}
                 axisLine={false} tickLine={false} unit="%" />
          <Tooltip contentStyle={{ fontSize: 12, borderRadius: 2, border: `1px solid ${C.rule}` }} />
          <Legend wrapperStyle={{ fontSize: 11 }} />
          <Bar yAxisId="l" dataKey="n" name="Bepalingen" fill={C.lilac} barSize={26} />
          <Line yAxisId="r" type="monotone" dataKey="pos" name="Positiviteit %"
                stroke={C.pink} strokeWidth={2.2} dot={{ r: 3 }} />
        </ComposedChart>
      </ResponsiveContainer>
    </div>
  );
}

function Pyramid({ c }) {
  const max = Math.max(...c.demo.flatMap((d) => [d.m, d.v]), 1);
  return (
    <div>
      {c.demo.map((d) => (
        <div key={d.band} className="flex items-center" style={{ gap: 8, marginBottom: 7 }}>
          <div style={{ flex: 1, display: "flex", justifyContent: "flex-end", alignItems: "center", gap: 4 }}>
            <div style={{ width: `${(d.m / max) * 100}%`, height: 15, background: C.petrol }} />
          </div>
          <div style={{ width: 56, textAlign: "center", fontSize: 11, color: C.muted, fontVariantNumeric: "tabular-nums" }}>{d.band}</div>
          <div style={{ flex: 1, display: "flex", alignItems: "center", gap: 4 }}>
            <div style={{ width: `${(d.v / max) * 100}%`, height: 15, background: C.lilac }} />
          </div>
        </div>
      ))}
      <div className="flex justify-between" style={{ fontSize: 11, color: C.muted, marginTop: 4 }}>
        <span>← man</span><span>vrouw →</span>
      </div>
    </div>
  );
}

function Bars({ rows, unit }) {
  const max = Math.max(...rows.map((r) => r.n), 1);
  return (
    <div>
      {rows.map((r) => (
        <div key={r.k} className="flex items-center" style={{ gap: 10, marginBottom: 7 }}>
          <div style={{ width: 170, fontSize: 12, color: C.ink, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{r.k}</div>
          <div style={{ flex: 1, background: C.track, height: 14 }}>
            <div style={{ width: `${(r.n / max) * 100}%`, height: "100%", background: C.petrol }} />
          </div>
          <div style={{ width: 26, fontSize: 12, textAlign: "right", fontVariantNumeric: "tabular-nums" }}>{r.n}</div>
        </div>
      ))}
      {unit && <div style={{ fontSize: 11, color: C.faint, marginTop: 6 }}>{unit}</div>}
    </div>
  );
}

function Antibiogram({ a }) {
  const col = { R: C.pink, I: C.yellow, S: C.olive };
  return (
    <table style={{ fontSize: 11.5, borderCollapse: "collapse" }}>
      <thead>
        <tr>
          <th style={{ padding: "4px 10px 6px 0", textAlign: "left", color: C.muted, fontWeight: 500 }}>Isolaat</th>
          {a.abx.map((x) => <th key={x} style={{ padding: "4px 8px", color: C.muted, fontWeight: 500 }}>{x}</th>)}
        </tr>
      </thead>
      <tbody style={{ fontVariantNumeric: "tabular-nums" }}>
        {a.isolates.map((iso) => (
          <tr key={iso.id}>
            <td style={{ padding: "3px 10px 3px 0" }}>{iso.id}</td>
            {iso.p.map((v, i) => (
              <td key={i} style={{ padding: 2, textAlign: "center" }}>
                <span style={{
                  display: "inline-block", width: 26, height: 20, lineHeight: "20px",
                  background: col[v], color: v === "I" ? C.ink : "#fff", borderRadius: 2, fontSize: 11,
                }}>{v}</span>
              </td>
            ))}
          </tr>
        ))}
      </tbody>
    </table>
  );
}

/* ---------------- assessment ---------------- */

function Assessment({ c, onSave, onClose, draft }) {
  const [klass, setKlass] = useState(c.klass);
  const [text, setText] = useState("");
  useEffect(() => { setKlass(c.klass); setText(""); }, [c.id]);
  const state = derivedState(c);

  const notifiable = klass === "confirmed" || c.mo.includes("Legionella");

  return (
    <div style={{ padding: 16, borderTop: `1px solid ${C.rule}`, background: C.soft }}>
      <div style={{ fontSize: 10.5, letterSpacing: 0.6, textTransform: "uppercase", color: C.muted, marginBottom: 4 }}>
        Beoordeling
      </div>
      <p style={{ fontSize: 11.5, color: C.muted, marginBottom: 10, lineHeight: 1.45 }}>
        Verheft u dit cluster tot epidemie?
      </p>

      <div className="flex items-center" style={{
        gap: 8, marginBottom: 12, padding: "6px 9px", background: C.surface,
        border: `1px solid ${C.rule}`, borderRadius: 2,
      }}>
        <span style={{ width: 7, height: 7, borderRadius: "50%", background: STATES[state].colour }} />
        <span style={{ fontSize: 12 }}>{STATES[state].label}</span>
        <span style={{ fontSize: 10.5, color: C.faint, marginLeft: "auto" }}>afgeleid</span>
      </div>

      <div style={{ marginBottom: 10 }}>
        {CLASSES.map((v, i) => (
          <button key={v.key} onClick={() => setKlass(v.key)} style={{
            display: "block", width: "100%", textAlign: "left", marginBottom: 3,
            fontSize: 12, padding: "6px 9px", borderRadius: 2, cursor: "pointer",
            border: `1px solid ${klass === v.key ? v.colour : C.rule}`,
            background: klass === v.key ? v.colour : C.surface,
            color: klass === v.key ? "#fff" : C.ink,
          }}>
            <span style={{ opacity: 0.55, marginRight: 7, fontVariantNumeric: "tabular-nums" }}>{i + 1}</span>
            {v.label}
            <div style={{ fontSize: 10.5, opacity: 0.75, marginTop: 1, marginLeft: 19 }}>{v.hint}</div>
          </button>
        ))}
      </div>

      <textarea value={text} onChange={(e) => setText(e.target.value)}
        placeholder="Onderbouwing (verplicht)"
        style={{
          width: "100%", minHeight: 90, fontSize: 12.5, padding: 8, resize: "vertical",
          border: `1px solid ${C.rule}`, borderRadius: 2, background: C.surface,
          fontFamily: "inherit", color: C.ink, lineHeight: 1.5,
        }} />

      <button onClick={() => setText(draft)} style={{
        marginTop: 6, fontSize: 11.5, padding: "5px 9px", width: "100%",
        border: `1px solid ${C.rule}`, background: C.surface, borderRadius: 2,
        cursor: "pointer", color: C.petrol,
      }} className="flex items-center justify-center">
        <Wand2 size={12} style={{ marginRight: 6 }} /> Duiding overnemen als concept
      </button>

      {notifiable && (
        <div style={{ marginTop: 10, padding: 10, background: C.pinkTint, border: `1px solid ${C.carmine}33`, borderRadius: 2 }}>
          <div style={{ fontSize: 11.5, color: C.carmine, marginBottom: 6, fontWeight: 500 }}>Meldingsplicht Wpg</div>
          <label className="flex items-center" style={{ gap: 7, fontSize: 12, cursor: "pointer" }}>
            <input type="checkbox" /> GGD geïnformeerd
          </label>
        </div>
      )}

      {klass && !TERMINAL.includes(klass) && (
        <div style={{
          marginTop: 10, padding: 10, borderRadius: 2,
          background: state === "closable" ? C.geel5 : C.surface,
          border: `1px solid ${state === "closable" ? C.geel3 : C.rule}`,
        }}>
          <div style={{ fontSize: 11.5, marginBottom: 7, lineHeight: 1.5, color: state === "closable" ? C.ink : C.muted }}>
            {state === "closable"
              ? `${c.closureRule} bereikt. Afsluiten blijft uw beslissing.`
              : `Afsluitcriterium: ${c.closureRule}. Nog niet bereikt, afsluiten kan altijd.`}
          </div>
          <button onClick={() => onClose()} style={{
            fontSize: 12, padding: "6px 11px", width: "100%", borderRadius: 2, cursor: "pointer",
            border: state === "closable" ? "none" : `1px solid ${C.rule}`,
            background: state === "closable" ? C.bruin0 : C.surface,
            color: state === "closable" ? "#fff" : C.ink,
          }}>Cluster afsluiten</button>
        </div>
      )}

      <label className="flex items-center" style={{ gap: 8, marginTop: 10, fontSize: 12, color: C.muted }}>
        Herbeoordelen op
        <input type="date" defaultValue="2026-09-01" style={{
          fontSize: 12, padding: "4px 6px", border: `1px solid ${C.rule}`,
          borderRadius: 2, fontFamily: "inherit", color: C.ink, flex: 1,
        }} />
      </label>

      <div className="flex" style={{ gap: 6, marginTop: 12 }}>
        <button style={{
          flex: 1, fontSize: 12, padding: "7px 10px", border: `1px solid ${C.rule}`,
          background: C.surface, borderRadius: 2, cursor: "pointer", color: C.ink,
        }}>Uitstellen</button>
        <button onClick={() => onSave(klass, text)} disabled={!klass || !text.trim()}
          style={{
            flex: 2, fontSize: 12, padding: "7px 10px", border: "none", borderRadius: 2,
            background: (!klass || !text.trim()) ? "#BFCBD2" : C.petrol, color: "#fff",
            cursor: (!klass || !text.trim()) ? "not-allowed" : "pointer",
          }}>Vastleggen</button>
      </div>
    </div>
  );
}


function Archive({ onOpen }) {
  const [f, setF] = useState("all");
  const [q, setQ] = useState("");
  const rows = ARCHIVE.filter((r) => (f === "all" || r.klass === f) &&
    (q === "" || (r.mo + " " + r.place).toLowerCase().includes(q.toLowerCase())));
  return (
    <div style={{ flex: 1, overflowY: "auto", padding: "18px 24px 40px" }}>
      <h1 style={{ fontSize: 22, fontWeight: 600, marginBottom: 4 }}>Archief</h1>
      <p style={{ fontSize: 12.5, color: C.muted, marginBottom: 16 }}>
        Alle afgesloten clusters. De beoordeling van vorig seizoen is de beste voorkennis voor het cluster van dit seizoen.
      </p>
      <input value={q} onChange={(e) => setQ(e.target.value)}
        placeholder="Zoek op verwekker of plaats, bijvoorbeeld Klebsiella"
        style={{
          width: "100%", maxWidth: 460, fontSize: 13, padding: "7px 10px", marginBottom: 12,
          border: `1px solid ${C.rule}`, borderRadius: 2, fontFamily: "inherit", color: C.ink,
        }} />
      <div className="flex" style={{ gap: 4, marginBottom: 14, flexWrap: "wrap" }}>
        {[{ key: "all", label: "Alles" }, ...CLASSES].map((k) => (
          <button key={k.key} onClick={() => setF(k.key)} style={{
            fontSize: 11.5, padding: "4px 9px", borderRadius: 2, cursor: "pointer",
            border: `1px solid ${f === k.key ? C.blauw : C.rule}`,
            background: f === k.key ? C.blauw : C.surface, color: f === k.key ? "#fff" : C.ink,
          }}>{k.label}</button>
        ))}
      </div>
      <div style={{ background: C.surface, border: `1px solid ${C.rule}`, borderRadius: 2 }}>
        <table style={{ width: "100%", fontSize: 12.5, borderCollapse: "collapse" }}>
          <thead>
            <tr style={{ textAlign: "left", color: C.muted }}>
              {["Cluster", "Niveau", "Plaats", "Gevallen", "Classificatie", "Geopend", "Afgesloten", "Duur", "Beoordelaar"].map((h) => (
                <th key={h} style={{ padding: "10px 12px", fontWeight: 500, fontSize: 11, borderBottom: `1px solid ${C.rule}` }}>{h}</th>
              ))}
            </tr>
          </thead>
          <tbody style={{ fontVariantNumeric: "tabular-nums" }}>
            {rows.map((r) => {
              const k = CLASSES.find((x) => x.key === r.klass);
              return (
                <tr key={r.id} onClick={() => onOpen(r.id)} style={{ cursor: "pointer" }}>
                  <td style={{ padding: "9px 12px", borderBottom: `1px solid ${C.rule}`, fontStyle: r.italic ? "italic" : "normal" }}>{r.mo}</td>
                  <td style={{ padding: "9px 12px", borderBottom: `1px solid ${C.rule}`, color: C.muted }}>{LEVELS[r.level]}</td>
                  <td style={{ padding: "9px 12px", borderBottom: `1px solid ${C.rule}` }}>{r.place}</td>
                  <td style={{ padding: "9px 12px", borderBottom: `1px solid ${C.rule}` }}>{r.n}</td>
                  <td style={{ padding: "9px 12px", borderBottom: `1px solid ${C.rule}` }}><Chip filled colour={k.colour}>{k.label}</Chip></td>
                  <td style={{ padding: "9px 12px", borderBottom: `1px solid ${C.rule}` }}>{r.opened}</td>
                  <td style={{ padding: "9px 12px", borderBottom: `1px solid ${C.rule}` }}>{r.closed}</td>
                  <td style={{ padding: "9px 12px", borderBottom: `1px solid ${C.rule}` }}>{r.dur} d</td>
                  <td style={{ padding: "9px 12px", borderBottom: `1px solid ${C.rule}` }}>{r.by}</td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
    </div>
  );
}

function ActivityLog() {
  return (
    <div style={{ flex: 1, overflowY: "auto", padding: "18px 24px 40px" }}>
      <h1 style={{ fontSize: 22, fontWeight: 600, marginBottom: 4 }}>Activiteit</h1>
      <p style={{ fontSize: 12.5, color: C.muted, marginBottom: 18 }}>
        Volledig, append-only logboek. Wie heeft wat wanneer gedaan, inclusief de runs van het systeem zelf.
      </p>
      {ACTIVITY.map((d) => (
        <div key={d.day} style={{ marginBottom: 22 }}>
          <div style={{ fontSize: 11, letterSpacing: 0.6, textTransform: "uppercase", color: C.muted, marginBottom: 8 }}>{d.day}</div>
          <div style={{ background: C.surface, border: `1px solid ${C.rule}`, borderRadius: 2 }}>
            {d.items.map((it, i) => (
              <div key={i} className="flex" style={{
                gap: 14, padding: "11px 14px",
                borderBottom: i < d.items.length - 1 ? `1px solid ${C.rule}` : "none",
              }}>
                <div style={{ width: 44, fontSize: 12, color: C.muted, fontVariantNumeric: "tabular-nums" }}>{it.t}</div>
                <div style={{ width: 110, fontSize: 12.5, color: it.who === "systeem" ? C.muted : C.ink,
                              fontStyle: it.who === "systeem" ? "italic" : "normal" }}>{it.who}</div>
                <div style={{ flex: 1 }}>
                  <div style={{ fontSize: 12.5 }}>
                    {it.act} <span style={{ color: C.muted }}>·</span> <span style={{ color: C.blauw }}>{it.tgt}</span>
                  </div>
                  {it.det && <div style={{ fontSize: 11.5, color: C.muted, marginTop: 2 }}>{it.det}</div>}
                </div>
              </div>
            ))}
          </div>
        </div>
      ))}
    </div>
  );
}


const PERF = {
  detectors: [
    { d: "farrington",   n: 118, useful: 41, ppv: 35 },
    { d: "clusters",     n: 96,  useful: 38, ppv: 40 },
    { d: "same_place",   n: 34,  useful: 27, ppv: 79 },
    { d: "rare_trigger", n: 21,  useful: 18, ppv: 86 },
    { d: "mem",          n: 9,   useful: 8,  ppv: 89 },
  ],
  verdicts: [
    { k: "artefact", n: 38 }, { k: "variation", n: 74 }, { k: "no_epidemic", n: 41 },
    { k: "possible", n: 26 }, { k: "confirmed", n: 18 },
  ],
  timeliness: [
    { k: "Eerste geval tot detectie", med: 5.2, p90: 11.0 },
    { k: "Detectie tot eerste beoordeling", med: 0.9, p90: 3.1 },
    { k: "Detectie tot classificatie", med: 4.4, p90: 14.2 },
  ],
  monthly: 9.8,
};

function Performance() {
  const maxN = Math.max(...PERF.verdicts.map((v) => v.n));
  return (
    <div style={{ flex: 1, overflowY: "auto", padding: "18px 24px 40px" }}>
      <h1 style={{ fontSize: 22, fontWeight: 600, marginBottom: 4 }}>Prestatie</h1>
      <p style={{ fontSize: 12.5, color: C.muted, marginBottom: 18, maxWidth: 720, lineHeight: 1.6 }}>
        Berekend uit de vastgelegde beoordelingen over de laatste 24 maanden. Dit is de basis voor het
        bijstellen van de toelatingsdrempels, en tegelijk een publiceerbaar resultaat op zichzelf.
      </p>

      <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 16 }}>
        {[["Clusters per maand", PERF.monthly, "streefwaarde 10"],
          ["Beoordeeld", PERF.verdicts.reduce((a, b) => a + b.n, 0), "laatste 24 maanden"],
          ["Verheven tot epidemie", 18, "bevestigd of lopend"]].map(([l, v, sub]) => (
          <div key={l} style={{ background: C.surface, border: `1px solid ${C.rule}`, borderRadius: 2, padding: 16, minWidth: 190 }}>
            <Stat label={l} value={v} sub={sub} />
          </div>
        ))}
      </div>

      <Panel title="Positief voorspellende waarde per detector"
             note="Aandeel van de signalen dat leidde tot een classificatie van mogelijke epidemie of hoger. Een lage PVW is niet per se slecht: farrington is breed en vangt vroeg, same_place is smal en bijna altijd raak.">
        {PERF.detectors.map((d) => (
          <div key={d.d} className="flex items-center" style={{ gap: 12, marginBottom: 9 }}>
            <div style={{ width: 120, fontSize: 12.5 }}>{d.d}</div>
            <div style={{ width: 70, fontSize: 12, color: C.muted, fontVariantNumeric: "tabular-nums" }}>{d.n} sign.</div>
            <div style={{ flex: 1, background: C.track, height: 15 }}>
              <div style={{ width: `${d.ppv}%`, height: "100%", background: d.ppv >= 70 ? C.groen : C.blauw }} />
            </div>
            <div style={{ width: 42, fontSize: 12.5, textAlign: "right", fontVariantNumeric: "tabular-nums" }}>{d.ppv}%</div>
          </div>
        ))}
      </Panel>

      <Panel title="Verdeling van classificaties">
        {PERF.verdicts.map((v) => {
          const k = CLASSES.find((x) => x.key === v.k);
          return (
            <div key={v.k} className="flex items-center" style={{ gap: 12, marginBottom: 9 }}>
              <div style={{ width: 190, fontSize: 12.5 }}>{k.label}</div>
              <div style={{ flex: 1, background: C.track, height: 15 }}>
                <div style={{ width: `${(v.n / maxN) * 100}%`, height: "100%", background: k.colour }} />
              </div>
              <div style={{ width: 34, fontSize: 12.5, textAlign: "right", fontVariantNumeric: "tabular-nums" }}>{v.n}</div>
            </div>
          );
        })}
      </Panel>

      <Panel title="Tijdigheid" aside="dagen"
             note="De vertraging tussen eerste geval en detectie wordt begrensd door de rapportagevertraging op afnamedatum, niet door het algoritme.">
        <table style={{ width: "100%", fontSize: 12.5, borderCollapse: "collapse" }}>
          <thead>
            <tr style={{ textAlign: "left", color: C.muted }}>
              {["Interval", "Mediaan", "P90"].map((h) => (
                <th key={h} style={{ padding: "6px 10px 8px 0", fontWeight: 500, fontSize: 11, borderBottom: `1px solid ${C.rule}` }}>{h}</th>
              ))}
            </tr>
          </thead>
          <tbody style={{ fontVariantNumeric: "tabular-nums" }}>
            {PERF.timeliness.map((t) => (
              <tr key={t.k}>
                <td style={{ padding: "8px 10px 8px 0", borderBottom: `1px solid ${C.rule}` }}>{t.k}</td>
                <td style={{ padding: "8px 10px 8px 0", borderBottom: `1px solid ${C.rule}` }}>{t.med}</td>
                <td style={{ padding: "8px 10px 8px 0", borderBottom: `1px solid ${C.rule}` }}>{t.p90}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </Panel>
    </div>
  );
}

/* ---------------- app ---------------- */

export default function EpiSODE() {
  const [clusters, setClusters] = useState(CLUSTERS);
  const [selId, setSelId] = useState(CLUSTERS[0].id);
  const [toast, setToast] = useState(null);
  const [view, setView] = useState("clusters");
  const open = clusters.filter((x) => derivedState(x) !== "closed");
  const c = clusters.find((x) => x.id === selId) || open[0] || clusters[0];

  useEffect(() => {
    const el = document.createElement("link");
    el.rel = "stylesheet";
    el.href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&display=swap";
    document.head.appendChild(el);
    return () => { document.head.removeChild(el); };
  }, []);

  const closeCluster = () => {
    setClusters((p) => p.map((x) => x.id === c.id ? {
      ...x, closedByHuman: true,
      traject: [...x.traject, { s: "closed", from: "18-08", d: 0 }],
      timeline: [...x.timeline, { who: "Matthijs Berends", when: "18-08 09:44", klass: x.klass, closed: true,
                                  text: "Cluster afgesloten." }],
    } : x));
    const rest = clusters.filter((x) => x.id !== c.id && !x.closedByHuman);
    if (rest.length) setSelId(rest[0].id);
    setToast("Cluster afgesloten en verplaatst naar het archief");
    setTimeout(() => setToast(null), 2600);
  };

  const save = (klass, text) => {
    setClusters((p) => p.map((x) => x.id === c.id ? {
      ...x, klass, changed: false,
      traject: [...x.traject, { s: "monitoring", from: "18-08", d: 0 }],
      timeline: [...x.timeline, { who: "Matthijs Berends", when: "18-08 09:41", klass, text }],
    } : x));
    setToast("Beoordeling vastgelegd");
    setTimeout(() => setToast(null), 2200);
  };

  const font = '"IBM Plex Sans", ui-sans-serif, system-ui, "Segoe UI", sans-serif';
  const klassObj = c.klass ? CLASSES.find((v) => v.key === c.klass) : null;

  return (
    <div style={{ fontFamily: font, background: C.paper, color: C.ink, minHeight: "100vh", fontSize: 13 }}>
      <div style={{ background: C.petrolD, color: "#fff", padding: "0 16px", height: 40 }} className="flex items-center justify-between">
        <div className="flex items-center" style={{ gap: 18 }}>
          <span style={{ fontWeight: 600, letterSpacing: 0.4, fontSize: 15 }}>Epi<span style={{ color: C.lilac }}>SODE</span></span>
          <div className="flex" style={{ gap: 2 }}>
            {[["clusters", "Clusters"], ["archive", "Archief"], ["activity", "Activiteit"], ["perf", "Prestatie"]].map(([k, label]) => (
              <button key={k} onClick={() => setView(k)} style={{
                fontSize: 12, padding: "4px 10px", border: "none", borderRadius: 2, cursor: "pointer",
                background: view === k ? "rgba(255,255,255,0.16)" : "transparent",
                color: view === k ? "#fff" : "#BDD0DA",
              }}>{label}</button>
            ))}
          </div>
          <span style={{ fontSize: 10.5, color: "#8FAAB8", letterSpacing: 0.5, textTransform: "uppercase" }}>Demodata</span>
        </div>
        <div className="flex items-center" style={{ gap: 18, fontSize: 11.5, color: "#BDD0DA" }}>
          <span className="flex items-center" style={{ gap: 6 }}>
            <span style={{ width: 7, height: 7, borderRadius: "50%", background: C.yellow }} />
            Run 04:12 geslaagd · 1.284 streams · 3 clusters gedetecteerd
          </span>
          <span className="flex items-center" style={{ gap: 6 }}><Clock size={12} /> Volledigheid 91% (9 dagen)</span>
          <span>MB</span>
        </div>
      </div>

      <div className="flex" style={{ height: 4 }}>
        {BRAND_BAR.map((k) => <div key={k} style={{ flex: 1, background: k }} />)}
      </div>

      {view === "clusters" && (
      <div className="flex" style={{ height: "calc(100vh - 44px)" }}>
        {/* rail */}
        <div style={{ width: 250, background: C.surface, borderRight: `1px solid ${C.rule}`, overflowY: "auto", flexShrink: 0 }}>
          <div style={{ padding: "12px 14px", borderBottom: `1px solid ${C.rule}` }}>
            <div style={{ fontSize: 10.5, letterSpacing: 0.6, textTransform: "uppercase", color: C.muted }}>
              Openstaande clusters
            </div>
            <div className="flex items-baseline justify-between" style={{ marginTop: 3 }}>
              <span style={{ fontSize: 11.5, color: C.muted, fontVariantNumeric: "tabular-nums" }}>
                {open.length} nieuw of actief
              </span>
              <button onClick={() => setView("archive")} style={{
                fontSize: 11.5, border: "none", background: "transparent",
                color: C.blauw, cursor: "pointer", padding: 0,
              }}>Archief →</button>
            </div>
          </div>
          {open.map((x) => {
            const active = x.id === selId;
            const k = x.klass ? CLASSES.find((v) => v.key === x.klass) : null;
            return (
              <button key={x.id} onClick={() => setSelId(x.id)} style={{
                display: "block", width: "100%", textAlign: "left", padding: "11px 14px",
                border: "none", cursor: "pointer", background: active ? C.tint : "transparent",
                borderLeft: `3px solid ${active ? C.petrol : "transparent"}`,
                borderBottom: `1px solid ${C.rule}`,
              }}>
                <div className="flex items-baseline justify-between" style={{ gap: 6 }}>
                  <span style={{ fontSize: 13, fontWeight: 500, fontStyle: x.italic ? "italic" : "normal" }}>{x.mo}</span>
                  {x.changed && <AlertTriangle size={12} color={C.yellowD} />}
                </div>
                <div style={{ fontSize: 11, color: C.muted, marginTop: 3 }}>{LEVELS[x.level]}</div>
                <div style={{ fontSize: 11, color: C.muted, marginTop: 1, fontVariantNumeric: "tabular-nums" }}>
                  {x.obs} gevallen · ratio {x.ratio}
                </div>
                <div className="flex items-center" style={{ gap: 6, marginTop: 6, flexWrap: "wrap" }}>
                  <span className="flex items-center" style={{ gap: 5, fontSize: 11, color: C.muted }}>
                    <span style={{ width: 6, height: 6, borderRadius: "50%", background: STATES[derivedState(x)].colour }} />
                    {STATES[derivedState(x)].label}
                  </span>
                  {k && <Chip filled colour={k.colour}>{k.label}</Chip>}
                </div>
              </button>
            );
          })}
        </div>

        {/* dossier */}
        <div style={{ flex: 1, overflowY: "auto", padding: "18px 20px 40px" }}>
          <div className="flex items-center" style={{ gap: 10, flexWrap: "wrap" }}>
            <h1 style={{ fontSize: 25, fontWeight: 600, lineHeight: 1.15 }}><Mo c={c} /></h1>
            <Chip colour={C.petrol}>{LEVELS[c.level]}</Chip>
            <Chip colour={STATES[derivedState(c)].colour}>{STATES[derivedState(c)].label}</Chip>
            {klassObj && <Chip filled colour={klassObj.colour}>{klassObj.label}</Chip>}
            {c.changed && <Chip colour={C.yellowD}>Gewijzigd na beoordeling</Chip>}
          </div>
          <div className="flex items-center" style={{ gap: 8, marginTop: 6, fontSize: 12.5, color: C.muted, flexWrap: "wrap" }}>
            {c.careLine === "second" ? <Building2 size={13} /> : <MapPin size={13} />}
            <span>{c.place}</span><span style={{ color: C.faint }}>·</span>
            <span>cluster #{c.id}</span><span style={{ color: C.faint }}>·</span>
            <span>eerste geval {c.first}, laatste {c.last}</span><span style={{ color: C.faint }}>·</span>
            <span>gedetecteerd door {c.detectors.join(" en ")}</span>
          </div>

          <div style={{
            background: C.surface, border: `1px solid ${C.rule}`, borderRadius: 2,
            padding: 18, margin: "16px 0", display: "flex", gap: 40, flexWrap: "wrap",
          }}>
            <Stat label="Waargenomen" value={c.obs} sub={`verwacht ${c.exp}`} colour={C.carmine} />
            <Stat label="Ratio O/V" value={c.ratio} sub="waargenomen / verwacht" />
            {c.density && <Stat label="Incidentiedichtheid" value={c.density.value}
                                 sub={`basislijn ${c.density.baseline} per 1.000 verpleegdagen`} />}
            {c.doubling && <Stat label="Verdubbelingstijd" value={`${c.doubling} d`} sub="laatste 14 dagen" />}
            <Stat label="Unieke patiënten" value={c.unique.patients} sub={`${c.unique.isolates} isolaten, 1 per episode`} />
            <Stat label="Casusvrij" value={`${c.caseFree.since} / ${c.caseFree.need} d`} sub={c.caseFree.rule} />
            <Stat label="Prioriteit" value={c.priority} sub="samengestelde score" />
          </div>

          <section style={{
            background: C.surface, border: `1px solid ${C.rule}`, borderRadius: 2,
            padding: "14px 18px", marginBottom: 16,
          }}>
            <div style={{ fontSize: 10.5, letterSpacing: 0.6, textTransform: "uppercase", color: C.muted, marginBottom: 10 }}>
              Statusverloop
            </div>
            <div className="flex" style={{ gap: 3 }}>
              {[...c.traject, { s: derivedState(c), from: "nu", d: 0, now: true }].map((t, i, arr) => (
                <div key={i} style={{ flex: t.now ? 1.2 : Math.max(t.d, 1) }}>
                  <div style={{ height: 7, background: STATES[t.s].colour, opacity: t.now ? 1 : 0.45, borderRadius: 1 }} />
                  <div style={{ fontSize: 11, marginTop: 5, color: t.now ? C.ink : C.muted, fontWeight: t.now ? 500 : 400 }}>
                    {STATES[t.s].label}
                  </div>
                  <div style={{ fontSize: 10.5, color: C.faint, fontVariantNumeric: "tabular-nums" }}>
                    {t.now ? "nu" : `${t.from} · ${t.d} d`}
                  </div>
                </div>
              ))}
            </div>
          </section>

          {/* duiding */}
          <Panel title="Duiding" aside="automatisch opgesteld, te bewerken">
            {c.duiding.map((p, i) => (
              <p key={i} style={{ fontSize: 13.5, lineHeight: 1.65, marginBottom: 10, color: C.ink }}>{p}</p>
            ))}
            <div className="flex items-start" style={{
              gap: 9, marginTop: 14, padding: 12, background: C.soft,
              borderLeft: `3px solid ${C.petrol}`, borderRadius: 2,
            }}>
              <Info size={15} color={C.petrol} style={{ flexShrink: 0, marginTop: 2 }} />
              <p style={{ fontSize: 13, lineHeight: 1.6 }}>{c.advies}</p>
            </div>
          </Panel>

          <Panel title="Epidemische curve" aside={c.shape}
                 note="De gearceerde zone beslaat de laatste negen dagen: de empirische rapportagevertraging uit de rapportagedriehoek. Gevallen met een afnamedatum in dit venster zijn nog niet volledig binnen. Een dalende staart is hier geen afname.">
            <DailyCurve c={c} />
          </Panel>

          <Panel title="Meerjarige trend" aside="156 weken"
                 note="Waargenomen aantallen tegen de verwachting en de signaalgrens uit farringtonFlexible. Bedoeld om te zien of de huidige toename binnen het normale seizoenspatroon valt.">
            <LongTrend c={c} />
          </Panel>

          <Panel title="Noemer en positiviteit" aside="per week"
                 note="De meest voorkomende oorzaak van een vals signaal is een stijgend testvolume, niet een stijgende incidentie. Als de staven stijgen maar de lijn vlak blijft, is de toename een noemer-effect.">
            <Denominator c={c} />
          </Panel>

          <div className="flex" style={{ gap: 16, alignItems: "flex-start" }}>
            <div style={{ flex: 1 }}>
              <Panel title="Leeftijd en geslacht"
                     note="Een verschuiving in de getroffen leeftijdsgroep ten opzichte van de historische basislijn is vaak informatiever dan het aantal zelf.">
                <Pyramid c={c} />
              </Panel>
            </div>
            <div style={{ flex: 1 }}>
              <Panel title="Geografische spreiding" aside="PC4">
                <Bars rows={c.geo} unit="In de app staat hier de PC4-choropleth via certegis." />
              </Panel>
            </div>
          </div>

          <Panel title={c.careLine === "second" ? "Afdeling" : "Instelling"}>
            <Bars rows={c.places} />
          </Panel>

          {c.abx && (
            <Panel title="Fenotypisch resistentieprofiel"
                   note="Geen vervanging voor typering, maar een gedeeld profiel maakt een gemeenschappelijke bron plausibeler en kan aanleiding zijn om isolaten in te sturen voor WGS.">
              <Antibiogram a={c.abx} />
            </Panel>
          )}

          <Panel title="Vergelijkbare eerdere clusters" aside="uit het archief"
                 note="Overeenkomst op verwekker, niveau, omvang, seizoen en duur. Wat u eerder besloot bij een vergelijkbaar cluster is de beste beschikbare voorkennis.">
            {c.analogues.map((a) => {
              const k = CLASSES.find((x) => x.key === a.klass);
              return (
                <div key={a.id} className="flex items-center" style={{
                  gap: 12, padding: "9px 0", borderBottom: `1px solid ${C.rule}`,
                }}>
                  <div style={{ width: 46, fontSize: 11.5, color: C.muted, fontVariantNumeric: "tabular-nums" }}>#{a.id}</div>
                  <div style={{ flex: 1, fontSize: 12.5 }}>
                    <span style={{ fontStyle: "italic" }}>{a.mo}</span>
                    <span style={{ color: C.muted }}> · {a.place} · {a.when}</span>
                  </div>
                  <div style={{ width: 120 }}><Chip filled colour={k.colour}>{k.label}</Chip></div>
                  <div style={{ width: 74 }}>
                    <div style={{ background: C.track, height: 6 }}>
                      <div style={{ width: `${a.sim * 100}%`, height: "100%", background: C.blauw }} />
                    </div>
                  </div>
                  <div style={{ width: 30, fontSize: 11.5, textAlign: "right", color: C.muted, fontVariantNumeric: "tabular-nums" }}>
                    {Math.round(a.sim * 100)}%
                  </div>
                </div>
              );
            })}
          </Panel>

          <Panel title="Lijnlijst" aside={`${c.cases.length} van ${c.obs} gevallen`}>
            <table style={{ width: "100%", fontSize: 12.5, borderCollapse: "collapse" }}>
              <thead>
                <tr style={{ textAlign: "left", color: C.muted }}>
                  {["Casus", "Afname", "Geslacht", "Leeftijd", "PC4", "Afdeling", "Materiaal"].map((h) => (
                    <th key={h} style={{ padding: "5px 8px 8px 0", fontWeight: 500, fontSize: 11, borderBottom: `1px solid ${C.rule}` }}>{h}</th>
                  ))}
                </tr>
              </thead>
              <tbody style={{ fontVariantNumeric: "tabular-nums" }}>
                {c.cases.map((x) => (
                  <tr key={x.id}>
                    {[x.id, x.date, x.sex, x.age, x.pc4, x.ward || "—", x.mat].map((v, i) => (
                      <td key={i} style={{ padding: "6px 8px 6px 0", borderBottom: `1px solid ${C.rule}` }}>{v}</td>
                    ))}
                  </tr>
                ))}
              </tbody>
            </table>
          </Panel>

          <Panel title="Detectie-instellingen">
            <dl style={{ fontSize: 12.5 }}>
              {[
                ["Detectoren", c.detectors.join(", ")],
                ["Rt van toepassing", c.rtApplicable ? "ja" : "nee, geen mens-op-mens transmissie"],
                ["Aggregatie", "week"],
                ["Populatie-offset", c.density ? "verpleegdagen" : "geen"],
                ["case_free_days", "14"],
                ["Laatste run", "18-08-2026 04:12 · certe-avd-p-64"],
                ["Pakketversies", "certestats 2.4.1 · surveillance 1.24.1 · AMR 3.0.0"],
              ].map(([k, v]) => (
                <div key={k} className="flex" style={{ padding: "6px 0", borderBottom: `1px solid ${C.rule}`, gap: 16 }}>
                  <dt style={{ width: 180, color: C.muted, flexShrink: 0 }}>{k}</dt>
                  <dd>{v}</dd>
                </div>
              ))}
            </dl>
          </Panel>
        </div>

        {/* assessment rail */}
        <div style={{ width: 330, background: C.surface, borderLeft: `1px solid ${C.rule}`, display: "flex", flexDirection: "column", flexShrink: 0 }}>
          <div style={{ padding: 16, flex: 1, overflowY: "auto" }}>
            <div style={{ fontSize: 10.5, letterSpacing: 0.6, textTransform: "uppercase", color: C.muted, marginBottom: 10 }}>Verloop</div>
            {c.timeline.length === 0
              ? <p style={{ fontSize: 12.5, color: C.muted, lineHeight: 1.55 }}>
                  Nog niet beoordeeld. Gedetecteerd op {c.first} door {c.detectors.join(" en ")}.
                </p>
              : c.timeline.map((t, i) => {
                  const k = t.klass ? CLASSES.find((x) => x.key === t.klass) : null;
                  return (
                    <div key={i} style={{ marginBottom: 16, paddingLeft: 12, borderLeft: `2px solid ${k ? k.colour : C.rule}` }}>
                      <div style={{ fontSize: 12.5, lineHeight: 1.55, marginBottom: 4 }}>
                        Op <b>{longDate(t.when)}</b>{" "}
                        {t.closed
                          ? <>het cluster <b style={{ color: C.groen0 }}>afgesloten</b></>
                          : k
                          ? <>geduid als <b style={{ color: k.colour }}>{k.label}</b></>
                          : <>in beoordeling genomen</>}{" "}
                        door <b>{t.who}</b>.
                      </div>
                      <div style={{ fontSize: 12.5, lineHeight: 1.55, color: C.muted }}>{t.text}</div>
                      <div style={{ fontSize: 11, color: C.faint, marginTop: 3 }}>{t.when}</div>
                    </div>
                  );
                })}
            {c.klass && (
              <button style={{
                marginTop: 10, fontSize: 12, padding: "7px 12px", width: "100%",
                border: `1px solid ${C.rule}`, background: C.surface, borderRadius: 2, cursor: "pointer",
              }} className="flex items-center justify-center">
                <FileText size={13} style={{ marginRight: 6 }} /> Rapport voor medische staf
              </button>
            )}
          </div>
          <Assessment c={c} onSave={save} onClose={closeCluster} draft={c.duiding.join(" ")} />
        </div>
      </div>
      )}

      {view === "archive" && (
        <div className="flex" style={{ height: "calc(100vh - 44px)", background: C.paper }}>
          <Archive onOpen={() => { setView("clusters"); }} />
        </div>
      )}
      {view === "activity" && (
        <div className="flex" style={{ height: "calc(100vh - 44px)", background: C.paper }}>
          <ActivityLog />
        </div>
      )}
      {view === "perf" && (
        <div className="flex" style={{ height: "calc(100vh - 44px)", background: C.paper }}>
          <Performance />
        </div>
      )}

      {toast && (
        <div style={{
          position: "fixed", bottom: 20, left: "50%", transform: "translateX(-50%)",
          background: C.petrolD, color: "#fff", padding: "9px 16px", borderRadius: 2, fontSize: 12.5,
        }} className="flex items-center">
          <Check size={14} style={{ marginRight: 8 }} /> {toast}
        </div>
      )}
    </div>
  );
}
