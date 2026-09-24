# AI Document Generation — Results

**Subject:** Meta-language pipeline — an agent generates source in a
meta-language (LaTeX / Graphviz / Pandoc) and compiles it into slides,
documents, and graphics.

**Goal:** Test whether "LLM generates source → agent compiles" holds up for a
mixed deck of beamer slides + a generated diagram + a Creative-Commons figure.

**Status:** ✅ Explored and verified end-to-end.

---

## 1. Headline result

The loop holds. A 4-page beamer deck was compiled in a single `pdflatex` pass,
no errors, mixing two distinct kinds of graphics:

| Element | Role | Source | Outcome |
|---|---|---|---|
| Title + takeaway pages | prose | authored | ✅ compiles |
| Flow diagram (page 2) | agent-generated **code** | `flow.dot` → `dot -Tpdf` | ✅ compiles, all labels render |
| Figure (page 3) | agent-assembled **asset** | `person.pdf` (CC0, pinned) | ✅ compiles, embeds |

All four pages verified present via `pdftotext`. Both non-prose elements pinned
in VCS → reproducible.

## 2. Core decision — two classes of graphics

The "davinchi human" (hand-built DOT figure) came out poor, which split the
graphics problem into two classes, each with a different agent role:

1. **Diagrams = agent-generated code.** Graphviz DOT / TikZ. The agent writes
   source, compiles it. DOT rendered cleanly.
2. **Illustrations = agent-assembled assets.** People/figures/icons from
   Creative Commons libraries. The agent pins the file locally and embeds it.
   **Not** agent-drawn — hand-drawing a human in DOT is a bad fit.

## 3. Toolchain (verified on this host)

| Tool | Version / endpoint | Role |
|---|---|---|
| pdflatex | TeX Live 2023 | compile beamer/Pandoc output |
| Beamer | themes incl. Madrid | slide framework |
| Graphviz `dot` | 2.43 | DOT → PDF directly (`dot -Tpdf`) |
| inkscape | 1.2.2 | SVG → PDF/PNG |
| pdftoppm / pdftotext | poppler | inspection / verification |
| Local LLM | ollama @ `cyberkleiber:40114`, `ornith.gguf` (3B Q4) | source generation |

## 4. License findings (reproducibility-relevant)

- **CC0 (best — no attribution, no copyleft):** Open Peeps, Open Doodles
  (Pablo Stanley). Good for people/figures.
- **Permissive code licenses:** Material Icons (Apache-2.0), Heroicons/Feather
  (MIT).
- **unDraw:** free/no-attribution but *custom license* forbids redistribution and
  ML use — unusable if the agent remixes/redistributes. Best palette matching
  (live recolor-to-hex).
- **Avoid in a pipeline:** CC BY-SA (copyleft), CC BY (attribution required).
  Wikimedia Commons is mixed — check each file.

> **"free" ≠ "pipeline-safe".** Prefer CC0 / MIT / Apache and pin the downloaded
> file locally (never hotlink).

## 5. Graphics findings

- Graphviz has **no built-in human glyph** — a figure is ~15 nodes + 10 edges of
  manual coordinate work. Reliable only because each element is a basic DOT
  primitive; not "one keyword = a human".
- This host's `dot` is the mitigated build (`/usr/bin/dot` →
  `libgvc6-config-update`, CVE-2023-34359 mitigation). Undirected `--` is
  rejected; use `->` edges with `arrowhead=none`.
- LaTeX **cannot `\includegraphics` an SVG directly** — convert SVG→PDF/PNG
  first (inkscape). A real agent step and a conversion failure point for
  complex SVGs.
- CDN fetches (raw GitHub, Wikimedia, undraw `/api/illustrations`) were
  unreliable in the sandbox (404/error pages). Don't depend on live URLs.

## 6. Bugs found (illustrative of the compile loop)

**a) Unescaped `_` in `\texttt`.**

```
\texttt{FIGURE_LICENSES.md}   →   Missing $ inserted
```

In text/`\texttt` mode `_` is active and needs escaping:
`\texttt{FIGURE\_LICENSES.md}`. Exactly the kind of subtle error an
LLM-generated source can introduce into a compile loop.

**b) Raw font switch in a beamer frame body breaks `\ifinframe`.**

A raw `\itseries` (or bare `\bfseries`) used inline in a frame body throws
`Undefined control sequence` — beamer's `\ifinframe` scan does not resolve it.
Use `\textit{...}` / `\emph{...}` (grouped) instead. This surfaced while
building the 2-page handout below.

**c) Beamer page-height overflow clips the last steps.**

A 20 pt beamer page is only ~272 pt tall. A frame title bar + `\large`
enumerate + tip line overflows ~26 pt and silently clips the last items
(pdftotext stops mid-enumerate). Fix: drop the title bar (`\begin{frame}`
with no arg) and tighten `itemsep`. Verify with ink-coverage analysis
(pdftotext alone can't parse beamer enumerates past ~step 4).

## 7. Recommended pipeline

- **Diagrams:** author DOT yourself — reproducible, no license, palette control.
- **Rich figures:** pull CC0/CC-BY libraries once, commit the file into
  `assets/` with a license sidecar, convert SVG→PDF, embed. Skip unDraw if
  redistributing. CC-BY is usable if you render an attribution line.
- **Beamer handouts:** works for multi-page PDFs too. Use Noto Sans (`\sfdefault`),
  large base size, grouped `\textit`/`\emph` (not raw `\itseries`), and watch the
  ~272 pt page height — no title bar + tight `itemsep` so the last items don't clip.
- **Always:** escape `_` in `\texttt`; convert SVG before embedding; pin assets,
  not URLs; verify layout with ink coverage, not just pdftotext.

## 8. Artifacts preserved

```
discoveries/arm-bow-exercise/     # further validation: a 2-page PDF handout
  exercise.tex            # beamer source (Noto Sans, 20 pt)
  exercise.pdf            # compiled output, 2 pages (picture + 6 steps)
  arm.png                 # CC-BY 3.0 illustration (Wikimedia Commons, Osteomyoamare)
  README.md               # attribution + build notes
```

**Further validation (2-page PDF handout).** The same loop produced
`discoveries/arm-bow-exercise/` — a two-page rehab handout: page 1 a CC-BY
illustration with caption, page 2 six numbered steps + tip + attribution,
rendered cleanly in one `pdflatex` pass. This is the concrete reference answer
to the rehab handout's open image question (CC-BY with an attribution line is
viable) — see `discoveries/ai-rehab-handout.md`.
