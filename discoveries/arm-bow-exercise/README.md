# Arm Bowing Exercise (elbow flexion/extension)

A two-page exercise card rendered from LaTeX (beamer) using the
meta-language pipeline.

- `exercise.tex` — source
- `exercise.pdf` — rendered output (2 pages)
- `arm.png` — illustration used

## Content

**Page 1 — picture:** the arm bending at the elbow, then straightening,
with a short caption.

**Page 2 — How to do it:** 6 numbered steps + a tip + attribution.

## Easy-to-read typography

- Sans-serif family (`\sfdefault`) via `fontspec`/Noto Sans.
- Large base size (20 pt), `\large` headings, `\normalsize` body.
- Generous item spacing; single-spaced steps for legibility.

## Illustration attribution

- Image: **(c) Osteomyoamare**, [Wikimedia Commons](https://commons.wikimedia.org/wiki/File:Flexion_Extension_Arm.png)
- License: **CC BY 3.0** (attribution required)
- Diagram: "Flexion/extension of the arm"

## Build

```bash
pdflatex exercise.tex
pdflatex exercise.tex   # second pass for page refs
```
