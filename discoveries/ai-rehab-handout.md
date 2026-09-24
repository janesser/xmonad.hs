# Rehab handout as PDF — practitioner interview guide

Status: **interview pending — but a ready sample now exists.** This is the
research note for a PDF rehab handout (daily-practice instructions + a
low-vision-friendly schematic per exercise), and the interview script that
decides the one open question: **what generates the imagery, and how.**

A working sample is now available to show the practitioner during the
interview (see **Ready sample** below) — it pre-answers question 13:
a CC-BY illustration **with an attribution line is fine in practice.

Related: `discoveries/ai-image-gen.md` (ComfyUI/Flux — uninstalled, was
unreliable, effectively out of scope).

---

## The one thing this interview decides

How the image component of the handout is produced:

- **(A)** A curated library of authored schematics (Creative Commons or
  hand-made) — lowest risk, no compute.
- **(B)** On-the-fly generated schematics (e.g. LLM-emitted SVG line art) —
  flexible, but only if generation is reliable *and* safe for low vision.
- **(C)** Diffusion-generated images — currently the **disfavored** option
  (soft raster, ambiguous anatomy, GPU cost; poor fit for low-vision
  schematics).
- **(D)** No image — layout/typography only, if practitioners don't find
  them useful.

The questions below are designed so the answers *fall into* one of these —
preferably by making the practitioner look at real samples rather than
describe an ideal.

---

## A. Frame & role

1. When a patient takes a handout home, what is the job of the image? Is it
   to let them **reproduce the movement correctly**, to **remember/recognize**
   it later, or something else (feel professional, motivation, distraction)?
2. On a typical day, does the patient *look at the image while exercising*, or
   just glance at it once? How soon after discharge do they use it?
3. What happens today, without a handout image — do they call you, guess, or
   skip? That tells us how much the image needs to carry the instruction alone.

## B. The patient / low-vision constraint

4. Describe your typical patient for these exercises. What vision levels do we
   actually have to design for (mild blur, low contrast, central scotoma)?
5. What makes a picture *instantly graspable* to that patient? Crisp thick
   outlines? Big fill vs. thin line? Color coding? What has *failed* to read
   before?
6. Do patients zoom, magnify, or print these? If so, the image must scale
   without losing clarity — a hard constraint on raster vs. vector.
7. Any cognitive/reading load we should factor in? (Short labels, few colors,
   left-to-right simplicity?)

## C. Show, don't ask (do this live)

Prepare 2–3 physical/screenshot samples for **one** well-known exercise, e.g.:

- **S1** — a crisp black-and-white line schematic (thick lines, minimal detail)
- **S2** — a styled/filled icon (e.g. a CC-licensed pictogram)
- **S3** — a photorealistic / diffusion-style render

Ask:

8. Point to the one you'd trust a patient to exercise from *without* you there.
9. Point to the one your *worst-vision* patient could read. What's wrong with
   the others for them?
10. Which one "looks right but is actually misleading"? (Exposes the
    plausible-but-wrong risk of generated/realistic images.)

**Also show `discoveries/arm-bow-exercise/`** — a finished 2-page handout
(picture on p1, six numbered steps on p2, CC-BY credit line). It is the real,
low-vision-tuned version of S2 and lets the practitioner judge the actual
product rather than an abstract pictogram.

## D. Specificity & scale

11. Of the exercises you hand out, roughly how many map cleanly to an existing
    picture, vs. always needing a bespoke one? (Decides library vs. on-demand.)
12. If two exercises look similar (e.g. side-l raise vs. front raise), does the
    patient tell them apart? That's a question for the image to solve — or a
    signal that a label, not a picture, is the answer.

## E. Licensing & attribution

13. Would a "Graphics: CC-BY © …" line in the handout be acceptable to you and
    your patients, or does it undermine the product? (Resolves whether CC-BY /
    CC-BY-SA is usable, or only CC0.)

> Demonstrable answer: the arm-bow handout uses a CC-BY 3.0 Wikimedia
> illustration (Osteomyoamare) with a visible attribution line on p2 — so a
> `CC-BY © …` credit is a live option, not a hypothetical. If the practitioner
> still objects, fall back to CC0 (Open Peeps/Open Doodles).
14. Do you distribute these commercially, freely, or internally? (Resolves
    whether NC-licensed graphics are even allowed.)

## F. Process & ownership

15. Who would review a handout before it goes to a patient — you, or a team?
    How much do you trust AI-drafted text or images to pass your review as-is?
16. What would make you *stop* using this tool — one failure that breaks your
    trust?

---

## How to read the answers

- If they pick **S1 (crisp line)** and stress *readability + scaling* → lean
  **vector schematics** (B) or a **CC line-art library** (A).
- If they pick **S2 (styled icon)** and accept attribution → **CC library** (A)
  is the strongest fit.
- If they can't tell S1 from S3 as "safe", or flag "looks right but wrong" →
  **diffusion is out**; don't build image generation.
- If images feel optional once they describe the patient's real behavior →
  **layout-only** (D) may be the honest answer.

## Ready sample

`discoveries/arm-bow-exercise/` — a complete 2-page PDF handout for one
exercise (elbow flexion/extension):

- p1: CC-BY 3.0 illustration + caption
- p2: 6 numbered steps, a tip line, and a CC-BY attribution line
- Noto Sans, 20 pt base, generous spacing (low-vision-friendly)
- Builds in one `pdflatex` pass via the ai-docgen pipeline

Show this in the interview; it resolves question 13 and gives a concrete
S2-style sample to point at.

## Open decisions still pending

- Image source (A/B/C/D) — decided by the interview above. The arm-bow sample
  is option **(A) CC library (CC-BY)** working end-to-end.
- Content source: practitioner reviews; exercises are well-known/documented.
- Hardware: not a constraint for A/B/D (no GPU needed); only C (diffusion)
  would need cyberkleiber to have a usable GPU.
