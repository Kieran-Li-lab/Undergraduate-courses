# Undergraduate Thesis Template

This folder is a clean thesis template. The main entry file is:

```text
thesis.tex
```

On Overleaf, set the compiler to `XeLaTeX`.

## File Structure

- `thesis.tex`: main file and complete calling example.
- `hitbook-en.cls`: thesis class file.
- `front/cover.tex`: abstract page example.
- `front/title.pdf`: full-page cover PDF placeholder.
- `body/`: body chapter examples.
- `back/`: conclusion, appendix, and full-page PDF placeholders.
- `figures/`: figure folder.
- `reference.bib`: bibliography examples.

## Main Workflow

The main file demonstrates the usual thesis order:

1. Full-page cover PDF, inserted by `\makeenglishcover`.
2. Abstract page, inserted by `\input{front/cover}`.
3. Table of contents.
4. Body chapters, inserted by `\include{body/...}`.
5. Conclusion.
6. References in GB/T 7714 numerical style.
7. Appendix.
8. Full-page PDF inserts for innovation achievements, originality/authorization, and acknowledgements.

## PDF Inserts

The template uses these default PDF paths:

```tex
\renewcommand{\coverpdf}{front/title.pdf}
\renewcommand{\innovativeachievementspdf}{back/my-achievements.pdf}
\renewcommand{\authorizationpdf}{back/my-authorization.pdf}
\renewcommand{\acknowledgementspdf}{back/my-acknowledgements.pdf}
```

The inserted PDF pages keep their table-of-contents entries and display the thesis page number. Replace the placeholder PDFs with your own finalized pages.

## Body Examples

The example body files include:

- ordinary paragraphs and heading levels;
- equations and equation references;
- figures and subfigures;
- three-line tables;
- algorithm tables;
- definitions, lemmas, theorems, proofs, and remarks;
- superscript citations with `\cite{...}`;
- inline citations with `\inlinecite{...}`.

## Bibliography

Bibliography entries are stored in `reference.bib`.

Use:

```tex
\cite{sample-journal}
\inlinecite{sample-book}
```

The bibliography style is:

```tex
\bibliographystyle{gbt7714-numerical}
```

## Notes

Do not upload local build artifacts such as `*.aux`, `*.log`, `*.toc`, `*.bbl`, `*.blg`, `*.out`, `*.fls`, or `*.fdb_latexmk`.
