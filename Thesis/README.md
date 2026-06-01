# HIT Undergraduate Thesis Templates

This folder contains two template projects:

- `Template_thesis/`: undergraduate thesis template.
- `Template_slides/`: Beamer slide template for thesis presentations.

For opening reports and mid-term reports, it is recommended to refer to the hithesis project: [hithesis/hithesis](https://github.com/hithesis/hithesis).

## Thesis Template

The thesis template entry file is:

```tex
Template_thesis/template_thesis.tex
```

On Overleaf, set the compiler to `XeLaTeX`. If references or the table of contents do not update after the first compilation, recompile several times.

Main files:

- `template_thesis.tex`: main entry file. Configure thesis metadata and include chapters here.
- `hitbook-en.cls`: class file. It controls page layout, fonts, headings, numbering, captions, citations, and cover format.
- `sections/`: main body chapters.
- `back/`: appendix and back-matter pages.
- `reference.bib`: bibliography database.
- `figures/`: figure assets.
- For PDF files should be insected into to the corresponding place, which could be directly exported from the official Word template. Please disable the header and footer in the template.
  
```tex
    front/title.pdf
    back/my-achievements.pdf
    back/my-authorization.pdf
    back/my-acknowledgements.pdf
```

Recommended use:

- Add body chapters under `sections/`, then include them with `\include{sections/file-name}`.
- Add figures under `figures/`, and keep `\graphicspath{{figures/}}` in the main file.
- Add bibliography entries to `reference.bib`; do not change citation keys after they are used in the text.
- Use `\cite{key}` for superscript citations and `\inlinecite{key}` for inline citations.
- Use `\begin{abstractpage}...\keywords{...}\end{abstractpage}` for the abstract page.
- Use `\appendix` before appendix files. Appendix sections are numbered as `A.1`, `A.2`, etc.
- The cover is counted as page 1 but does not display a page number. The abstract starts from page 2.

The example files demonstrate equations, figures, tables, algorithm tables, theorem-like environments, references, appendices, acknowledgements, and the originality statement.

## Slides Template

Compile with `XeLaTeX`

Minimum files needed for Overleaf:

- `template.tex`
- `beamerthemehitspbu.sty`
- `hit-template-assets/`

Replace the metadata in `template.tex`:

```tex
\title[Short Presentation Title]{Full Presentation Title}
\author[Presenter]{Presenter Name}
\renewcommand{\hitsupervisor}{Supervisor Name}
\institute[Short Institute]{Department or School, University Name}
\date{Month Day, Year}
```

Recommended use:

- Use `\section{...}` to organize the presentation and generate the contents page.
- Use standard Beamer environments such as `frame`, `block`, `alertblock`, `columns`, equations, and tables.
- Put all image assets in `hit-template-assets/` or another clearly named folder.
- Use `\begin{frame}[noframenumbering]{...}` for backup or question slides that should not affect the total slide count.

