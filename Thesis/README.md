# HIT Undergraduate Thesis Templates

This folder contains two template projects:

- `Template_thesis/`: undergraduate thesis template.
- `Template_slides/`: Beamer slide template for thesis presentations.

For opening reports and mid-term reports, it is recommended to refer to the official hithesis project: [hithesis/hithesis](https://github.com/hithesis/hithesis).

## Thesis Template

The thesis template entry file is:

```tex
Template_thesis/template_thesis.tex
```

Compile with:

```text
XeLaTeX -> BibTeX -> XeLaTeX -> XeLaTeX
```

On Overleaf, set the compiler to `XeLaTeX`. If references or the table of contents do not update after the first compilation, recompile several times.

Main files:

- `template_thesis.tex`: main entry file. Configure thesis metadata and include chapters here.
- `hitbook-en.cls`: class file. It controls page layout, fonts, headings, numbering, captions, citations, and cover format.
- `sections/`: main body chapters.
- `back/`: appendix and back-matter pages.
- `reference.bib`: bibliography database.
- `figures/`: figure assets.

Replace the placeholders near the top of `template_thesis.tex`:

```tex
\renewcommand{\thesistitle}{Template Thesis Title}
\renewcommand{\thesisauthor}{Author Name}
\renewcommand{\thesisstudentid}{Student ID}
\renewcommand{\thesisschool}{School Name}
\renewcommand{\thesismajor}{Major Name}
\renewcommand{\thesissupervisor}{Supervisor Name}
\renewcommand{\thesisdegree}{Bachelor Degree}
\renewcommand{\thesisdate}{Month Year}
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

The slide template entry file is:

```tex
Template_slides/template.tex
```

Compile with:

```text
XeLaTeX
```

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

## Upload Notes

When sharing the templates, upload the `Template_thesis/` and `Template_slides/` folders. Avoid uploading local build artifacts such as:

- `*.aux`
- `*.log`
- `*.out`
- `*.toc`
- `*.bbl`
- `*.blg`
- `*.fls`
- `*.fdb_latexmk`
- `*.synctex.gz`

These files are generated automatically during compilation.
