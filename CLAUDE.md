# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

quizR is an R package that creates randomized quizzes for Moodle. It allows users to define quiz questions with randomized data in R and export them to Moodle XML format, AMC LaTeX, or PDF correction sheets.

## Build and Development Commands

### Build the package
```bash
make
```
This runs the complete build pipeline: load_all, document, build, check, and install.

### Individual commands
```bash
# Generate documentation from roxygen comments
Rscript -e "library(devtools); document('.')"

# Build the package tarball
R CMD build .

# Check the package
R CMD check quizR_1.0.tar.gz

# Install the package
R CMD INSTALL quizR_1.0.tar.gz
```

### Run tests
```bash
# Run all tests
Rscript -e "library(testthat); test_dir('tests/testthat')"

# Run a single test file
Rscript -e "library(testthat); test_file('tests/testthat/test_question.R')"
```

## Architecture

The package is organized into three distinct layers:

### Layer 1 — Data (R6 classes)

R6 classes that store question structure and raw (unevaluated) data. They have no rendering logic.

- **SimpleQuestion** (`R/simple_question.R`) — abstract base class for all question types
  - Stores `text`, `data`, `hidden_data`, `seed`, `hidden_seed`, `feedback`, `answer`
  - `rename()` and friends prefix variable names to avoid collisions in composite questions
  - `hidden_data_names()` introspects which variables `hidden_data` produces

- **Question types** (inherit from SimpleQuestion):
  - **NumericalQuestion** (`R/numerical_question.R`) — numeric answer
  - **ShortanswerQuestion** (`R/shortanswer_question.R`) — short text answer
  - **MultipleChoice** (`R/multiple_choice.R`) — MC/MR with many subtypes (mc, mcv, mch, mr, mrs, …)
  - **ClozeQuestion** (`R/cloze_question.R`) — composite question embedding inline answers ("cookies")
  - **GapselectQuestion** (`R/gapselect_question.R`) — gap-fill question

- **Group** (`R/group.R`, inherits from SimpleQuestion) — hierarchical container; supports sequential or random child selection

- **Quiz** (`R/quiz.R`, inherits from Group) — top-level container; entry points are:
  - `generate_XML(...)` — Moodle XML
  - `generate_AMC(...)` — AMC LaTeX
  - `generate_correction(...)` — Markdown/PDF correction sheet
  - `generate_data_file(filename)` — R script reproducing the evaluated data

- **Question factory** (`R/question.R`) — `Question()` creates the right class from a `type` string via `Question_classes` lookup table

### Layer 2 — Instantiator (`R/instantiate.R`)

Walks the R6 node tree, evaluates `hidden_data` (with `hidden_seed`) and `data` (with `seed`), substitutes variables in all text/answer fields, and returns a **plain-list inode** (no R6, no unevaluated expressions). Inodes are the sole input to exporters.

Key functions:
- `instantiate(node, parent_var_list)` — entry point; dispatches on node class
- `instantiate_leaf()` — handles all simple question types
- `instantiate_cloze()` — handles ClozeQuestion (text + inline cookies)
- `instantiate_group()` — handles Group/Quiz; propagates variables down to children

Inodes carry a `class_name` field so exporters can dispatch on it without R6 inheritance.

### Layer 3 — Exporters

R6 classes with a uniform `render(inode, opts)` interface. Each exporter handles all node types via `switch(inode$class_name, ...)`.

- **XMLExporter** (`R/exporter_xml.R`) — renders to Moodle XML; uses whisker templates for question bodies; renders markdown to HTML via `render_HTML()` from utils.R
- **MarkdownExporter** (`R/exporter_markdown.R`) — renders to R Markdown correction sheet; `generate_correction()` knits it to PDF via rmarkdown
- **AMCExporter** (`R/exporter_amc.R`) — renders to AMC LaTeX; uses whisker templates; wraps questions in `\element{group}{...}` blocks

### File Organization (Collate Order)

The DESCRIPTION `Collate` field controls load order (dependencies must come first):
1. `utils.R` — shared helpers
2. `simple_question.R` — base class
3. `multiple_choice.R`, `shortanswer_question.R`, `numerical_question.R` — basic question types
4. `cloze_question.R`, `gapselect_question.R` — composite types
5. `group.R` — container
6. `question.R` — factory
7. `instantiate.R` — instantiator
8. `exporter_amc.R`, `exporter_xml.R`, `exporter_markdown.R` — exporters
9. `quiz.R` — top-level class (depends on all exporters)

## Key Utilities (`R/utils.R`)

- `merge_languages()` — combines R language objects (expressions)
- `instantiate_data_list()` — substitutes variables in expressions
- `instantiate_text_list()` — substitutes variables in text with inline R code
- `instantiate_object()` — dispatches to above based on type
- `render_HTML()` — renders a markdown string to HTML (used by XMLExporter)
- `render_latex_fragment()` — renders a markdown string to LaTeX (used by AMCExporter)
- `expr()` — helper to create quoted expressions
- `empty_env()` — creates a clean evaluation environment

## Development Notes

- The package uses roxygen2 for documentation (run `document()` after modifying `@export` or `@param` tags)
- Tests use testthat
- The `Collate` field in DESCRIPTION controls file load order — maintain it when adding new files
- **ClozeQuestion** can be built from text with inline cookies (`{1:NM:=1:0.001}` syntax) or from an explicit list of subquestions; the instantiator handles both paths
- Adding a new export format means: create `exporter_foo.R` with an R6 class, add a `generate_foo()` method to Quiz, and add the file to `Collate` before `quiz.R`
