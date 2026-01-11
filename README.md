# scriptbuddy

scriptbuddy makes any R script or analysis folder runnable on a new machine.

It scans R code, detects required packages, follows `source()` files recursively, and classifies dependencies as CRAN, Bioconductor, or GitHub. It can install only what is missing, export a dependency report, and generate a reproducible `renv.lock` file.

This tool is designed for wet-lab scientists, collaborators, and reviewers who need R scripts to run reliably without manual package troubleshooting.

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("dipankatanu/scriptbuddy")
```

---

## How scriptbuddy works

scriptbuddy detects packages used through:

- `library()` and `require()`
- `pkg::function()` and `pkg:::function()`
- `pacman::p_load()`
- `source()` files recursively
- GitHub installs via `remotes::install_github()` and `devtools::install_github()`

Dependencies are classified into:

- CRAN
- Bioconductor
- GitHub
- Unknown

---

## Quick start

Scan a script without installing anything:

```r
library(scriptbuddy)
scriptbuddy("analysis.R", install = FALSE)
```

Install missing packages from CRAN and Bioconductor:

```r
scriptbuddy("analysis.R", install = TRUE)
```

Only missing packages are installed. Already installed packages are skipped automatically.

---

## Copy and paste install commands

To generate explicit install commands for CRAN, Bioconductor, and GitHub:

```r
print_install_commands("analysis.R")
```

Example output:

```r
install.packages(c("dplyr", "ggplot2"))
BiocManager::install(c("limma", "edgeR"))
remotes::install_github(c("jokergoo/ComplexHeatmap"))
```

This is useful for sending installation instructions to collaborators.

---

## Export a dependency report

```r
write_deps_report("analysis.R", out_dir = "scriptbuddy_report")
```

This creates:

```
scriptbuddy_report/
├── deps.csv
├── cran.txt
├── bioc.txt
├── github.txt
└── unknown.txt
```

These files can be shared, archived, or attached to manuscripts.

---

## Create a reproducible environment

```r
scriptbuddy_lock("analysis.R", project_dir = ".")
```

This creates or updates a `renv.lock` file so the exact software environment can be reproduced on another machine.

---

## Why scriptbuddy exists

Most R scripts fail on new machines because:

- Required packages are missing  
- CRAN and Bioconductor repositories are confused  
- GitHub packages are forgotten  
- Dependencies are hidden inside `source()` files  

scriptbuddy solves these problems in one command.

---

## GitHub dependencies

GitHub packages are detected and reported, and install commands are printed for them. They are not installed automatically to avoid authentication problems, private repositories, and accidental overwrites.

---

## License

MIT License
