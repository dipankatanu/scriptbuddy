# scriptbuddy

**ScriptBuddy** makes any R script or analysis folder runnable on a new machine by:
- detecting required packages from code (`library()`, `pkg::fun()`, `pacman::p_load()`)
- following `source()` files recursively
- classifying dependencies as **CRAN**, **Bioconductor**, or **GitHub**
- optionally installing only what is missing
- exporting a portable dependency report (`deps.csv`, `cran.txt`, `bioc.txt`, `github.txt`)
- optionally generating a reproducible `renv.lock`

This is designed for **wet-lab and non-bioinformatics users** who just want the script to run.

---

## Installation (development)

```r
# install.packages("remotes")
remotes::install_github("dipankatanu/scriptbuddy")
