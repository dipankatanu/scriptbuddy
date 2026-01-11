# ScriptBuddy stress-test script

library(dplyr)
library(ggplot2)

# Rare CRAN package
library(ggpubr)

# Bioconductor packages (likely missing)
library(GenomicRanges)
library(AnnotationDbi)

# Namespace style
SummarizedExperiment::SummarizedExperiment()

# pacman loader
pacman::p_load(WGCNA, ComplexHeatmap)

# GitHub-only install
remotes::install_github('jokergoo/ComplexHeatmap')

# Another GitHub repo
devtools::install_github('satijalab/seurat-data')

# Fake analysis
df <- data.frame(a = rnorm(100), b = rnorm(100))
ggplot(df, aes(a, b)) + geom_point()

# Pull in another script
source('tmp/helper.R')
