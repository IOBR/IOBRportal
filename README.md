# IOBRportal

<!-- badges: start -->
<!-- badges: end -->

IOBRportal is an R package and Shiny-based web platform for bulk transcriptome and immuno-oncology studies. It integrates signature score calculation, tumor microenvironment deconvolution, clustering, visualization, survival modeling, correlation analysis, mutation analysis, and cohort-oriented workflows into a unified analysis environment. More information is available in the [IOBRportal Book](https://qingcongl.github.io/IOBRportal_book/).

## Overview

IOBRportal is designed to connect data preparation, feature generation, statistical analysis, and interactive visualization in one package-oriented framework. It supports both module-level analyses and workflow-level pipelines, allowing users to move from raw expression matrices or curated cohort tables to interpretable results in a reproducible manner.

![](man/figures/IOBRportal.png)
![](man/figures/IOBRportal_overview.svg)

## Features

### Signature score calculation

- PCA-based signature scoring
- ssGSEA-based signature scoring
- z-score-based signature scoring
- multiple built-in signature collections

### Tumor microenvironment deconvolution

- CIBERSORT
- EPIC
- quanTIseq
- xCell
- ESTIMATE
- TIMER
- MCPcounter
- IPS
- integration mode for combined TME estimation

### Statistical analysis

- batch correlation
- partial correlation
- batch survival screening
- Wilcoxon test
- Kruskal-Wallis test
- time-dependent ROC analysis
- signature ROC analysis

### Visualization

- heatmap
- box plot
- percent bar plot
- cell bar plot
- forest plot
- correlation plot
- correlation matrix
- Kaplan-Meier survival plots

### Mutation analysis

- MAF-to-mutation-matrix conversion
- phenotype-associated mutation analysis
- oncoprint visualization
- mutation-associated boxplot visualization

### Workflow support

- integrated workflow for user-uploaded data
- TCGA cohorts workflow
- cancer cohorts workflow
- immunotherapy cohorts workflow
- other cohorts workflow
- mutation workflow
- signature-gene workflow

## Installation

You can install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("IOBR/IOBRportal")
```

Then load the package:

```r
library(IOBRportal)
```
For the interactive Shiny interface:

```r
run_shinyapp()
```
Some cohort-oriented modules in local deployments may require separately configured external data resources.

## What data can I use?

IOBRportal is designed for bulk transcriptomic and cohort-level analyses. Typical inputs include:

- raw count matrices
- TPM or other normalized expression matrices
- phenotype or clinical annotation tables
- signature score matrices
- TME deconvolution matrices
- mutation data in MAF-derived format

Depending on the selected module or workflow, IOBRportal can work with user-uploaded datasets and, when configured in deployed environments, database-backed cohort resources such as TCGA, curated cancer cohorts, immunotherapy cohorts, CPTAC, and TARGET.

## Main workflows

### 1. Integrated Workflow

This workflow is designed for user-uploaded data and supports:

- counts to TPM conversion
- outlier detection
- signature scoring or TME deconvolution
- clustering
- phenotype combination
- downstream visualization and statistical analysis

### 2. TCGA Cohorts Workflow

This workflow is designed for TCGA cohort analysis and supports:

- cohort selection
- signature or TME data preparation
- clustering
- visualization
- survival analysis
- correlation analysis
- group comparison

### 3. Cancer Cohorts Workflow

This workflow is designed for curated cancer cohort datasets and supports:

- data selection
- signature or TME preparation
- clustering
- visualization
- correlation analysis
- group comparison

### 4. Immunotherapy Cohorts Workflow

This workflow is designed for immunotherapy-focused datasets and supports:

- filtering by cancer type, treatment, drug, and timepoint
- signature or TME extraction
- clustering
- visualization
- correlation analysis
- group comparison

### 5. Other Cohorts Workflow

This workflow is designed for CPTAC/TARGET-style datasets and supports:

- cohort selection
- signature or TME data preparation
- clustering
- visualization
- survival analysis
- correlation analysis
- group comparison

### 6. Mutation Workflow

This workflow supports:

- mutation matrix construction from MAF files
- phenotype-associated mutation analysis
- oncoprint generation
- mutation-associated boxplot generation

### 7. Signature-Gene Workflow

This workflow supports:

- TPM preprocessing
- outlier removal
- signature calculation
- signature-gene correlation analysis
- correlation matrix construction

## Core analysis modules

IOBRportal provides reusable analysis modules covering:

- data preparation
- signature score calculation
- TME deconvolution
- statistical analysis
- visualization
- TME interaction analysis
- mutation analysis
- workflow orchestration

## Package structure

The package is organized around modular analysis components and workflow-level wrappers. Core modules can be used independently, while workflows connect multiple modules into a complete analysis pipeline.

Typical components include:

- preprocessing modules for expression normalization and outlier detection
- feature-generation modules for sigscore calculation and TME deconvolution
- downstream modules for visualization, survival analysis, correlation analysis, and group comparison
- mutation-focused modules for mutation matrix construction and phenotype-associated mutation discovery
- database-backed workflow modules for TCGA, cancer cohorts, immunotherapy cohorts, and other public resources

## Outputs

Typical outputs generated by IOBRportal include:

- processed expression matrices
- signature score matrices
- TME deconvolution tables
- cluster assignments
- combined phenotype-feature tables
- survival statistics
- correlation statistics
- group comparison results
- mutation result tables
- publication-ready figures

## Shiny application

In addition to package functions, IOBRportal provides a Shiny-based interactive interface for end-to-end analysis. The application supports both user-uploaded datasets and cohort-oriented resources in a unified workflow environment. In deployed full-data instances, database-backed cohort modules can be used for interactive analysis alongside upload-based workflows.

## Documentation

IOBRportal includes function-level documentation, workflow-level help pages, and interactive analysis interfaces. It is suitable for exploratory analysis, reproducible downstream reporting, and cohort-oriented immuno-oncology studies.

## Citation

If you use IOBRportal in your work, please cite:

- the IOBRportal package paper or preprint, when available
- the original methods implemented in the package, such as CIBERSORT, EPIC, xCell, ESTIMATE, TIMER, MCPcounter, quanTIseq, IPS, and related statistical tools

## License

Please add the package license here, for example:

`GPL-3`

or

`MIT + file LICENSE`