# Happiness and Socioeconomic Structure Across 93 Countries

A multivariate statistical analysis of the relationship between national happiness and socioeconomic conditions across **93 countries**, combining exploratory visualization, **PCA, canonical correlation analysis (CCA), clustering, and multivariate regression**.

The project integrates indicators from the United Nations, World Bank, and World Happiness Report to study how governance, labor-market conditions, macroeconomic variables, urbanization, and population structure move together with happiness and human-development outcomes.

## Main findings

Across the exploratory, PCA, CCA, and regression analyses, the project identifies a consistent multivariate pattern:

- **government effectiveness, rule of law, tax revenue, and urban population share** are associated with higher happiness/development outcomes;
- **inflation, unemployment, GDP growth, and total population size** load in the opposite direction in the observed cross-country structure;
- the first principal component captures a large share of the common socioeconomic variation, and four PCs are retained for the reduced representation;
- CCA provides a second view of the joint relationship between the socioeconomic indicator block and the happiness/development block;
- multivariate regression is used to assess which associations remain prominent after modeling the indicators jointly.

## Variables

The analysis includes:

- GDP / GDP growth
- inflation
- unemployment
- tax revenue
- rule of law
- urban population
- control of corruption
- government effectiveness
- total population
- Human Development Index (HDI)
- Happiness Index (HI)

## Methods

### Exploratory data analysis

Country- and continent-level visualizations are used to inspect distributions, outliers, and pairwise patterns before multivariate modeling.

### Principal Component Analysis

PCA compresses the correlated socioeconomic variables into a smaller set of orthogonal components and reveals the dominant directions of cross-country variation.

### Canonical Correlation Analysis

CCA studies the strongest linear relationships between two multivariate blocks: socioeconomic indicators and happiness/development outcomes.

### Multivariate regression

Regression models quantify conditional associations between the outcome measures and socioeconomic predictors after accounting for the remaining variables in the model.

### Clustering

Country groupings are explored to identify socioeconomic profiles that emerge from the multivariate feature space.

## Repository structure

- `data.csv`, `data_2020.csv` — assembled country-level datasets
- `exploratory analysis.Rmd` — exploratory visualization and descriptive analysis
- `PCA.Rmd` — principal component analysis
- `CCA.Rmd`, `cca.R`, `cca 456.R` — canonical correlation analysis
- `Cluster.Rmd` — clustering analysis
- additional R/RMarkdown files — regression and supporting analysis

## Reproducibility

The analysis is implemented in R/RMarkdown. Core packages used across the notebooks include `dplyr`, `ggplot2`, `patchwork`, and standard multivariate-statistics functions in R.

The committed CSV files allow the main analyses to be reproduced without downloading external data first.

## Statistical interpretation

This is a cross-country observational study. The reported relationships are **multivariate associations**, not causal treatment effects. The value of the project is the consistency of the socioeconomic structure across several complementary statistical methods rather than a single isolated coefficient.

## Contributors

- Daniel Ye
- Yaling Hong
- Zekun Wang
