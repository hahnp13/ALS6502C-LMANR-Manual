---
title: "Linear models in Agriculture and Natural Resources"
author: "Phil Hahn & Leo Ohyama"
date: "2025-01-12"
output:
  html_document2:
    fig_caption: yes
    df_print: paged
  pdf_document:
geometry: margin=5cm
documentclass: book
bibliography:
- book.bib
- packages.bib
description: "This book contains code associated with ALS6502C. \nThe HTML output
  format for this example is bookdown::gitbook,\nset in the _output.yml file.\n"
link-citations: yes
github-repo: rstudio/bookdown-demo
version: "1.0.0"
site: bookdown::bookdown_site
---

# Preface

This is a book written in **Markdown** for ALS6502C at the University of Florida, instructed by Dr. Phil Hahn ([hahnp\@ufl.edu](mailto:hahnp@ufl.edu){.email}). This is Version 1.0 and will be updated periodically.

This book is meant to be used in conjunction with the class lectures and is not meant to replace lectures. With that being said, this book is written to be semi-standalone and to provide students to review the material. Please visit to course canvas page for syllabus and additional materials.

## Learning outcomes:

By the end of the course, students will be able to:

-   Propose biological questions and formulate hypotheses to test them.

-   Construct statistical models that are commonly used in agricultural and natural resource studies, including linear models, generalized linear models, linear mixed models, and generalized linear mixed effects models, using freely available packages in R.

-   Analyze, visualize, interpret, and report the results of statistical models using formats acceptable for publication.

-   Critique results of analyses reported by peers and in the literature.

-   Select R documentation and use new R packages and functions.

## Overview of statistical analysis workflow

Below is the general workflow we will use for our analyses, starting with things that need to be done before data is collected (pre-analysis) and then for the analysis stage. In future versions of the book, this will be fleshed out into a chapter. Another great source is [Zuur and Ieno 2016](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12577).

### Workflow pre-analysis

1.  What is your question of interest? Hypothesis?
2.  Pick an approach (observational, experiment, model)
3.  What data (and covariates) to collect?
4.  Carefully plan your analysis ahead of time

-   Focus on testing your hypothesis

5.  Data wrangling

### Workflow for analysis

0.  Data exploration

1.  Construct a statistical model to test your hypothesis

-   Response variable - which distribution?
-   Fixed effects and potential interactions
-   Random effects

2.  Check model assumptions

-   Residuals
-   Overdispserion or zero-inflation (if applicable)

3.  Check random effects and/or distribution using AIC or LRT

-   Be cautious with this step
-   Usually random effects and distributions should be selected *a priori* based on the study design and type of data collect

4.  Examine significance of terms in model

5.  Examine parameter estimates

-   emmeans and/or emtrend

6.  Construct contrasts

-   also emmeans/emmtrends

7.  Calculate R2m and R2c

8.  Make pretty figure
