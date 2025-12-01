AutomatedMediation: Automatically Run and Summarize Mediation Analyses
================

## Brief Overview

`AutomatedMediation` is an R package that automatically runs every possible
simple mediation analysis between any number of specified variables. It 
wraps the Python `pyprocessmacro` implementation of PROCESS model 4 via the
`reticulate` package, leveraging the superior computational efficiency of 
python's version of the PROCESS macro in combination with workflow convenience 
and many other advantages of R Studio.

The main function, `automated_mediation()`, interactively:

- Prompts you for a CSV file in your working directory.
- Lets you choose a set of variables of interest (predictors, mediators,
  outcomes).
- Optionally lets you specify control variables.
- Automatically runs all ordered X–M–Y combinations as mediation models.
- Prints significant indirect effects to the console.
- Optionally writes a summary CSV of all model results.

This is useful when you have many variables of interest and want to
quickly investigate mediational pathways among them. The csv output
allows for easy filtering in excel for significant effects by predictor,
mediator, outcome or any specific combinations.

## Installation

``` r
# install.packages("devtools")   # if needed
devtools::install_github("AAEStudy/AutomatedMediation")
```

This package depends on:

- R package: `reticulate`
- Python 3 on your system (`python3` or `python` on the PATH)

On first run, `automated_mediation()` will:

- Create a Python virtual environment called `r-pyproc-env`.
- Install the Python packages `numpy<2`, `pandas`, and `pyprocessmacro`
  into that environment using `reticulate::py_install()`.

****NOTE -- FIRST RUN CAN TAKE 5-10 MINUTES!
Creating the python environment naturally takes a bit longer and this is normal!

## Basic Usage

``` r
library(AutomatedMediation)

# Make sure your working directory contains your CSV,
# for example: "mydata.csv"
setwd("path/to/your/data")

automated_mediation()
```

You will then be prompted for:

1.  The CSV file name (e.g., `"psychdataset.csv"`).
2.  A list of variables of interest (minimum 3), separated by comma and
    space: e.g. `X1, X2, M1, Y1`.
3.  Whether to print direct effects for significant mediations.
4.  Whether to include control variables.
5.  Whether to write a results CSV, and its base file name.

## Example (conceptual)

Suppose your CSV has columns:

- `anxiety`
- `depression`
- `attentional_control`
- `mindfulness_nonjudging`

You could run:

``` r
automated_mediation()
# Enter CSV file name: psychdataset.csv
# Enter all interest variables (min 3): anxiety, depression, attentional_control
# Control for any variables? yes
# Enter control variables: mindfulness_nonjudging
# Write results to a CSV file? yes
# Base name for output: mediation_results
```

The function will:

- Explore all ordered triples of the interest variables (e.g., Anxiety →
  Depression → Attentional Control, Depression → Anxiety → Attentional
  Control, etc.).
- For each model, estimate the indirect effect and its bootstrapped
  confidence interval.
- Print any significant indirect effects to the console.
- Save a summary CSV called `mediation_results.csv` in your working
  directory.

## Notes and Limitations

- All variables included in the mediation models must be **numeric** and
  must not contain missing values.
- Each additional variable increases the number of possible mediation
  models combinatorially, so large sets of variables can take a long
  time to run.
- This package is intended as a rapid exploration tool; any models of
  substantive interest should be followed up with careful, theory-driven
  analysis.

## More on the Rationale Behind This Script
A simple mediation assesses whether an independent variable (X) predicts 
or affects a dependent variable (Y) indirectly, through its relationship 
to a third variable known as the mediator (M). In short it tests whether 
the following indirect relationship is significant:

X → M → Y

Running mediation analyses can be extremely time-consuming, especially when 
researchers wish to test many potential relationships among multiple variables. 
For example, if you have 10 variables, manually testing all possible simple mediation 
paths would require 720 separate analyses. Even in modern statistical software, that 
process is tedious and extremely time consuming.

This package solves this problem by automatically running all possible mediation analyses 
between user-specified variables from any wide format CSV dataset and return the results—complete 
with significance tests, confidence intervals, and a CSV output in a matter of seconds or minutes. 

This package uses the Python version of PROCESS (pyprocessmacro) in an R script, 
rather than the R version of the package because the Python implementation can perform 
the analyses exponentially faster. By leveraging the reticulate package, this script provides 
the speed and reliability of Python’s PROCESS macro while maintaining the workflow convenience 
and other functions and advantages of RStudio.
