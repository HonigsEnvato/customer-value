# Customer Valuation

This repository contains the estimation of lifetime value of Elements
customers.

This
[presentation](https://docs.google.com/presentation/d/1AmM7Ppa9dRTxk-RGwOs2YFf1iYLSggrBoajy6KBDgKo/edit?usp=sharing)
gives a quick run down of what is LTV and high level information.


If you are looking to calculate LTV breakdown for custom groups, please refer to the [guide](create_custom_group.md).

## Organisation

The work is organised as follow:

### data

This folder contains R and SQL scripts which perform ETL required for
the analysis.

The `data_processing.R` is the main script which executes all other
scripts.

### demo

This folder contains a small Shiny app which provides a demonstration
of the lifetime calculator.

### eda

`eda` is short for exploratory data analysis. The folder is subdivided
by each round of analysis, the `.Rmd` report can be rendered to
`.html` by

```{r}
rmarkdown::render("<file_name.Rmd>")
```

### model

This is where the model is trained, currently only the example model is stored.


### Custom functions

The [controller.R](controller.R) script contains all the utility
functions that have been created through the analysis. They should
potentially be cleaned up and wrapped up into a package.


## Pipeline