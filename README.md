# Replication Code: The Government's Gift to Givers

Replication code for all tables and computed numbers in:

**"The Government's Gift to Givers: Donating Appreciated Stock"**  
Thomas J. Brennan and David M. Schizer  
*Florida Tax Review* (forthcoming)

## Overview

This repository contains an R script that reproduces every table in the paper (Tables 1-21 and Appendix Table A1), along with the computed numbers used in the text. Option values are calculated with the Black-Scholes formulas for European options on a non-dividend-paying stock, using a 5% annual (4.88% continuously compounded) risk-free rate.

## Requirements

- **R**: developed and tested with R 4.6.1 or later
- **Required R packages**: `derivmkts`, `knitr`

```r
install.packages(c("derivmkts", "knitr"))
```

## Running the Replication

Open R or RStudio with this repository folder as the working directory, then run:

```r
source("GovernmentsGiftToGivers_table_calculations.R")
```

or, from a terminal:

```
Rscript GovernmentsGiftToGivers_table_calculations.R
```

The script prints each table to the console, labeled by table number and by the Part of the paper in which it appears, followed by session information. The underlying data frames (`tab_1`, `tab_2`, ...) and formatted tables (`tab_1s`, `tab_2s`, ...) remain in the global environment.
