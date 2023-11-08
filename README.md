# Preface {.unnumbered}

This is a study project of the book *Fundamentals of Causal Inference With R* by Babette A. Brumback. Ms Brumback uses base R for all the code. This study project is coding with the tidyverse way. The motivation stems from the opinion that, in practice, the code is as important as the theory and learning better coding practice should be started as early as possible.

## Where-to-find

-   The online version of this document can be found at [FCI](https://github.com/FrankLef/fci2020).

-   The companion package for this project is `fciR` and can be found at [fciR](https://github.com/FrankLef/fciR).

-   The exercises can be found at [FCI exercises](https://github.com/FrankLef/fci2020). This repo could be set as a **private** to avoid frustrating the publisher.

## Acknowledgements

Many thanks to Babette A. Brumback for a book full of amazing observations, tricks and tips.

## Packages

For data processing and analysis the following packages are used

| Package                                                               | Comment                                     |
|-------------------------|----------------------------------------------|
| [fciR](https://github.com/FrankLef/fciR)                              | Companion R package for this book           |
| [conflicted](https://conflicted.r-lib.org)                            | Manage conflict resolution amongst packages |
| [tidyverse](https://www.tidyverse.org)                                | Tidyverse is the favored coding way         |
| [skimr](https://docs.ropensci.org/skimr/)                             | Summary statistics                          |
| [modelr](https://modelr.tidyverse.org)                                | Create elegant pipelines when modelling     |
| [simpr](https://statisfactions.github.io/simpr/)                      | Generate simulated data                     |
| [MonteCarlo](https://github.com/FunWithR/MonteCarlo)                  | Monte Carlo simulation                      |
| [geepack](https://cran.r-project.org/web/packages/geepack/index.html) | Generalized estimating equations solver     |
| [rsample](https://rsample.tidymodels.org)                             | Resampling and bootstraps                   |

For plotting, graphs and tables these packages are used

| Package                                                             | Comment                                          |
|-------------------------|----------------------------------------------|
| [ggplot](https://ggplot2.tidyverse.org)                             | Create graphics based on the grammar of graphics |
| [ggdag](https://github.com/r-causal/ggdag)                          | Causal directed acyclic graphs                   |
| [tidygraph](https://tidygraph.data-imaginist.com)                   | Graphs and networks manipulation                 |
| [ggvenn](https://cran.r-project.org/web/packages/ggvenn/index.html) | Venn diagrams by ggplot2. Used in chap. 4        |
| [gt](https://gt.rstudio.com)                                        | Nice-looking tables                              |
