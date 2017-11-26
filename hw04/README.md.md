# HW04 - Grades Visualizer

From the user point of view, the main deliverable will be a shiny app to visualize: 1) the
overall grade distribution, 2) the distribution and summary statistics of various scores, and
3) the relationships between pairs of scores.
From the developer point of vew, you will have to write a number of functions that help you
process the data, and compute the required statistics. In addition, you will have to write
unit tests for the programmed functions, which is an essential part of any programming task.
In summary, this project involves working around three primary aspects:
 Low level coding:
�C writing functions (and document them)
�C testing functions (runing unit tests)
 Data Analysis Cycle:
�C data preparation, and reformatting
�C data analysis and visualization
�C reporting via interactive tools
 Practice with R packages:
�C "testthat"
�C "shiny"
�C "ggvis"
�C optional: "readr", "dplyr", etc.


To run Shiny App:

```{r}
library(shiny)
# Run an app from a subdirectory in the repo
runGitHub("stat133-hws-fall17", "josiayuan", subdir = "hw04/app")
```