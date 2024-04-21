#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, hrbrthemes, fixest,
               scales, gganimate, gapminder, gifski, png, tufte, plotly, OECD,
               ggrepel, survey, foreign, devtools, pdftools, kableExtra, modelsummary,
               kableExtra)
#
#
#
#
knitr::opts_chunk$set(warning = FALSE)
#
#
#
#
#| include: false
#| eval: true
load("Hw5_workspace.Rdata")
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: taxchange
#| fig-cap: Question 1 Graph

question1
#
#
#
#
#
#
#
#
#
#
#
#
#
#
