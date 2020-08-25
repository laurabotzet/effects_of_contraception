#' # Helper functions used throughout {.tabset .tabset-sticky}
#' documentation on the functions is interspersed through code comments
#'
#' ## set some options
#' dont show messages when loading libraries
library = function(...) suppressMessages(base::library(...))
#' never set strings as factors automatically (google for reason why)
options(stringsAsFactors = FALSE)
#' show four significant digits tops
options(digits = 4)
#' tend not to show scientific notation, because we're just psychologists
options(scipen = 7)
#' make output a bit wider
options(width = 110)
#' set a seed to make analyses depending on random number generation reproducible
set.seed(1710) # if you use your significant other's birthday make sure you stay together for the sake of reproducibility


#' ## Load packages
#' generate the site
library(rmarkdown)
#' set options for chunks
library(knitr)
#' my formr utility package to generate e.g. the bibliography
library(formr)
#' pretty-printed output
library(pander)
#' tidyverse date times
library(lubridate)
#' tidyverse strings
library(stringr)
#' extractor functions for models
library(broom)
#' grammar of graphics plots
library(ggplot2)
library(ggdist)
library(ggthemes)
library(codebook)
library(apaTables)
library(brms)
library(loo)
library(bayestestR)
library(magrittr)
library(purrr)
library(forcats)
library(modelr)
library(ggdist)
library(tidybayes)
library(cowplot)
library(rstan)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(sensemakr)
library(jtools)
library(Cairo)

#'UpSetR is used to generate exclusion plots
library(UpSetR)

#' tidyverse: has a lot of naming conflicts, so always load last
library(tidyverse)

#' some packages may be needed without being loaded
fool_packrat = function() {
  # needed to install formr package
  library(devtools)
  # needed to actually run rmarkdown in RStudio, but for some reason not in its dependencies
  library(formatR)
}

#' ## Spin R files
#' R scripts can be documented in markdown using Roxygen comments, as demonstrated here
#' This function turns all R files (that don't have an Rmd file of the same name and that don't start with an underscore _) into HTML pages
spin_R_files_to_site_html = function() {
  library(knitr)
  all_Rs = c(list.files(pattern = "^[^_].+\\.R$"), ".Rprofile")
  component_Rmds = list.files(pattern = "^_.+\\.Rmd$")
  temporary_Rmds = c()
  for (i in seq_along(all_Rs)) {
    if(all_Rs[i] == ".Rprofile") {
      Rmd_file = ".Rprofile.Rmd"
    } else {
      Rmd_file = paste0(all_Rs[i], "md")
    }
    if (!file.exists(Rmd_file)) {
      next_document = length(temporary_Rmds) + 1
      if(file.exists(all_Rs[i])) {
      temporary_Rmds[next_document] = spin(all_Rs[i], knit = FALSE, envir = new.env(), format = "Rmd")
      prepended_yaml = paste0(c("---
output:
  html_document:
    code_folding: 'show'
---

", readLines(temporary_Rmds[next_document])), collapse = "\n")
      cat(prepended_yaml, file = temporary_Rmds[next_document])
      }
    }
  }
  components_and_scripts = c(temporary_Rmds, component_Rmds)
  for (i in seq_along(components_and_scripts)) {
    opts_chunk$set(eval = FALSE, cache = FALSE)
    # if we call render_site on the .R file directly it adds a header I don't like
    rmarkdown::render_site(components_and_scripts[i], quiet = TRUE)
  }
  opts_chunk$set(eval = TRUE, cache = TRUE)
  unlink(temporary_Rmds)
}

#' ## Output options
#' use pander to pretty-print objects (if possible)
opts_chunk$set(
  dev = "png"
)

#' don't split tables, scroll horizontally
panderOptions("table.split.table", Inf)


#' ##### we use this function to automatically get nice tables
pander_handler = function(x, ...) {
  anyS3method = function(x) {
    classes = class(x)
    any(sapply(classes, FUN = function(classes) { !is.null(getS3method('pander',classes,TRUE)) } ))
  }
  if ("knit_asis" %in% class(x)) {
    x # obj is knit_asis already, don't touch it
    # (useful if e.g. pander is called with options in the doc)
  } else if (anyS3method(x)) {
    pander(x, row.names = F, ...) # if method available, pander
  } else if (isS4(x)) {
    show(x)
  } else {
    print(x)
  }
}


theme_set(theme_tufte(base_size = 20, base_family='Helvetica Neue'))

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text = element_text(size=20))

