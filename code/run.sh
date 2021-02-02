#!/usr/bin/env bash

Rscript variable_selection.R
Rscript -e "rmarkdown::render(input = 'model-nonrecovery.Rmd', 'html_document',output_dir = '../docs/model-nonrecovery.html', clean = TRUE)"
