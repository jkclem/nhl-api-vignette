# author: John Clements
# date: 06/19/2021
# purpose: Render nhlVignette.Rmd as a .md file called README.md for my repo.

rmarkdown::render(
  input="nhlVignette.Rmd",
  output_format = "github_document",
  output_file = "README.md",
  runtime = "static",
  clean = TRUE,
  params = NULL,
  knit_meta = NULL,
  envir = parent.frame(),
  run_pandoc = TRUE,
  quiet = FALSE,
  encoding = "UTF-8"
)
