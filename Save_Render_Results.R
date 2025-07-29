rmarkdown::render(
  "Plastic_Pirates_EU_Analysis.Rmd",
  output_format = "html_notebook",
  output_file = "Plastic_Pirates_Paper_I_Results.nb.html",
  output_dir = "OUTPUT/RESULTS",
  clean = FALSE
)
