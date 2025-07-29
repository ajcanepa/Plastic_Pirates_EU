# # Render R Notebook -------------------------------------------------------
# rmarkdown::render(
#   "Plastic_Pirates_EU_Analysis.Rmd",
#   output_format = "html_notebook",
#   output_file = "Plastic_Pirates_Paper_I_Results.nb.html",
#   output_dir = "OUTPUT/RESULTS",
#   clean = FALSE
# )


# Render .html and .md files ----------------------------------------------
rmarkdown::render(
  "Plastic_Pirates_EU_Analysis.Rmd",
  output_format = "html_document",
  output_file = "OUTPUT/RESULTS/Plastic_Pirates_Paper_I_Results.html",
  clean = FALSE
)

# # Render a .md file -------------------------------------------------------
# knitr::knit("Plastic_Pirates_EU_Analysis.Rmd", output = "OUTPUT/RESULTS/Plastic_Pirates_Paper_I_Results.md")
