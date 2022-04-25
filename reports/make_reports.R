# This script generate the reports and the website
library(rmarkdown)

# Define the track to consider
gdl_list <- c("26IM", "26IL", "26HS", "24EA", "24EP", "24DK", "24TJ", "16IQ", "16JL", "16JB", "16IT", "20TJ", "24IS") #  "20TZ" is removed 1 sta only

# set <- read_excel("data/gdl_settings.xlsx")
# gdl_list = set$gdl_id

# Define the report to produce
report_list <- c("basic_trajectory", "technical_details")

# Generate the following report for each tracks
# This will produce an HTML file for each reports
for (report in report_list) {
  dir.create(paste0("reports/", report), showWarnings = F)
  for (gdl in gdl_list) {
    render(paste0("reports/_", report, ".Rmd"),
      output_file = paste0(report, "/", gdl, ".html"),
      params = list(
        gdl_id = gdl
      )
    )
  }
}

# The index page (List of Report) and all the reports are then combined into a website and moved to
# the docs/ folder to be serve on Github easily.
# Change _site._yml for the overall look of the website and index.Rmd for the content of the index

str <- ""
for (report in report_list) {
  str <- paste0(str,'- text: "',report,'"\n  menu:\n')
  for (gdl in gdl_list) {
    str <- paste0(str,'   - text: "',gdl,'"\n')
    str <- paste0(str,'     href: "/Val-Piora-Wheatear/',report,'/',gdl,'.html\n')
  }
}
writeLines(str)

render_site("./reports")
