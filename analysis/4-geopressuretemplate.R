# See https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html

library(GeoPressureR)

# Run workflow step-by-step for a single tag
id <- "26IM" # Run a single tag
geopressuretemplate_config(id)
geopressuretemplate_tag(id)
graph <- geopressuretemplate_graph(id)
geopressuretemplate_pressurepath(id)


## Run workflow for all tags
list_id <- tail(names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)), -1)

list_id <- c("26IL", "26HS", "24EA", "24EP", "24DK", "24TJ", "16IQ", "16JL", "16JB", "16IT", "20TJ", "20TZ", "24IS")
for (id in list_id){
  cli::cli_h1("Tag {id}")
  geopressuretemplate_tag(id)
}

geopressureviz(id)
