library(tidyverse)
library(readxl)

gdl_list <- read_excel("data/gpr_settings.xlsx") %>%
  filter(keep>1) %>% # keep: 0: before migration, 1: some migration flight, 2: up to wintering site, 9: full migration
  .$gdl_id

for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]
  # source("analysis/1-pressure.R")
  # source("analysis/2-light.R")
  # source("analysis/3-static.R")
  source("analysis/4-basic-graph.R")
  source("analysis/5-2-wind-graph_create.R")
  source("analysis/5-3-wind-graph_analyse.R")
}

## Check with GeoPressureViz
gdl <- gdl_list[6]
load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
load(paste0("data/2_light/", gdl, "_light_prob.Rdata"))
load(paste0("data/3_static/", gdl, "_static_prob.Rdata"))
load(paste0("data/4_basic_graph/", gdl, "_basic_graph.Rdata"))

geopressureviz(
  pam = pam,
  static_prob = static_prob,
  static_prob_marginal = static_prob_marginal,
  pressure_prob = pressure_prob,
  light_prob = light_prob,
  pressure_timeserie = shortest_path_timeserie
)



# Add Wind
dir.create("data/5_wind_graph", showWarnings = FALSE)
# Create the request
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]
  # source("analysis/5-1-wind-graph_download.R")
  rstudioapi::jobRunScript("analysis/5-1-wind-graph_download.R",
                           name = paste0("wind_graph_download_", gdl),
                           workingDir = ".",
                           importEnv = TRUE)
}






# Update gpr ---
# If you modified some value in gpr_settings, you can update gpr with the following code
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]

  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))

  gpr <- read_excel("data/gpr_settings.xlsx") %>%
    filter(gdl_id == gdl)

  save(
    # pressure_timeserie, # can be removed in not in debug mode
    pressure_prob,
    pam,
    gpr,
    file = paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata")
  )
}
