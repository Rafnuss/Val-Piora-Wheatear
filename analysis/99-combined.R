gdl_list <- c("26IM", "26IL", "26HS", "24EA", "24EP", "24DK", "24TJ", "16IQ", "16JL", "16JB", "16IT", "20TJ", "24IS") #  "20TZ" is removed 1 sta only

for (gdl in gdl_list){
  # source('analysis/1-pressure.R')
  # source('analysis/2-light.R')
  # source('analysis/3-static.R')
  source('analysis/4-basic-graph.R')
}

## Check
gdl <- "16IQ"
load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
load(paste0("data/2_light/", gdl, "_light_prob.Rdata"))
load(paste0("data/3_static/", gdl, "_static_prob.Rdata"))
load(paste0("data/4_basic_graph/", gdl, "_basic_graph.Rdata"))

# sta_static <- unlist(lapply(static_prob, function(x) raster::metadata(x)$sta_id))
sta_marginal <- unlist(lapply(static_prob_marginal, function(x) raster::metadata(x)$sta_id))
sta_pres <- unlist(lapply(pressure_prob, function(x) raster::metadata(x)$sta_id))
sta_light <- unlist(lapply(light_prob, function(x) raster::metadata(x)$sta_id))
pressure_prob <- pressure_prob[sta_pres %in% sta_marginal]
light_prob <- light_prob[sta_light %in% sta_marginal]

geopressureviz <- list(
  pam_data = pam,
  static_prob = static_prob,
  static_prob_marginal = static_prob_marginal,
  pressure_prob = pressure_prob,
  light_prob = light_prob,
  pressure_timeserie = static_timeserie
)
save(geopressureviz, file = "~/geopressureviz.RData")

shiny::runApp(system.file("geopressureviz", package = "GeoPressureR"),
              launch.browser = getOption("browser")
)
