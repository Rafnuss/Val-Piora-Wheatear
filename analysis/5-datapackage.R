library(GeoLocatoR)
library(frictionless)

pkg <- create_gldp_geopressuretemplate()
pkg <- add_gldp_geopressuretemplate(pkg)

check_gldp(pkg)

dir.create("data/zenodo", showWarnings = FALSE)
write_package(pkg,"data/zenodo")

