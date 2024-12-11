library(GeoLocatoR)
library(frictionless)

pkg <- create_gldp_geopressuretemplate()
pkg <- add_gldp_geopressuretemplate(pkg)

observations(pkg) <- read.csv("data/observations.csv")
tags(pkg) <- read.csv("data/tags.csv")

pkg$title <- "Geolocator Data Package: Trajectories of Northern Wheatears breeding in Val Piora (Switzerland) and Val Troncea (Italy)"
pkg$id <- "https://doi.org/10.5281/zenodo.14364136"
pkg$description <- "This repository contains the raw data and the trajectory information generated with the GeoPressureR workflow, following the GeoLocator Data Package standard (v0.1). The complete code used to generate this datapackage can be found at https://github.com/Rafnuss/Val-Piora-Wheatear/.
It contains 10 tags fitted on Northern Wheatears in Val Piora, Switzerland, between 2016 and 2020 and retrieved between 2017 and 2021, and 4 tags fitted in Val Troncea, Italy, in 2019 and 2020 and retrieved in 2020 and 2021. The data is associated to Rime et al. ([2023](https://doi.org/10.1186/s40462-023-00381-6))."

# pkg$contributors
list(
  list(
    givenName = "Raphaël",
    familyName = "Nussbaumer",
    roles = c("ContactPerson", "DataCurator"),
    path = "https://orcid.org/0000-0002-8185-1020"
  ),
  list(
    givenName = "Martha Maria",
    familyName = "Sander",
    roles = "DataCollector",
    path = "https://orcid.org/0000-0002-9036-0450"
  ),
  list(
    givenName = "Felix",
    familyName = "Liechti",
    roles = "Supervisor",
    path = "https://orcid.org/0000-0001-9473-0837"
  ),
  list(
    givenName = "Dan",
    familyName = "Chamberlain",
    roles = "Supervisor",
    path = "https://orcid.org/0000-0002-5381-2024"
  ),
  list(
    givenName = "Valentin",
    familyName = "Amrhein",
    roles = "Supervisor",
    path = "https://orcid.org/0000-0001-5173-4571"
  ),
  list(
    givenName = "Martins",
    familyName = "Briedis",
    roles = "Supervisor",
    path = "https://orcid.org/0000-0002-9434-9056"
  ),
  list(
    givenName = "Barbara",
    familyName = "Helm",
    roles = "Supervisor",
    path = "https://orcid.org/0000-0002-6648-1463"
  ),
  list(
    givenName = "Christoph M.",
    familyName = "Meier",
    roles = c("Supervisor", "ProjectManager"),
    path = "https://orcid.org/0000-0001-9584-2339"
  ),
  list(
    givenName = "Yann",
    familyName = "Rime",
    roles = c("Contributor", "DataCollector", "ProjectLeader"),
    path = "https://orcid.org/0009-0005-7264-6753"
  )
)

pkg$citation <- "Nussbaumer, R. & Rime, Y. (2024). Geolocator Data Package: Trajectories of Northern Wheatears breeding in Val Piora (Switzerland) and Val Troncea (Italy) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.14364136"

pkg$relatedIdentifiers <- list(
  list(
    relationType="IsSupplementTo",
    relatedIdentifier="10.1186/s40462-023-00381-6",
    relatedIdentifierType="DOI"
  ),
  list(
    relationType="IsDerivedFrom",
    relatedIdentifier=" q",
    relatedIdentifierType="DOI"
  )
)

plot(pkg)

check_gldp(pkg)

dir.create("data/zenodo", showWarnings = FALSE)
write_package(pkg,"data/datapackage")

