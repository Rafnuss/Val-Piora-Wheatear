library(GeoLocatoR)
library(frictionless)

zenodo <- zen4R::ZenodoManager$new(token = keyring::key_get(service = "ZENODO_PAT"))

z <- zenodo$getDepositionByConceptDOI("10.5281/zenodo.14364135")

pkg <- zenodoRecord2gldp(z)

pkg <- add_gldp_geopressuretemplate(pkg)

validate_gldp(pkg)

# New version
pkg$version <- "v1.2.0"
pkg <- pkg %>% update_gldp_metadata()
z$setVersion(pkg$version)

# Write datapackage
write_package(pkg, pkg$version)

# Create new record
z <- zenodo$depositRecordVersion(
  z,
  delete_latest_files = TRUE,
  files = file.path(pkg$version, list.files(pkg$version)),
  publish = FALSE
)





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

