library(GeoPressureR)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(raster)
library(readxl)

# Set debug T to see all check and set to F once everything is correct
debug <- F

# Define the geolocator data logger id to use
# gdl <- "24TJ" # 26IM 26IL 26HS

# Read its information from gpr_settings.xlsx
gpr <- read_excel("data/gpr_settings.xlsx") %>%
   filter(gdl_id == gdl)

# assert gdl in setting

# Read, classify and label ----
pam <- pam_read(paste0("data/0_PAM/", gpr$gdl_id),
  crop_start = gpr$crop_start,
  crop_end = gpr$crop_end
)

# Read previous classification. Perform only once. Write the new csv file in the data/label folder
if (FALSE){
  flr <- "/Users/raphael/Library/CloudStorage/Box-Box/GeoPressureMAT/data/labels/activity_label/"
  old_csv <- read.csv(paste0(flr,gpr$gdl_id,"_act_pres-labeled.csv"))

  old_csv$date <- strptime(old_csv$timestamp, "%FT%T", tz = "UTC")
  csv_acc <- subset(old_csv, series == "act")
  csv_pres <- subset(old_csv, series == "pres")

   pam$acceleration = data.frame(
    date = csv_acc$date,
    act = csv_acc$value,
    ismig = csv_acc$label==3
  )

  id_pres_match <- match(as.numeric(pam$pressure$date), as.numeric((csv_pres$date)))
  missing_pres <- sum(is.na(id_pres_match))

  pam$pressure$isoutlier <- !is.na(csv_pres$label[id_pres_match])

  trainset_write(pam, pathname = "data/1_pressure/labels/", filename = paste0(pam$id, "_act_pres-labeled"))
}

# no acceleration data present in some/all tracks. replace by labeling
pam$acceleration = pam$pressure
pam$acceleration$obs = 0 # you could also keep the pressure measurement for ease of identification in TRAINSET
pam$acceleration$obs[1] = 1 # trick to avoid y axis issue in TRAINSET (see https://github.com/Rafnuss/GeoPressureR/discussions/26)
pam$acceleration$ismig = FALSE

# Read the label and compute the stationary info
pam <- trainset_read(pam, "data/1_pressure/labels/")
pam <- pam_sta(pam)

# define the discrete colorscale. Used at multiple places.
col <- rep(RColorBrewer::brewer.pal(9, "Set1"), times = ceiling((nrow(pam$sta) + 1) / 9))
col <- col[1:(nrow(pam$sta) + 1)]
names(col) <- levels(factor(c(0, pam$sta$sta_id)))


if (debug) {
  # Test 1 ----
  pam$sta %>%
    mutate(
      duration = difftime(end, start, units = "hours"),
      next_flight_duration = difftime(lead(start), end, units = "hours")
    ) %>%
    filter(duration < 3) %>%
    arrange(duration)


  # Test 2 ----
  pressure_na <- pam$pressure %>%
    mutate(obs = ifelse(isoutlier | sta_id == 0, NA, obs))

  p <- ggplot() +
    geom_line(data = pam$pressure, aes(x = date, y = obs), col = "grey") +
    geom_line(data = pressure_na, aes(x = date, y = obs, color = factor(sta_id))) +
    # geom_point(data = subset(pam$pressure, isoutlier), aes(x = date, y = obs), colour = "black") +
    theme_bw() +
    scale_color_manual(values = col) +
    scale_y_continuous(name = "Pressure(hPa)")

  ggplotly(p, dynamicTicks = T) %>% layout(showlegend = F)
}


# Filter stationary period based on the number of pressure datapoint available
thr_dur <- gpr$thr_dur # 24*4 # duration in hour. Decrease this value down to gpr$thr_dur
res <- as.numeric(difftime(pam$pressure$date[2], pam$pressure$date[1], units = "hours"))
sta_id_keep <- pam$pressure %>%
  filter(!isoutlier & sta_id > 0) %>%
  count(sta_id) %>%
  filter(n * res > thr_dur) %>%
  .$sta_id

# Duplicate the pam data to avoid issue after filtering, and put NA on the sta to not consider
pam_short <- pam
pam_short$pressure <- pam_short$pressure %>%
  mutate(sta_id = ifelse(sta_id %in% sta_id_keep, sta_id, NA))

# Query pressure map
# We overwrite the setting parameter for resolution to make query faster at first
pressure_maps <- geopressure_map(pam_short$pressure,
  extent = c(gpr$extent_N, gpr$extent_W, gpr$extent_S, gpr$extent_E),
  scale = gpr$map_scale,
  max_sample = gpr$map_max_sample,
  margin = gpr$map_margin
)

# Convert to probability map
pressure_prob <- geopressure_prob_map(pressure_maps,
  s = gpr$prob_map_s,
  thr = gpr$prob_map_thr
)

id_calib <- c(1,length(pressure_prob))
pressure_prob[id_calib] <- geopressure_prob_map(pressure_maps[id_calib],
                                      s = gpr$prob_map_s_calib,
                                      thr = gpr$prob_map_thr)

if (debug) {
  # Compute the path of the most likely position
  path <- geopressure_map2path(pressure_prob)

  # Query timeserie of pressure based on these path
  pressure_timeserie <- geopressure_ts_path(path, pam_short$pressure, include_flight = F)

  # Test 3 ----
  p <- ggplot() +
    geom_line(data = pam$pressure, aes(x = date, y = obs), colour = "grey") +
    # geom_point(data = subset(pam$pressure, isoutlier), aes(x = date, y = obs), colour = "black") +
    geom_line(data = pressure_na, aes(x = date, y = obs, color = factor(sta_id)), size = 0.5) +
    geom_line(data = do.call("rbind", pressure_timeserie), aes(x = date, y = pressure0, col = factor(sta_id)), linetype = 2) +
    theme_bw() +
    scale_colour_manual(values = col) +
    scale_y_continuous(name = "Pressure(hPa)")

  ggplotly(p, dynamicTicks = T) %>% layout(showlegend = F)


  # Test 4 ----
  par(mfrow = c(5, 6), mar = c(1, 1, 3, 1))
  for (i_r in seq_len(length(pressure_timeserie))) {
    if (!is.null(pressure_timeserie[[i_r]])) {
      i_s <- unique(pressure_timeserie[[i_r]]$sta_id)
      df3 <- merge(pressure_timeserie[[i_r]], subset(pam$pressure, !isoutlier & sta_id == i_s), by = "date")
      df3$error <- df3$pressure0 - df3$obs
      hist(df3$error, main = i_s, xlab = "", ylab = "", freq = F)
      x <- seq(-4, 4, length=100)
      if (i_r %in% c(1, length(pressure_timeserie))) {
          lines(x,dnorm(x,0,gpr$prob_map_s_calib), col = "red")
        } else{
          lines(x,dnorm(x,0,gpr$prob_map_s), col = "red")
      }
    }
  }

  # Map the most likely position
  sta_duration <- unlist(lapply(pressure_prob, function(x) {
    as.numeric(difftime(metadata(x)$temporal_extent[2], metadata(x)$temporal_extent[1], units = "days"))
  }))
  pal <- colorFactor(col, as.factor(seq_len(length(col))))
  leaflet() %>%
    addProviderTiles(providers$Stamen.TerrainBackground) %>%
    addFullscreenControl() %>%
    addPolylines(lng = path$lon, lat = path$lat, opacity = 0.7, weight = 1, color = "#808080") %>%
    addCircles(lng = path$lon, lat = path$lat, opacity = 1, color = pal(factor(path$sta_id, levels = pam$sta$sta_id)), weight = sta_duration^(0.3) * 10)
}

# Export ----
save( # pressure_timeserie,
  pressure_prob,
  pam,
  gpr,
  file = paste0("data/1_pressure/", gpr$gdl_id, "_pressure_prob.Rdata")
)
