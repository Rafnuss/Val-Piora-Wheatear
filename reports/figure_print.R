library(GeoPressureR)
library(raster)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(maps)
library(ggmap)
library(ggnewscale)
library(readxl)
require(gridExtra)
library(cowplot)
library(moveVis)
library(move)
library(lubridate)
library(colorspace)
library(ggsn)
library(ncdf4)

gdl_list <- read_excel("data/gpr_settings.xlsx") %>%
  filter(keep>1) %>% # keep: 0: before migration, 1: some migration flight, 2: up to wintering site, 9: full migration
  .$gdl_id


# Figure non-breeding site ----

p0 <- get_googlemap(center = c(lon = 3, lat = 14), zoom = 6, maptype = "hybrid", size = c(1280, 1280), scale = 4) %>%
  ggmap() + theme_map()

# p0 + coord_fixed(
#   xlim=c(20, 37),
#   ylim=c(4, 12),
#   ratio=1/cos(pi*41.39/180),
#   expand = F
# )+ scalebar(x.min= 20, x.max = 37, y.min=4, y.max=12, transform=T,
#            location = "topright", dist = 100, height = 0.1, model = "WGS84", dist_unit = "km")

# p <- p0
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]

  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  # load(paste0("data/4_basic_graph/", gdl, "_basic_graph.Rdata"))
  load(paste0("data/5_wind_graph//", gdl, "_wind_graph.Rdata"))

  dur <- pressure_prob %>%
    lapply(function(x) {
      difftime(metadata(x)$temporal_extent[2], metadata(x)$temporal_extent[1], units = "days") %>%
        as.numeric()
    }) %>%
    unlist()

  if (gpr$keep==9){
    id_winter <- which(dur>40) %>%
      nth(2)
  } else if( tail(dur,1)>10) {
    id_winter=length(dur)
  } else {
    next
  }

  df <- pressure_prob[[id_winter]] %>%
    disaggregate(2, method = "bilinear") %>%
    as.data.frame(xy = TRUE) %>%
    mutate(layer = layer) %>%
    filter(!is.na(layer)) %>%
    arrange(desc(layer)) %>%
    mutate(layerP = 1 - cumsum(layer) / sum(layer))

  p <- p0 + new_scale_colour() +
    geom_contour(data = df, aes(x = x, y = y, z = layerP, color = ..level..), size = .9, breaks = c(.5, .1)) +
    scale_colour_gradient(
      high = gpr$Color,
      low = "white",
      limits = c(0, .1),
      guide = "none"
    )

  ggsave(plot = p, paste0("reports/figure_print/wintering_location/wintering_location_",gdl,".png"))
}

plot_inset <- ggplot() +
  borders("world", colour = "gray90", fill = "gray50", size = 0.1) +
  coord_quickmap(
    # xlim=c(gpr$extent_W, gpr$extent_E),
    # ylim=c(gpr$extent_S, gpr$extent_N),
    xlim = c(-18, 51),
    ylim = c(-35, 37),
    expand = F
  ) +
  geom_rect(aes(
    xmin = layer_scales(p0)$x$range$range[1],
    xmax = layer_scales(p0)$x$range$range[2],
    ymin = layer_scales(p0)$y$range$range[1],
    ymax = layer_scales(p0)$y$range$range[2]
  ),
  color = "red", alpha = 0.1, size = 1
  ) +
  theme_map() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "#FFFFFF")
  )

pf <- ggdraw() +
  draw_plot(p) +
  draw_plot(plot_inset,
            x = .15,
            y = .06,
            width = 0.35, height = 0.35
  )

ggsave(plot = pf, "reports/figure_print/wintering_location.png")
ggsave(plot = pf, "reports/figure_print/wintering_location.eps", device = "eps")









# Cumulative flight duration ----
d <- list()
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]
  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  load(paste0("data/3_static/", gdl, "_static_prob.Rdata"))

  tmp <- lapply(static_prob, function(x) {
    mt <- metadata(x)
    if (is.null(mt$flight)) {
      f <- 0
    } else {
      f <- as.numeric(sum(difftime(mt$flight$end, mt$flight$start, units = "hours")))
    }
    df <- data.frame(
      start = mt$temporal_extent[1],
      end = mt$temporal_extent[2],
      flight = f
    )
  })

  tmp2 <- do.call("rbind", tmp)

  if (gdl == "20IK") {
    tmp2$start[1] <- tmp2$start[2] - 22 * 60 * 60 * 24
  }
  tmp2$flight[month(tmp2$end) < 8] <- -tmp2$flight
  d[[i]] <- data.frame(
    x = c(tmp2$start, tail(tmp2$end, 1)),
    y = c(0, cumsum(tmp2$flight)),
    gdl = gdl,
    color = darken(gpr$Color, 0.2)
  ) %>%
    mutate(
      x = x + years(2000-year(tmp2$start[1]))
    )
}
d2 <- do.call("rbind", d)

l <- d2 %>%
  summarise(color, gdl) %>%
  unique()

p <- d2 %>%
  ggplot(aes(x = x, y = y, color = color, group = gdl)) +
  geom_step(size = 1) +
  scale_color_identity(
    name = "Track",
    labels = l$gdl,
    breaks = l$color,
    guide = "legend"
  ) +
  scale_x_datetime(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  scale_y_continuous(breaks = seq(0, 210, by = 20)) +
  coord_cartesian(
    xlim = as.POSIXct(c("2000-07-01 UTC", "2001-07-1 UTC"))
  ) +
  ylab("Cumulative Hours of flight") +
  xlab("Date") +
  theme_bw() +
  theme(
    legend.position = c(.5, .1),
    legend.box.background = element_rect(colour = "black"),
    legend.direction = "horizontal"
  )

ggsave(plot=p, "reports/figure_print/cumulative_flight.png", width = 8, height = 4)
# ggsave(p, "reports/figure_print/cumulative_flight.eps", device = "eps", width = 8, height = 4)




# Animated map of marginal ----


p0 <- get_googlemap(
  center = c(
    lon = mean(c(gpr$extent_W, gpr$extent_E)),
    lat = mean(c(gpr$extent_N, gpr$extent_S))
  ),
  zoom = 4, maptype = "satellite"
) %>% ggmap() +
  borders("world", colour = "gray90", size = 0.1) +
  theme_map() +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]

  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  # load(paste0("data/4_basic_graph/", gdl, "_basic_graph.Rdata"))
  load(paste0("data/5_wind_graph//", gdl, "_wind_graph.Rdata"))

  dfr <- do.call(rbind, lapply(static_prob_marginal, function(x) {
    mt <- metadata(x)
    as.data.frame(x, xy = TRUE) %>%
      filter(!is.na(layer) & layer > 0) %>%
      mutate(
        layer = layer / max(layer),
        sta_id = mt$sta_id,
        start = mt$temporal_extent[1],
        end = mt$temporal_extent[2],
        dur = as.numeric(difftime(mt$temporal_extent[2], mt$temporal_extent[1], units = "days"))
      )
  })) %>% mutate(
    sta_id <- as.factor(sta_id),
    rect_norm_start = as.numeric(difftime(start, min(start), units = "days")) / as.numeric(difftime(max(end), min(start), units = "days")),
    rect_norm_start = as.numeric(difftime(end, min(start), units = "days")) / as.numeric(difftime(max(end), min(start), units = "days"))
  )


  p <- p0 + geom_tile(data = dfr, aes(x = x, y = y, fill = layer, group = seq_along(sta_id))) +
    scale_fill_gradient(
      high = gpr$Color,
      low = paste0(gpr$Color, "00"),
      limits = c(-.1, 1),
      guide = "none"
    ) +
    # + geom_rect(data=dfr, aes(xmin = gpr$extent_W, xmax = mean(gpr$extent_W+rect_norm_start), ymin = gpr$extent_S, ymax = gpr$extent_S+.1))
    # geom_text(data = dfr, aes(gpr$extent_W, gpr$extent_N, label=start),nudge_y=-1, nudge_x=.1, size = 8, hjust = 0, color = gpr$Color) +
    transition_states(sta_id) +
    enter_fade() +
    exit_shrink() +
    ggtitle("Stationary period {closest_state}")

  animate(p, height = 800, width = 600)
  anim_save(paste0("reports/figure_print/marginal_animation_", gdl, ".gif"))
}









# Expected habitat use ----
p0 <- get_googlemap(
  center = c(
    lon = mean(c(gpr$extent_W+8, gpr$extent_E-3)),
    lat = mean(c(gpr$extent_N, gpr$extent_S+10))
  ),
  zoom = 4, maptype = "satellite"
) %>% ggmap(darken = c(0.4, "black")) +
  # borders("world", colour = "gray40", size = 0.2) +
  theme_map() +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
# + scale_x_continuous(limits=c(-8, 14))


p <- list()
p[[1]] <- list()
p[[2]] <- list()
d <- list()
rest <- list()
u=0
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]

  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  # load(paste0("data/4_basic_graph/", gdl, "_basic_graph.Rdata"))
  load(paste0("data/5_wind_graph//", gdl, "_wind_graph.Rdata"))

  dur <- pressure_prob %>%
    lapply(function(x) {
      difftime(metadata(x)$temporal_extent[2], metadata(x)$temporal_extent[1], units = "days") %>%
        as.numeric()
    }) %>%
    unlist()

  if (gpr$keep==9){
    id_winter <- which(dur>40) %>%
      nth(2)
  } else if( tail(dur,1)>10) {
    id_winter=length(dur)
  } else {
    next
  }

  for (i_s in c(1, 2)) {
    if (i_s == 1) {
      id_out <- seq(1, id_winter)
      id_in <- seq(2, id_winter - 1)
      s <- "post"
    } else {
      if (id_winter == length(static_prob_marginal)) {
        next
      }
      s <- "pre"
      id_out <- seq(id_winter, length(static_prob_marginal))
      id_in <- seq(id_winter + 1, length(static_prob_marginal) - 1)
    }

    sim_lat <- as.data.frame(t(path_sim$lat[seq(1,10), id_out])) %>%
      mutate(sta_id = path_sim$sta_id[id_out]) %>%
      pivot_longer(-c(sta_id))
    sim_lon <- as.data.frame(t(path_sim$lon[seq(1,10), id_out])) %>%
      mutate(sta_id = path_sim$sta_id[id_out]) %>%
      pivot_longer(-c(sta_id))
    sim <- full_join(sim_lon, sim_lat, by = c("sta_id", "name")) %>% arrange(sta_id)


    tmp2 <- static_prob_marginal[id_in] %>%
      lapply(function(x) {
        dur <- difftime(metadata(x)$temporal_extent[2], metadata(x)$temporal_extent[1], units = "days") %>%
          as.numeric()
        if (dur > 30) {
          dur <- 0
        }
        values(x) <- values(x) / sum(values(x), na.rm = T) * dur
        x
      })

    u=u+1
    rest[[u]] <- Reduce("+", tmp2)

    rest2 <- rest[[u]] %>%
      as.data.frame(xy = TRUE)

    d[[u]] <-data.frame(
      lat = unique(rest2$y),
      dens = rowSums(as.matrix(rest[[u]]), na.rm=T),
      season=i_s,
      gdl = gdl
    )

    res<-rest2 %>%
      filter(!is.na(layer) & layer > 0) %>%
      mutate(
        layer = round(layer/max(layer),3)
      )

    p[[i_s]][[i]] <- p0 +
      geom_path(data = sim, aes(x = value.x, y = value.y, group = name), alpha=.5, color = "white")+
      geom_tile(data = res, aes(x = x, y = y, fill = layer)) +
      scale_fill_gradient(
        high = gpr$Color,
        low = paste0(gpr$Color, "00"),
        limits=c(-0.01,max(res$layer)),
        guide = "none"
      )

    ggsave(paste0("reports/figure_print/expected_use/", s, "_breeding_", gdl, ".png"), plot = p[[i_s]][[i]], width=7.8, height=11.5)
  }
}

do.call("grid.arrange", c(p[[1]], nrow = 1))
do.call("grid.arrange", c(p[[2]], nrow = 1))

d2 <- do.call("rbind", d)

d2 %>%
  ggplot(aes(x=lat,y=dens, color=gdl, group=paste0(gdl,"_",season))) +
  geom_line()


#landcover <- raster("data/land_cover/land_cover.tif")
restt <- Reduce("+", rest)
#plot(landcover)
plot(restt)





# moveVis ----
d <- list()
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]

  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  # load(paste0("data/4_basic_graph/", gdl, "_basic_graph.Rdata"))
  load(paste0("data/5_wind_graph//", gdl, "_wind_graph.Rdata"))

  tmp <- shortest_path %>%
    as.data.frame() %>%
    left_join(pam$sta, by = "sta_id") %>%
    mutate(gdl = gdl,
           col = gpr$Color
    )

  d[[(i - 1) * 2 + 1]] <- tmp %>% mutate(time = start) %>% mutate(time = time + years(2000-year(min(time))))
  d[[(i - 1) * 2 + 2]] <- tmp %>% mutate(time = end) %>% mutate(time = time + years(2000-year(min(time))))
}

d <- do.call("rbind", d)

hist(d$time, "days")





ds <- d %>%
  # filter(time > "2000-3-1") %>% # %>%   filter(gdl !="22NO")
  mutate(time = round_date(time, unit = "days")) %>%
  dplyr::select(-c(start, end, id, col, sta_id)) %>%
  group_by(time, gdl) %>%
  summarise(
    lat = mean(lat),
    lon = mean(lon)
  ) %>%
  ungroup()


# hist(ds$time,"days")

m <- df2move(ds, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
             x = "lon", y = "lat", time = "time", track_id = "gdl"
)

ma <- align_move(m, res = 1, unit = "days")


path_color <- ds %>%
  dplyr::select(gdl) %>%
  unique() %>%
  left_join(read_excel("data/gpr_settings.xlsx"), by = c("gdl" = "gdl_id")) %>%
  .$Color


frames <- frames_spatial(ma,
                         path_colours = path_color, trace_colour = path_color, equidistant = T, ext = extent(static_prob[[1]]),
                         map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoicmFmbnVzcyIsImEiOiIzMVE1dnc0In0.3FNMKIlQ_afYktqki-6m0g"
) %>%
  add_labels(x = NULL, y = NULL) %>% # add some customizations, such as axis labels
  add_timestamps(type = "label", size = 6) %>%
  add_progress()

frames[[204]]

animate_frames(frames, out_file = paste0("reports/figure_print/movevis.gif"), height = 1000, width = 1000, overwrite = T)








# Latitude plot ----

for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]

  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  # load(paste0("data/4_basic_graph/", gdl, "_basic_graph.Rdata"))
  load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))

  tmp <- lapply(pressure_prob, function(x) {
    mt <- metadata(x)
    df <- data.frame(
      start = mt$temporal_extent[1],
      end = mt$temporal_extent[2],
      sta_id = mt$sta_id
    )
  })
  tmp2 <- do.call("rbind", tmp)


sim_lat <- as.data.frame(t(path_sim$lat)) %>%
  mutate(sta_id = path_sim$sta_id) %>%
  pivot_longer(-c(sta_id)) %>%
  left_join(tmp2,by="sta_id")

sim_lat_p <- sim_lat %>%
  filter(sta_id==max(sta_id)) %>%
  mutate(start=end) %>%
  rbind(sim_lat)

sp_lat <- as.data.frame(shortest_path) %>% left_join(tmp2,by="sta_id")

sp_lat_p <- sp_lat %>%
  filter(sta_id==max(sta_id)) %>%
  mutate(start=end) %>%
  rbind(sp_lat)


p <- ggplot() +
  geom_step(data=sim_lat_p, aes(x=start, y=value, group=name), alpha=.07, color=gpr$Color) +
  geom_point(data=sp_lat_p, aes(x=start, y=lat), color=gpr$Color) +
  xlab('Date') +
  ylab('Lattitude') +
  theme_light() +
  coord_cartesian(ylim = c(11,47),
                  xlim=as_datetime(c(
                    paste0(year(min(sim_lat_p$start)),"-08-01"),
                    paste0(year(min(sim_lat_p$start))+1,"-06-01")
                    )),
                  expand = F)

ggsave(paste0("reports/figure_print/latitude_plot/latitude_plot_", gdl, ".png"), plot = p, width=16, height=9)

}




# Altitude plot ----
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]
  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))

  d <- do.call("rbind", shortest_path_timeserie)
  p <- ggplot() +
    # geom_line(data = pam$pressure, aes(x = date, y = obs), colour = "grey") +
    geom_line(data=d, aes(x = date, y = altitude)) +
    geom_line(data=d %>% filter(sta_id > 0), aes(x = date, y = altitude, col = factor(sta_id))) +
    #geom_vline(data = twl, aes(xintercept = twilight, linetype = ifelse(rise, "dashed", "solid"), color="grey"), lwd=0.1) +
    theme_bw() +
    scale_colour_manual(values = col) +
    scale_y_continuous(name = "Altitude (m)") +
    theme(legend.position="none") +
    coord_cartesian(ylim = c(-10, 4800),
                    xlim=as_datetime(c(
                      paste0(year(min(d$date)),"-08-01"),
                      paste0(year(min(d$date))+1,"-06-01")
                    )),
                    expand = F)

  ggsave(paste0("reports/figure_print/altitude_plot/altitude_plot_", gdl, ".png"), plot = p, width=16, height=9)
}








# Histogram of wind, air and groundspeed ----
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]
  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))
  load(paste0("data/5_wind_graph/", gdl, "_grl.Rdata"))

  edge <- t(graph_path2edge(path_sim$id, grl))
  nj <- ncol(edge)

  speed_df <- data.frame(
    as = abs(grl$as[edge]),
    gs = abs(grl$gs[edge]),
    ws = abs(grl$ws[edge]),
    sta_id_s = rep(head(grl$sta_id,-1), nj),
    sta_id_t = rep(tail(grl$sta_id,-1), nj),
    flight_duration = rep(head(grl$flight_duration,-1), nj)
  ) %>% mutate(
    name = paste(sta_id_s,sta_id_t, sep="-")
  )

  plot1 <- ggplot(speed_df, aes(reorder(name, sta_id_s), gs)) + geom_boxplot() + theme_bw() +scale_x_discrete(name = "")
  plot2 <- ggplot(speed_df, aes(reorder(name, sta_id_s), ws)) + geom_boxplot() + theme_bw() +scale_x_discrete(name = "")
  plot3 <- ggplot(speed_df, aes(reorder(name, sta_id_s), as)) + geom_boxplot() + theme_bw() +scale_x_discrete(name = "")
  plot4 <- ggplot(speed_df, aes(reorder(name, sta_id_s), flight_duration)) + geom_point() + theme_bw() +scale_x_discrete(name = "")
  # grid.arrange(plot1, plot2, plot3, plot4, nrow=4)

  p <- arrangeGrob(plot1, plot2, plot3, plot4, nrow=4)
  ggsave(paste0("reports/figure_print/hist_speed_plot/hist_speed_plot_", gdl, ".png"), plot = p, width=16, height=9)
}






# Table of transition ----
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]
  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))
  load(paste0("data/5_wind_graph/", gdl, "_grl.Rdata"))

  edge <- t(graph_path2edge(path_sim$id, grl))
  nj <- ncol(edge)
  nsta <- ncol(path_sim$lon)

  speed_df <- data.frame(
    as = abs(grl$as[edge]),
    gs = abs(grl$gs[edge]),
    ws = abs(grl$ws[edge]),
    sta_id_s = rep(head(grl$sta_id,-1), nj),
    sta_id_t = rep(tail(grl$sta_id,-1), nj),
    flight_duration = rep(head(grl$flight_duration,-1), nj),
    dist = geosphere::distGeo(
      cbind(as.vector(t(path_sim$lon[,1:nsta-1])), as.vector(t(path_sim$lat[,1:nsta-1]))),
      cbind(as.vector(t(path_sim$lon[,2:nsta])),   as.vector(t(path_sim$lat[,2:nsta])))
    ) / 1000
  ) %>% mutate(
    name = paste(sta_id_s,sta_id_t, sep="-")
  )

  alt_df = do.call("rbind", shortest_path_timeserie) %>%
    arrange(date) %>%
    mutate(
      sta_id_s = cummax(sta_id),
      sta_id_t = sta_id_s+1
    ) %>%
    filter(sta_id == 0 & sta_id_s > 0 ) %>%
    group_by(sta_id_s, sta_id_t) %>%
    summarise(
      alt_min = min(altitude),
      alt_max = max(altitude),
      alt_mean = mean(altitude),
      alt_med = median(altitude),
    )

  trans_df <- speed_df  %>%
    group_by(sta_id_s,sta_id_t,flight_duration) %>%
    summarise(
      as_m = mean(as),
      as_s = sd(as),
      gs_m = mean(gs),
      gs_s = sd(gs),
      ws_m = mean(ws),
      ws_s = sd(ws),
      dist_m = mean(dist),
      dist_s = sd(dist)
    ) %>%
    left_join(alt_df)

  write.csv(trans_df,paste0("reports/figure_print/table_transition/", gdl, ".csv"))
}
