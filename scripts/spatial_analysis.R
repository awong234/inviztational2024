library(RSQLite)
library(sf)
library(spData)
library(DBI)
library(dplyr)
library(mgcv)
library(ggplot2)

con = dbConnect(RSQLite::SQLite(), 'data/data.db')

source('functions.R')

output_folder = 'img/analysis'
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

windowsFonts("consolas" = windowsFont("Consolas"))

# -----------

world = spData::world
sf_use_s2(FALSE)
atlantic = world %>% st_crop(xmin = -160, xmax = 0, ymin = 0, ymax = 80)
sf_use_s2(TRUE)


path = dbGetQuery(con,
                  '
select p.*, n.hurricane_name, n.hurricane_year,
prop.maximum_sustained_wind_in_knots, prop.status_of_system, prop.minimum_pressure_in_millibars
from paths p
join names n on
p.hurricane_id = n.hurricane_id
join properties prop on
prop.hurricane_id = p.hurricane_id
and prop.datetime = p.datetime
')

path = path %>% add_overland_indicator()

speed_model = gam(formula = average_speed_mph ~ s(latitude, longitude, bs = "sos", k = 60) + over_land, data = path, weights = delta_t_hours)

bbox = st_bbox(atlantic)
grid = expand.grid(longitude = seq(bbox['xmin'], bbox['xmax'], length = 200), latitude = seq(bbox['ymin'], bbox['ymax'], length=200))
grid = add_overland_indicator(grid)
grid$fill = predict(speed_model, grid)

png(filename = file.path(output_folder, 'avg-storm-speed.png'), width=1920, height=1080, res=200, bg='transparent')
ggplot(grid) +
    geom_tile(aes(x = longitude, y = latitude, fill = fill)) +
    geom_sf(data = st_geometry(atlantic), fill = NA, color = 'white') +
    theme_bw() +
    scale_fill_viridis_c(name = "Mean Speed (mph)") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(label = "Average Storm Speed", subtitle = "fit by GAM model with splines-on-sphere smoothing") +
    theme(
        panel.border = element_rect(colour = 'white',  linewidth = 1, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill  = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", colour = 'white'),
        legend.key = element_rect(fill = "transparent"),
        text = element_text(size = 15, family = 'consolas', color = 'white'),
        axis.text = element_text(size = 15, family = 'consolas', color = 'white'),
        axis.ticks.x = element_blank()
    )
dev.off()

## Overland

# paths_interp = path %>% filter(hurricane_year > 2000) %>% interpolate_paths(interval_hours = 5/60)
# paths_interp = paths_interp %>% add_overland_indicator()
#
# overland_summary = paths_interp %>%
#     filter(!is.na(delta_t_hours)) %>%
#     group_by(hurricane_id) %>%
#     summarize(lifetime = sum(delta_t_hours, na.rm = TRUE),
#               overland_pct = weighted.mean(over_land, w = delta_t_hours, na.rm = TRUE),
#               time_over_land = mean(over_land * delta_t_hours, na.rm = TRUE),
#               sum_land = sum(over_land, na.rm = TRUE),
#               average_speed_mph = weighted.mean(average_speed_mph, w = delta_t_hours, na.rm = TRUE))
#
# ggplot(overland_summary) +
#     geom_point(aes(x = overland_pct, y = lifetime)) +
#     geom_smooth(aes(x = overland_pct, y = lifetime))
#
# ggplot(overland_summary) +
#     geom_point(aes(x = sum_land, y = lifetime)) +
#     geom_smooth(aes(x = sum_land, y = lifetime))
#
# ggplot(overland_summary) +
#     geom_point(aes(x = time_over_land, y = lifetime)) +
#     geom_smooth(aes(x = time_over_land, y = lifetime))
#
# ggplot(overland_summary) +
#     geom_point(aes(x = overland_pct, y = average_speed_mph)) +
#     geom_smooth(aes(x = overland_pct, y = average_speed_mph))


# Vectors -------------------

path2 = path %>%
    filter(hurricane_year > 2000) %>%
    group_by(hurricane_id) %>%
    arrange(datetime) %>%
    mutate(
        longitude = case_when(longitude > 0 ~ 360+longitude, .default = longitude),
        vector_x = sin(bearing),
        vector_y = cos(bearing),
        vector_x2 = vector_x * average_speed_mph,
        vector_y2 = vector_y * average_speed_mph,
        next_lat = lead(latitude),
        next_lon = lead(longitude),
        lat_delta = latitude - lag(latitude),
        lon_delta = longitude - lag(longitude),
        accel = average_speed_mph - lag(average_speed_mph)
        ) %>%
    select(datetime, latitude, longitude, next_lat, next_lon, vector_x, vector_y, vector_x2, vector_y2, average_speed_mph, accel, bearing, bearing_degrees, over_land, hurricane_year) %>%
    ungroup() %>%
    filter(longitude < 300)

path2 = na.omit(path2)

# # North vs. south vectors point in different directions. Preferably we'd capture this pattern in a model.
# ggplot(path2 %>%
#            mutate(updown = case_when(latitude <= 25 ~ 'southern', latitude > 25 ~ 'northern', .default = 'middle'),
#                   updown = factor(updown, levels = rev(c('southern', 'middle', 'northern')))) %>%
#            slice_sample(n = 1000)) +
#     geom_segment(aes(x = bearing_degrees, y = 0, xend = bearing_degrees, yend = average_speed_mph), alpha = 0.2) +
#     geom_hline(yintercept = 11) +
#     scale_x_continuous(limits = c(-180, 180), breaks = c(-180, -90, 0, 90, 180)) +
#     coord_radial(start = to_rad(180), end = to_rad(-180), expand = FALSE) +
#     facet_grid(updown~1) +
#     theme_bw()
#
# # Direction of movement over space.
# ggplot(path2 %>% filter(longitude > -150) %>% slice_sample(prop = 0.2)) +
#     geom_sf(data = atlantic) +
#     geom_segment(aes(x = longitude, xend = longitude+vector_x*1.1,
#                      y = latitude, yend = latitude+vector_y*1.1,
#                      color = bearing_degrees),
#                  arrow = arrow(length = unit(0.01, 'npc'))) +
#     scale_color_gradientn(colors = c('green', 'red', 'yellow', 'blue', 'green'), values = scales::rescale(c(-180, -90, 0, 90, 180)) )


# Vector model ------------------------------

mod = mgcv::gam(data = path2,
                formula = list(vector_x2 ~ s(latitude, longitude, bs = 'sos', k=60) + over_land,
                               vector_y2 ~ s(latitude, longitude, bs = 'sos', k=60) + over_land),
                family = mvn(d=2))

bbox = st_bbox(atlantic)
grid = expand.grid(longitude = seq(bbox['xmin'], bbox['xmax'], length = 200), latitude = seq(bbox['ymin'], bbox['ymax'], length=200))
grid = add_overland_indicator(grid)
vector_mod_pred = predict(mod, grid)
grid$vector_x = vector_mod_pred[,1]
grid$vector_y = vector_mod_pred[,2]
grid$speed = apply(X = vector_mod_pred, MARGIN = 1, FUN = \(x) norm(x, type = '2'))

size = 50
bbox_small = bbox+c(10, 10, -10, -10)
grid_smaller = expand.grid(longitude = seq(bbox_small['xmin'], bbox_small['xmax'], length = size), latitude = seq(bbox_small['ymin'], bbox_small['ymax'], length=size))
grid_smaller = add_overland_indicator(grid_smaller)
vector_mod_pred = predict(mod, grid_smaller)
grid_smaller$vector_x = vector_mod_pred[,1]
grid_smaller$vector_y = vector_mod_pred[,2]
grid_smaller$speed = apply(X = vector_mod_pred, MARGIN = 1, FUN = \(x) norm(x, type = '2'))

png(filename = file.path(output_folder, 'storm-vector-field.png'), width=1920, height=1080, res=200, bg='transparent')
pl = ggplot(grid) +
    geom_tile(aes(x = longitude, y = latitude, fill = speed)) +
    geom_segment(data = grid_smaller,
                 aes(x = longitude,
                     y = latitude,
                     xend = longitude + (vector_x*(speed/100)),
                     yend = latitude + (vector_y*(speed/100))),
                 arrow = arrow(length = unit(0.005, 'npc')),
                 alpha = 0.5
                 ) +
    geom_sf(data = st_geometry(atlantic), fill = NA, color = 'white') +
    theme_bw() +
    scale_fill_viridis_c(name = "Mean Speed (mph)") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(label = "Storm Vector Field", subtitle = "Speed and direction.") +
    theme(
        panel.border = element_rect(colour = 'white',  linewidth = 1, fill = NA),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill  = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", colour = 'white'),
        legend.key = element_rect(fill = "transparent"),
        text = element_text(size = 15, family = 'consolas', color = 'white'),
        axis.text = element_text(size = 15, family = 'consolas', color = 'white'),
        axis.ticks.x = element_blank()
    )
print(pl)
dev.off()

make_projection = function(pos, mod, n) {
    pos = replicate(n, pos, FALSE) |> bind_rows()
    pos$latitude_low = NA
    pos$longitude_low = NA
    pos$latitude_high = NA
    pos$longitude_high = NA

    for (i in 2:n) {
        vec = predict(mod, pos[i-1,], se.fit = TRUE)
        speed = norm(vec$fit, 'F')
        bearing = asin(vec$fit[1]/speed) |> to_deg()
        next_pos = next_position(pos$latitude[i-1], pos$longitude[i-1], bearing_deg = bearing, distance_mi = speed*6)
        next_pos_lower = next_position(pos$latitude[i-1] - 1.96*vec$se.fit[1], pos$longitude[i-1]  - 1.96*vec$se.fit[2], bearing_deg = bearing, distance_mi = speed)
        next_pos_upper = next_position(pos$latitude[i-1] + 1.96*vec$se.fit[1], pos$longitude[i-1]  + 1.96*vec$se.fit[2], bearing_deg = bearing, distance_mi = speed)
        pos$speed[i] = speed
        pos$bearing[i] = bearing
        pos$latitude[i] = next_pos[1]
        pos$longitude[i] = next_pos[2]
        pos$latitude_low[i] = next_pos_lower[1]
        pos$longitude_low[i] = next_pos_lower[2]
        pos$latitude_high[i] = next_pos_upper[1]
        pos$longitude_high[i] = next_pos_upper[2]
    }
    pos
}


# Play out paths "ride vector field" ----------------

mod = mgcv::gam(data = path2,
                formula = list(vector_x2 ~ s(latitude, longitude, bs = 'sos', k=60),
                               vector_y2 ~ s(latitude, longitude, bs = 'sos', k=60)),
                family = mvn(d=2))


starting_pos = expand.grid(
    longitude = seq(-10, -120, length = 30),
    latitude = 15
)

pos = lapply(1:nrow(starting_pos), \(x) {
    pos = starting_pos[x, ]
    newpos = make_projection(pos = pos, mod, n = 100)
    newpos
})
pos = bind_rows(pos, .id = 'id')
pos = pos %>% filter(longitude < 0, latitude < 60)

dir.create('img/analysis/predicted_paths', showWarnings = FALSE, recursive = TRUE)
for (i in 1:100) {
    cat(i, '\r')
    png(filename = sprintf('img/analysis/predicted_paths/frame_%05d.png', i), width = 1920, height = 1080, units = 'px', res = 200, bg='gray10')
    pl = ggplot() +
        geom_sf(data = atlantic, color = 'white', fill = NA) +
        geom_path(data = pos %>% group_by(id) %>% slice_head(n=i), aes(x = longitude, y = latitude, color = 'Projected Path', group = id)) +
        xlab("Longitude") + ylab("Latitude") +
        ggtitle(label = "Projected storm paths") +
        theme(
            panel.border = element_rect(colour = 'white',  linewidth = 1, fill = NA),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill  = "transparent", color = NA),
            legend.title = element_blank(),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent", colour = 'white'),
            legend.key = element_rect(fill = "transparent"),
            text = element_text(size = 15, family = 'consolas', color = 'white'),
            axis.text = element_text(size = 15, family = 'consolas', color = 'white'),
            axis.ticks.x = element_blank()
        )
    print(pl)
    dev.off()
}

# Acceleration --------


path2 = path %>%
    filter(hurricane_year > 2000) %>%
    group_by(hurricane_id) %>%
    mutate(
        accel = (average_speed_mph - lag(average_speed_mph)) / delta_t_hours
    ) %>%
    ungroup()

# accel_model_full = gam(
#     formula = accel ~
#         s(latitude, longitude, bs = "sos", k = 60) +
#         over_land +
#         maximum_sustained_wind_in_knots +
#         minimum_pressure_in_millibars +
#         status_of_system
#         ,
#     data = path2, weights = delta_t_hours)
#
# summary(accel_model_full)

accel_model = gam(formula = accel ~ s(latitude, longitude, bs = "sos", k = 60) + over_land, data = path2, weights = delta_t_hours)
summary(accel_model)

bbox = st_bbox(atlantic)
grid = expand.grid(longitude = seq(bbox['xmin'], bbox['xmax'], length = 200), latitude = seq(bbox['ymin'], bbox['ymax'], length=200))
grid = add_overland_indicator(grid)
grid$fill = predict(accel_model, grid)

pl = ggplot(grid) +
    geom_tile(aes(x = longitude, y = latitude, fill = fill)) +
    geom_sf(data = st_geometry(atlantic), fill = NA, color = 'white') +
    theme_bw() +
    scale_fill_gradient2(mid = 'black', name = 'Accel (mph/h)') +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(label = "Average Storm Acceleration", subtitle = "fit by GAM model with splines-on-sphere smoothing")

png(filename = 'img/analysis/avg-storm-accel.png', width=1920, height=1080, res=200, bg='transparent')
pl = pl + theme(
    panel.border = element_rect(colour = 'white',  linewidth = 1, fill = NA),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill  = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent", colour = 'white'),
    legend.key = element_rect(fill = "transparent"),
    text = element_text(size = 15, family = 'consolas', color = 'white'),
    axis.text = element_text(size = 15, family = 'consolas', color = 'white'),
    axis.ticks.x = element_blank()
)
print(pl)
dev.off()
