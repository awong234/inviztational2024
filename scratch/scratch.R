library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mgcv)
library(sf)

hurr = readxl::read_excel('data/Copy of Atlantic & Pacific Hurricane Data.xlsx')
hurr = janitor::clean_names(hurr)
hurr = hurr %>%
    mutate(
        raw_date = as.Date(as.integer(raw_date), origin = '1899-12-30'),
        datetime = as.POSIXct(data_date_time, format = '%m/%d/%Y %H:%M')
    )

hurr = hurr %>% arrange(hurricane_id, datetime)
hurr = hurr %>% mutate(across(where(is.numeric), ~case_when(.x == -999 ~ NA, .default = .x)))

world = spData::world

sf_use_s2(FALSE)
atlantic = world %>% st_crop(xmin = -160, xmax = 0, ymin = 0, ymax = 84)
sf_use_s2(TRUE)

# # # # # # # # # # # # # # # #
############ Adding motion dynamics ############

# Distance traveled, and rates of travel
library(fields)

bearing = function(long1, long2, lat1, lat2) {
    atan2(sin(long2-long1)*cos(lat2), cos(lat1)*sin(lat2) - sin(lat1)*cos(lat2)*cos(long2-long1))
}

to_rad = function(x) {
    pi * x / 180
}
to_deg = function(x) {
    180 * x / pi
}

add_motion_dynamics = function(data) {
    if (nrow(data) == 1) {
        data$distance_traveled_miles = NA
        data$delta_t_hours = NA
        data$average_speed_mph = NA
        data$bearing = NA
        data$bearing_degrees = NA
    } else {
        dists = fields::rdist.earth(
            as.matrix(data[, c('longitude', 'latitude')]),
            as.matrix(data[(2:nrow(data)), c('longitude', 'latitude')])
        )
        dists = dists |> diag()
        data$distance_traveled_miles = c(NA, dists)
        delta_t = data$datetime[-1] - data$datetime[-nrow(data)]
        units(delta_t) = 'hours'
        data$delta_t_hours = c(NA, delta_t)
        data$average_speed_mph = data$distance_traveled_miles / data$delta_t_hours
        bearing_calc = bearing(to_rad(data$longitude[-nrow(data)]), to_rad(data$longitude[-1]), to_rad(data$latitude[-nrow(data)]), to_rad(data$latitude[-1]))
        data$bearing = c(NA, bearing_calc)
        data$bearing_degrees = to_deg(data$bearing)
    }
    data
}

hurrs = hurr %>% split(hurr$hurricane_id)
hurrs = lapply(hurrs, add_motion_dynamics)
hurrs[[1]]
hurr = bind_rows(hurrs)

hurr %>% group_by(hurricane_year) %>%
    summarize(mean(is.na(radius_of_maximum_wind_in_nautical_miles)))

hurr_agg = hurr %>% filter(lubridate::year(datetime) > 2000) %>%
    group_by(year = lubridate::year(datetime), hurricane_id, hurricane_name) %>%
    summarize(
        total_average_speed_mph = weighted.mean(average_speed_mph, w = delta_t_hours, na.rm = TRUE),
        total_speed_sd = sd(average_speed_mph, na.rm = TRUE),
        total_distance_traveled = sum(distance_traveled_miles, na.rm = TRUE),
        total_lifetime = sum(delta_t_hours, na.rm = TRUE),
        max_windspeed = max(maximum_sustained_wind_in_knots, na.rm = TRUE),
        avg_max_windspeed = mean(maximum_sustained_wind_in_knots, na.rm = TRUE),
        .groups = 'drop'
    )

hurr_agg %>%
    ggplot() +
    geom_point(aes(x = total_average_speed_mph, y = total_distance_traveled, color = total_lifetime)) +
    theme_bw() +
    scale_color_viridis_b() +
    NULL

hurr_agg %>% filter(year > 2010, total_average_speed_mph > 20 & total_distance_traveled > 1000) %>% arrange(desc(total_average_speed_mph))
hurr_agg |> filter(hurricane_name == 'OLGA', year == 2019)
hurr_agg |> filter(hurricane_name == 'IAN', year == 2022)
hurr_agg |> filter(hurricane_name == 'KATRINA', year == 2005)
hurr_agg |> filter(hurricane_name == 'IRMA', year == 2017)
hurr_agg |> filter(hurricane_name == 'DORIAN', year == 2019)



