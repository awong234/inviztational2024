

bearing = function(long1, long2, lat1, lat2) {
    atan2(sin(long2-long1)*cos(lat2), cos(lat1)*sin(lat2) - sin(lat1)*cos(lat2)*cos(long2-long1))
}

to_rad = function(x) {
    pi * x / 180
}
to_deg = function(x) {
    180 * x / pi
}
to_naut_mi = function(mi) {
    mi/1.15078
}
to_mi = function(naut_mi) {
    naut_mi*1.15078
}

add_motion_dynamics = function(data) {
    if (nrow(data) == 1) {
        data$distance_traveled_miles = NA
        data$delta_t_hours = NA
        data$average_speed_mph = NA
        data$bearing = NA
        data$bearing_degrees = NA
    } else {
        dists = fields::rdist.earth.vec(
            as.matrix(data[-1, c('longitude', 'latitude')]),
            as.matrix(data[-nrow(data), c('longitude', 'latitude')])
        )
        data$distance_traveled_miles = c(0, dists)
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

next_position = function(lat, lng, bearing_deg, distance_mi) {
    R = 6371; # Earth Radius in Km
    distance = 1.609344 * distance_mi # Km
    lat = pi / 180 * lat
    lng = pi / 180 * lng
    bearing = pi / 180 * bearing_deg
    lat2 = asin(sin(lat) * cos(distance / R) + cos(lat) * sin(distance / R) * cos(bearing));
    lon2 = lng + atan2(sin(bearing) * sin(distance / R) * cos( lat ), cos(distance / R) - sin(lat) * sin(lat2));
    return (c(180/pi * lat2 , 180/pi*lon2));
}


interpolate_path = function(storm, interval_hours) {
    interval_seconds = interval_hours * 60 * 60
    N = nrow(storm)
    if (N == 1) return(NULL)
    new_points = list()
    for (i in 1:(N-1)) {
        first = storm[i, ]
        second = storm[i+1, ]
        n_interval = ceiling(storm$delta_t_hours[i+1] / interval_hours)
        if (i == (N-1)) n_interval = n_interval + 1
        placeholder = numeric(n_interval)
        new_lat_lon = data.frame(
            datetime = placeholder,
            latitude = placeholder,
            longitude = placeholder,
            distance_traveled_miles = placeholder
        )
        distance = storm$distance_traveled_miles[i+1]
        distance_per = distance / n_interval
        next_point = next_position(first$latitude, first$longitude, second$bearing_degrees, distance_per)
        new_lat_lon$latitude[1]  = next_point[1]
        new_lat_lon$longitude[1] = next_point[2]
        new_lat_lon$datetime[1]  = first$datetime
        if (n_interval > 1) {
            for (j in 2:n_interval) {
                next_point = next_position(next_point[1], next_point[2], second$bearing_degrees, distance_per)
                new_lat_lon$latitude[j]  = next_point[1]
                new_lat_lon$longitude[j] = next_point[2]
                new_lat_lon$datetime[j]  = new_lat_lon$datetime[j-1] + interval_seconds
            }
        }
        new_lat_lon$distance_traveled_miles = distance_per
        new_lat_lon$delta_t_hours = interval_hours
        new_lat_lon$average_speed_mph = distance_per / interval_hours
        new_lat_lon$ogdatetime = first$datetime
        new_points[[i]] = new_lat_lon
    }
    new_points = bind_rows(new_points)
    new_points$datetime = as.POSIXct(new_points$datetime, origin = '1970-01-01')
    new_points$cumulative_hours = cumsum(new_points$delta_t_hours)
    new_points$cumulative_dist_mi = cumsum(new_points$distance_traveled_miles)
    new_points$hurricane_id = storm$hurricane_id[1]
    return(new_points)
}

interpolate_paths = function(storms, interval_hours) {
    storms = split(storms, storms$hurricane_id)
    storms = lapply(storms, \(x) interpolate_path(x, interval_hours))
    storms = bind_rows(storms)
    storms
}

add_overland_indicator = function(path) {
    world = spData::world
    path_sf = sf::st_as_sf(path, coords = c('longitude', 'latitude'))
    sf::st_crs(path_sf) = sf::st_crs(world)
    over_land = !is.na(as.numeric(sf::st_intersects(path_sf, world)))
    path$over_land = over_land
    path
}
