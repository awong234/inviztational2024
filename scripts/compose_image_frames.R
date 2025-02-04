library(magick)
library(RSQLite)
library(DBI)
library(dplyr)

con = dbConnect(SQLite(), 'data/data.db')

colors = yaml::read_yaml('colors.yml')

# Load storm paths
paths = dbGetQuery(con, 'select * from paths_interp where hurricane_id in (select ID from chosen_storms)')
properties = dbGetQuery(con, 'select hurricane_id, status_of_system, datetime from properties where hurricane_id in (select ID from chosen_storms)')
paths$datetime = as.POSIXct(paths$datetime, origin = '1970-01-01 00:00:00', tz = 'UTC')
paths$datetime_str = format(paths$datetime, "%Y-%m-%dT%H-%M-%S")
paths$over_land = as.logical(paths$over_land)
overall_max_speed = max(paths$average_speed_mph)
average_speed = weighted.mean(paths$average_speed_mph, w = paths$delta_t_hours)
paths = paths %>% left_join(properties, by = c("ogdatetime" = "datetime", "hurricane_id"))

# Filter up to 4000 MI
# paths = paths %>% filter(cumulative_dist_mi < 4005)

# Storm speed modifications
storm_paths = split(paths, paths$hurricane_id)
speed_breaks = seq(0,1,length.out=30)
storm_paths = lapply(storm_paths, \(path) {
    path_smooth = smooth.spline(x=path$datetime, y = path$average_speed_mph, spar = 0.15)
    path$smooth_speed_mph = fitted(path_smooth)
    path$p_max_speed = path$smooth_speed_mph / max(path$smooth_speed_mph)
    path$p_max_speed_d = cut(path$p_max_speed, breaks = speed_breaks)
    path
})
paths = bind_rows(storm_paths)
paths$p_omax_speed = paths$smooth_speed_mph / max(paths$smooth_speed_mph)
paths$p_omax_speed_d = cut(paths$p_omax_speed, breaks = speed_breaks)
storm_paths = split(paths, paths$hurricane_id)

# Create output directory
outdir = 'img/composite_frames/'
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
# List input directories
map_dir = 'img/map_animation/'
storm_dir = 'img/storm_centered/'

# List files in folders
# Storm frames
storm_folders = dir(storm_dir, full.names = TRUE)
ns = length(storm_folders)
storm_frames = lapply(storm_folders, \(x) {
    list.files(x, full.names = TRUE, pattern = '*.png')
})
names(storm_frames) = basename(storm_folders)
n_storm_frames = sapply(storm_frames, length)
# List map frames
map_frames = list.files(map_dir, full.names = TRUE, pattern = '*.png')
n_map_frames = length(map_frames)

# Max frames is full length of video
max_frames = max(n_storm_frames, n_map_frames)

# READ IN IMAGES ###############################################################

# Read in base image
base = image_read('img/base.png')
base_info = image_info(base)
BASE_W = base_info$width
BASE_H = base_info$height
START_X = BASE_W/5.101485
FINISH_X = BASE_W/1.814261 + START_X
LEN_X = FINISH_X - START_X
START_Y = BASE_H/2.869081
LANE_H = 120
NAMEBOX_R = 400

# Read megaman meters - https://www.spriters-resource.com/fullview/113678/ Credit: Mister Mike
megaman_w = 8
megaman_x = c(1, 10, 19, 28)
megaman_x = 1+(9*0:28)
megaman_y = 266
megaman_h = 56
geom = sprintf("%dx%d+%d+%d", megaman_w, megaman_h, megaman_x, megaman_y)
bars = image_read('img/megaman_health.png')
meters = lapply(geom, \(x) image_crop(bars, geometry = x) |> image_scale(sprintf('x%d', LANE_H)))
meters = rev(meters)

# Read in finish line flag
flag = image_read('img/Flag_Waving_Animation.webp') |> image_convert(format = 'png') |> image_scale('40%')

# FUNCTIONS ################################################

offset_geometry = function(x,y) {
    sprintf("+%0.0f+%0.0f", x, y)
}

# Read in terrain image
load_terrain = function() {
    # https://www.deviantart.com/johnnytzut96/art/Oasis-in-Desert-TileSet-and-Sprite-Sheet-1011976003
    terrain = image_read('img/terrain.png')
    water = image_crop(terrain, '84x84+133+16')

    water_collection = lapply(seq(0, 270, by = 90), \(x) image_rotate(water, x))

    # islandbr = image_crop(terrain, '84x84+133+249')
    # islandbl = image_crop(terrain, '84x84+249+249')
    # islandb  = image_append(c(islandbl, islandbr))
    # islandtr = image_crop(terrain, '84x84+610+133')
    # islandtl = image_crop(terrain, '84x84+16+249')
    # islandt  = image_append(c(islandtl, islandtr))
    # island = image_append(c(islandb, islandt), stack = TRUE) %>% image_scale('84')
    #
    # water_collection = c(water_collection, island)

    # Land pixels
    land_x = c(366, 482, 610,
               016, 133, 249, 366, 482, 610,
               016, 133, 249, 366)
    land_y = c(249, 249, 249,
               rep(366, 6),
               rep(482, 4)
    )
    land_collection = c()
    for (i in 1:length(land_x)) {
        cut = sprintf("84x84+%d+%d", land_x[i], land_y[i])
        land_collection = c(land_collection, image_crop(terrain, cut))
    }

    sample_water = function(size) {
        s = sample(1:length(water_collection), size = size, replace = TRUE)
        water_collection[s]
    }
    sample_land = function(size) {
        s = sample(1:length(land_collection), size = size, replace = TRUE)
        land_collection[s]
    }
    sample_terrain = function(path) {
        path$rleid = data.table::rleid(path$over_land)
        terrain_lengths = path %>% group_by(rleid, over_land) %>% summarize(n = sum(distance_traveled_miles), .groups = 'drop')
        out = c()
        for (i in 1:nrow(terrain_lengths)) {
            land = terrain_lengths$over_land[i]
            length = terrain_lengths$n[i] %>% ceiling()
            if (land) {
                out = c(out, sample_land(length))
            } else {
                out = c(out, sample_water(length))
            }
        }
        out
    }
    env = new.env()
    env$land = land_collection
    env$water = water_collection
    env$sample_water = sample_water
    env$sample_land = sample_land
    env$sample = sample_terrain
    env
}

terrain = load_terrain()

# Create terrain for each storm.
rugs = list()
dir.create('img/rugs', recursive = TRUE, showWarnings = FALSE)

for (name in names(storm_paths)) {
    outfile = sprintf('img/rugs/%s_rug.png', name)
    if (!file.exists(outfile)) {
        path = storm_paths[[name]]
        sampled_terrain = terrain$sample(path)
        rugs[[name]] = do.call(c, sampled_terrain) %>% image_append()
        rugs[[name]] %>% image_write(outfile)
    } else {
        rugs[[name]] = image_read(outfile)
    }
}

# Function to lay map
put_map = function(base, frame, offset_x, offset_y) {
    mapfile = map_frames[frame]
    map_frame = image_read(mapfile) |> image_scale(geometry="112%") |> image_border(color='white', geometry="5x5")
    map_frame_info = image_info(map_frame)
    map_w = map_frame_info$width
    map_h = map_frame_info$height
    # offset_x = BASE_W - (map_w*1.05)
    # offset_y = BASE_H / 2
    offset_x = BASE_W - (map_w) - 5
    offset_y = 5
    map_offset = offset_geometry(offset_x, offset_y)
    img = image_composite(base, map_frame, offset = map_offset)
    img
}

# Function to lay storms
put_storms = function(img, frame, buffer, storm_offset_x, rug_crop_offset_x, speed, speed_bucket, distance, status) {
    storms = lapply(storm_frames, \(x) {
        # Ensures that even after the storm is dead the last frame is visible
        file = x %>% head(frame) %>% tail(1)
        image_read(file) |> image_scale(geometry = sprintf('x%d', LANE_H))
    })
    storms = do.call(c, args=storms)
    storm_dim = image_info(storms)
    storm_heights = storm_dim$height
    storm_offsets_y = Reduce(f = \(x,y) x + y, storm_heights, accumulate = TRUE, init = 0)
    storm_offsets_y = storm_offsets_y[-length(storm_offsets_y)]
    storm_height_total = storm_offsets_y[ns] + storm_heights[ns] - storm_offsets_y[1]
    storm_offsets_y = storm_offsets_y + START_Y
    storm_offsets_y = storm_offsets_y + (buffer * 0:4)
    storm_offsets = offset_geometry(storm_offset_x, storm_offsets_y)
    # Rug offset ON PAGE
    rug_height = image_info(rugs[[1]])$height
    rug_offsets = offset_geometry(START_X, storm_offsets_y+(rug_height/5))
    # Speed offsets
    speed_offsets            = offset_geometry(NAMEBOX_R - 12, storm_offsets_y)
    speed_text_offsets       = offset_geometry(NAMEBOX_R - 259, storm_offsets_y + storm_heights - 80)
    # Distance text
    dist_text_offsets        = offset_geometry(NAMEBOX_R - 102, storm_offsets_y + storm_heights - 30)
    # Status text
    status_text_offsets      = offset_geometry(NAMEBOX_R - 341, storm_offsets_y + storm_heights - 80)
    for (s in 1:ns) {
        rug_frame = rugs[[s]]
        # This offset is for cropping the image itself, so that it "moves" across the screen.
        # rug_frame = rug_frame %>% image_crop(sprintf('%0.0fx%0.0f+%0.0f', FINISH_X - START_X - 19, 0, rug_crop_offset_x[s]))
        rug_frame = rug_frame %>% image_crop(sprintf('%0.0fx%0.0f+%0.0f', BASE_W - START_X - 19, 0, rug_crop_offset_x[s]))
        img = image_composite(img, rug_frame, offset = rug_offsets[s])
        img = image_composite(img, storms[s], offset = storm_offsets[s])
        # Speed meters
        meter = meters[[speed_bucket[s]]]
        # Annotate speed -- meter
        img = image_composite(img, meter, offset = speed_offsets[s])
        # Annotate speed -- text
        img = image_annotate(
            img,
            text = sprintf("%5.2f MPH", speed[s]),
            location = speed_text_offsets[s],
            color = 'gray90',
            size = 50,
            font = "Consolas",
            style = 'italic',
            weight = 700,
            boxcolor = colors[s] |> colorspace::darken(0.9)
        )
        # Annotate distance traveled - text
        img = image_annotate(
            img,
            text = sprintf("%4.0f mi.", distance[s]),
            location = dist_text_offsets[s],
            color = 'gray90',
            size = 20,
            font = "Consolas",
            style = 'italic',
            weight = 700,
            boxcolor = colors[s] |> colorspace::darken(0.9)
        )
        # Annotate storm type
        img = image_annotate(
            img,
            text = sprintf("%s", status[s]),
            location = status_text_offsets[s],
            color = 'gray90',
            size = 50,
            font = "Consolas",
            style = 'italic',
            weight = 700,
            boxcolor = colors[s] |> colorspace::darken(0.9)
        )
    }
    img
}

put_flag = function(img, frame, offset_x, offset_y) {
    # Create first flag
    idx = (frame)%%length(flag)
    if (idx == 0) idx = length(flag)
    fl = flag[idx]
    offset = offset_geometry(offset_x, offset_y)
    # Create second flag; offset index so that they wave differently
    idx2 = (frame+5)%%length(flag)
    if (idx2 == 0) idx2 = length(flag)
    fl2 = flag[idx2]
    offset2 = offset_geometry(offset_x, offset_y+575)
    # Place both flags
    img = image_composite(img, fl, offset=offset)
    img = image_composite(img, fl2, offset=offset2)
}



compose_image = function(frame, save = TRUE) {
    # Draw Storms
    current_location = storm_paths %>% lapply(\(x) x %>% slice_head(n = frame) %>% tail(1))
    storm_offsets_x   = sapply(current_location, \(x) (x$cumulative_dist_mi / 4000) * LEN_X) + START_X
    rug_crop_offset_x = sapply(current_location, \(x) (x$cumulative_dist_mi)*84)
    speed_bucket      = sapply(current_location, \(x) x$p_omax_speed_d)
    speed             = sapply(current_location, \(x) x$smooth_speed_mph)
    distance          = sapply(current_location, \(x) x$cumulative_dist_mi)
    status            = sapply(current_location, \(x) x$status_of_system)
    img = put_storms(
        base,
        frame,
        buffer = 3,
        storm_offset_x = storm_offsets_x,
        rug_crop_offset_x = rug_crop_offset_x,
        speed_bucket = speed_bucket,
        speed = speed,
        distance = distance,
        status = status
    )

    # Draw flag
    img = put_flag(img, frame, FINISH_X*1.01, START_Y*0.98)

    # Draw map -- draw after storms so it's always on top
    img = put_map(img, frame)

    # Output file
    if (save) image_write(img, outfile) else print(img)
    img
}

# FRAME CREATION ##############################

DEBUG = F
OVERWRITE = T
# Create images
for (frame in 1:max_frames) {
    cat(frame, '\r')
    # Set up output directory
    outfile = file.path(outdir, sprintf('frame_%05d.png', frame))
    if (DEBUG) {
        compose_image(frame, save = FALSE)
        break
    } else {
        if (!OVERWRITE && file.exists(outfile)) next
        compose_image(frame, save = TRUE)
    }
}

