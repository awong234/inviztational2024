#! /bin/bash

# Should edit the "~/.config/goes2go/config.toml" path so that the value is set
# as desired.

# Activate conda environment
conda activate goes2go

# First step is to process xl file into sqlite database
Rscript scripts/format_hurricane_data.R

# We can produce some of the image sets for the final product from this point already
Rscript scripts/make_all_storm_map.R
Rscript scripts/make_map_track.R
Rscript scripts/make_storm_profiles_charts.R
Rscript scripts/spatial_analysis.R

# We will need to handle the satellite imagery download and centering. This
# depends only on the first step creating the SQLite database.
# First just download all the requisite files
python scripts/goes2go-data-download.py
# Then do the centering and cropping
python scripts/format-storm-images.py

# We now need to compose each individual frame from the before-hand jobs
Rscript scripts/compose_image_frames.R

# Compose video frames into video
./scripts/make_videos.sh
