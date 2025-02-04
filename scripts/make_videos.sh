# For files with no easy sequential pattern, like the hurricane images. This
# could have been fixed upstream by emitting file names with an increasing
# numeric index.
function make_video() {
    # cat $1/*.png | ffmpeg  -f image2pipe -r 60 -i -  -vf pad="width=ceil(iw/2)*2:height=ceil(ih/2)*2" -c:v libx264 -pix_fmt yuv420p $2
    cat $1/*.png | ffmpeg  -f image2pipe -r 60 -i -  -vf crop="trunc(iw/2)*2:trunc(ih/2)*2" -c:v libx264 -pix_fmt yuv420p $2
}

# For final video including audio
function make_video_audio() {
    ffmpeg -framerate 60 -i $1 -i $2 -vf "pad=width=ceil(iw/2)*2:height=ceil(ih/2)*2" -c:v libx264 -pix_fmt yuv420p -c:a copy -shortest $3
}

# For videos without audio
function make_video2() {
    ffmpeg -framerate 60 -i $1 -vf "pad=width=ceil(iw/2)*2:height=ceil(ih/2)*2" -c:v libx264 -pix_fmt yuv420p $2
}

# Final videos
# make_video_audio "img/composite_frames/frame_%05d.png" "audio/big-blue.mp3" video/race.mp4
make_video_audio "img/composite_frames/frame_%05d.png" "audio/fake-fzero2.mp3" video/race.mp4
make_video2 "img/map_animation/frame_%05d.png" "video/map.mp4"
make_video2 "img/analysis/all_storm_map/frame_%05d.png" "video/map.mp4"
make_video2 "img/analysis/predicted_paths/frame_%05d.png" "video/predicted_paths.mp4"

# Videos of storms -- individually
make_video "img/storm_centered/AL052019" "video/dorian.mp4"
make_video "img/storm_centered/AL092022" "video/ian.mp4"
make_video "img/storm_centered/AL112017" "video/irma.mp4"
make_video "img/storm_centered/AL152017" "video/maria.mp4"
make_video "img/storm_centered/AL172019" "video/olga.mp4"

# Copy videos to website directory
cp video inviztational2024 -r
cp audio inviztational2024 -r
# Copy analytical images to directory
find img/analysis -maxdepth 1 -type f -exec cp {} inviztational2024/analysis \;
