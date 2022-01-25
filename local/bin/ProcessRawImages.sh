#! /usr/bin/sh

# Fail script if any command fails
set -e

## keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
## echo an error message before exiting
trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

# Setup where everything goes
DOWNLOADS_DIR=~/Downloads
GOOGLE_TAKEOUT_DIR=./Takeout
LOCATION_HISTORY_FILE_GPX="$GOOGLE_TAKEOUT_DIR/Location History/Location History.gpx"
LOCATION_HISTORY_FILE_JSON="$GOOGLE_TAKEOUT_DIR/Location History/Records.json"
LOCATION_HISTORY_FILE_KML="$GOOGLE_TAKEOUT_DIR/Location History/Location History.kml"
WORKING_DIR=~/Documents/Personal/PhotosFromCamera

cd "$WORKING_DIR"

function process_photo_directory() {
    local directory="$1"
    local dropbox_location="$2"
    local directory_location_data="${directory}_loc_data"
    local directory_dropbox="${directory}_dropbox"

    echo "Clear out directories if we can ${directory}"
    rm -rf "$directory_location_data"
    [ -d $directory_dropbox ] && rm -r "$directory_dropbox"

    echo "Copy pictures to be geolocated ${directory}"
    mkdir -p "$directory_location_data"
    cp -r $directory/* "$directory_location_data"

    echo "Add location data $directory"
    # [ "$(ls -A $directory)" ] && exiftool -geotag "$LOCATION_HISTORY_FILE_KML" '-geotime<${DateTimeOriginal}-04:00' . -api GeoMaxIntSecs=1800 -overwrite_original_in_place $directory_location_data/*
    [ "$(ls -A $directory)" ] && exiftool -v5 -geotag "$LOCATION_HISTORY_FILE_KML" '-geotime<${DateTimeOriginal}-05:00' . -api GeoMaxIntSecs=1800 -overwrite_original_in_place $directory_location_data/*

    # Sort the processed files in the location directory
    pushd .
    cd $directory_location_data
    SortFilesInDirectory --extension CR2 --extension JPG
    popd

    # echo "Copy pictures to dropbox directory ${directory}"
    # mkdir -p "$directory_dropbox"
    # cp -R $directory_location_data/* "$directory_dropbox"

    # # echo "Move pictures from [$directory_dropbox] to dropbox location [$dropbox_location] $directory"
    # rclone move "${directory_dropbox}" "Dropbox:${dropbox_location}"
}

echo Setup Google Takeout data
rm -rf "$GOOGLE_TAKEOUT_DIR"
if [[ $(find $DOWNLOADS_DIR -maxdepth 1 -name 'takeout-*' -printf c | wc -c) == "1" ]]; then
    tar -xzf $DOWNLOADS_DIR/takeout-*
    location-history-json-converter -f kml -s 2021-01-01 "$LOCATION_HISTORY_FILE_JSON" "$LOCATION_HISTORY_FILE_KML"
    # location-history-json-converter -f gpxtracks -s 2021-11-01 "$LOCATION_HISTORY_FILE_JSON" "$LOCATION_HISTORY_FILE_GPX"
else
    figlet Too many or too few takeout location zip files
    exit -1
fi

process_photo_directory "$WORKING_DIR/CanonT7i"  "Pictures/CanonRawPhotos"
# process_photo_directory "$WORKING_DIR/CanonSX540HS"  "Pictures/CanonSX540HS"


# Start terminal to watch the pipeline
# terminal -e watch --interval 120 "ls --format=single-column $DROPBOX_UPLOAD_DIR $ORIGINAL_GOOGLE_PHOTOS_RAWS_UPLOAD_DIR $DROPBOX_UPLOAD_DIR_ERIC | wc --lines | figlet -k"

# figlet Step 4: Copy original RAW files to staging
# rsync --ignore-missing-args $ORIGINAL_RAWS_DIR/* $DROPBOX_UPLOAD_DIR
# rsync --ignore-missing-args $ORIGINAL_CANON_POWERSHOT_DIR/* $DROPBOX_UPLOAD_DIR
# rsync --ignore-missing-args $ORIGINAL_ERIC_CAMERA_DIR/* $DROPBOX_UPLOAD_DIR_ERIC
# rsync --ignore-missing-args $ORIGINAL_GOOGLE_PHOTOS_RAWS/* $ORIGINAL_GOOGLE_PHOTOS_RAWS_UPLOAD_DIR

# figlet Step 5: Geotag photos
# [ "$(ls -A $DROPBOX_UPLOAD_DIR)" ] && exiftool -geotag "$LOCATION_HISTORY_FILE_KML" '-geotime<${DateTimeOriginal}-04:00' . -api GeoMaxIntSecs=1800 -overwrite_original_in_place $DROPBOX_UPLOAD_DIR/*
# [ "$(ls -A $DROPBOX_UPLOAD_DIR_ERIC)" ] && exiftool -geotag "$LOCATION_HISTORY_FILE_KML" '-geotime<${DateTimeOriginal}-04:00' . -api GeoMaxIntSecs=1800 -overwrite_original_in_place $DROPBOX_UPLOAD_DIR_ERIC/*
# [ "$(ls -A $ORIGINAL_GOOGLE_PHOTOS_RAWS_UPLOAD_DIR)" ] && exiftool -geotag "$LOCATION_HISTORY_FILE_KML" '-geotime<${DateTimeOriginal}-04:00' . -api GeoMaxIntSecs=1800 -overwrite_original_in_place $ORIGINAL_GOOGLE_PHOTOS_RAWS_UPLOAD_DIR/*

# figlet Step 6: Convert photos going to Google to JPEG
# Convert files in batch mode using default parameters
# [ "$(ls -A $ORIGINAL_GOOGLE_PHOTOS_RAWS_UPLOAD_DIR)" ] && rawtherapee-cli -d -s -o $GOOGLE_PHOTOS_JPEG_UPLOAD_DIR -c $ORIGINAL_GOOGLE_PHOTOS_RAWS_UPLOAD_DIR

# Remember we may need to enter password
# figlet -k DropBox, You may need to enter passwword

# figlet Step 7: Upload RAW files to Dropbox
# rclone move "$DROPBOX_UPLOAD_DIR" Dropbox:Pictures/CanonRawPhotos
# rclone move "$ORIGINAL_GOOGLE_PHOTOS_RAWS_UPLOAD_DIR" Dropbox:Pictures/CanonRawPhotos
# rclone move "$DROPBOX_UPLOAD_DIR_ERIC" Dropbox:Pictures/EricCameraPhotos

# figlet Step 8: You now need to manually upload the photos going to Google Photos
# Remember we may need to enter password
# figlet -k Google Photos, You may need to enter passwword

# Move JPEG files to Google Photos
# echo Moving JPEG photos to Google Photos
# rclone move "$GOOGLE_PHOTOS_JPEG_UPLOAD_DIR" GooglePhotos:upload
