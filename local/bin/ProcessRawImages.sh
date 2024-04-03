#! /usr/bin/bash

# Fail script if any command fails
set -e

# Echo commands
set -x

## keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
## echo an error message before exiting
trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

# Setup where everything goes
DOWNLOADS_DIR=~/Documents/Personal/Dropbox/FrederickDocuments/DropBoxDownloads
LOCATION_HISTORY_FILE_TGZ=$DOWNLOADS_DIR/takeout-*
LOCATION_HISTORY_FILE_KML=$DOWNLOADS_DIR/GoogleTakeoutLocations.kml
DAYS_LOCATION_RECORDS_TO_USE=60
WORKING_DIR=~/Documents/Personal/PhotosFromCamera

# ARTIST="Tamara Price"
ARTIST="Frederick Price"
COMPANY="PricePixel Photography"

cd "$WORKING_DIR"

function setup_google_location_data() {
    echo Setup Google Takeout data
    trash -f "$LOCATION_HISTORY_FILE_KML"
    # Check if there is more than one takeout-* file, if so, then user needs to delete the old ones
    if [[ $(find $DOWNLOADS_DIR -maxdepth 1 -name 'takeout-*' -printf c | wc -c) == "1" ]]; then
        echo "...Starting Processing Location Data."
        glconverter --inputFile $LOCATION_HISTORY_FILE_TGZ --outputFile $LOCATION_HISTORY_FILE_KML --filterMoreThanDays=$DAYS_LOCATION_RECORDS_TO_USE
        echo "...Finished Processing Location Data."

    else
        figlet Too many or too few takeout location zip files
        echo "Location Checked: [" $LOCATION_HISTORY_FILE_TGZ "]"
        exit -1
    fi
}

function process_photo_directory() {
    local directory="$1"
    local dropbox_location="$2"
    local directory_location_data="${directory}_loc_data"
    local directory_dropbox="${directory}_dropbox"

    echo "Clear out directories if we can ${directory}"
    trash -f "$directory_location_data"
    [ -d $directory_dropbox ] && trash -f "$directory_dropbox"

    echo "Copy pictures to be geolocated ${directory}"
    mkdir -p "$directory_location_data"
    cp -r $directory/* "$directory_location_data"

    echo "Add location data $directory"
    exiftool -v5 -Copyright="All rights reserved. $COMPANY" -CopyrightNotice="All rights reserved. $COMPANY" -Rights="All rights reserved" -Artist="$ARTIST" -geotag "$LOCATION_HISTORY_FILE_KML" '-geotime<${DateTimeOriginal}-05:00' . -api GeoMaxIntSecs=1800 -overwrite_original_in_place $directory_location_data
    echo "Organize Images $directory_location_data"
    exiftool '-filename<DateTimeOriginal' -d "$directory_location_data/%Y-%m-%d/%Y-%m-%d_%%-c%%f.%%e" $directory_location_data
}

setup_google_location_data
process_photo_directory "$WORKING_DIR/Incoming" "Pictures/CanonRawPhotos"
# process_photo_directory "$WORKING_DIR/CanonSX540HS"  "Pictures/CanonSX540HS"
