#!/usr/bin/env python3

import argparse
import datetime
import errno
import os
import sys
import subprocess


def create_file_list(CWD):
    """takes string as path, returns tuple(files,date)"""

    files_with_mtime = []
    for filename in [f for f in os.listdir(CWD) if os.path.splitext(f)[1] in ext]:
        # First check if we can get the EXIF created date
        # exiftool -s -s -s -CreateDate -d "%Y-%m-%d"  IMG_0197.CR2
        exif_created_date = subprocess.run(['exiftool', '-s','-s','-s','-CreateDate','-d','%Y-%m-%d',filename], stdout=subprocess.PIPE).stdout.decode('utf-8')
        if exif_created_date:
            canonical_date=str.strip(exif_created_date)
        else:
            canonical_date=datetime.datetime.fromtimestamp(os.stat(filename).st_ctime).strftime( "%Y-%m-%d")

        # print(f"Canonical date for {filename} is {canonical_date}")
        files_with_mtime.append(
            (
                filename,
                canonical_date
            )
        )

    return files_with_mtime


def create_directories(files):
    """takes tuple(file,date) from create_file_list()"""

    m = []
    for i in files:
        m.append(i[1])
    for i in set(m):
        try:
            os.makedirs(os.path.join(CWD, i))
        except OSError as exception:
            if exception.errno != errno.EEXIST:
                raise


def move_files_to_folders(files):
    """gets tuple(file,date) from create_file_list()"""

    fileCount=0
    for i in files:
        try:
            originalFilename=i[0]
            (originalFilenameFile,originalFilenameExt)=os.path.splitext(originalFilename)
            canonicalDate=i[1]
            newFilename=f"{originalFilenameFile}-{canonicalDate}{originalFilenameExt}"
            existingLocation=os.path.join(CWD, originalFilename)
            newLocation=os.path.join(CWD, (canonicalDate + "/" + newFilename))
            print(f"Will move:\n{existingLocation}\nto:\n{newLocation}\n")
            os.rename(existingLocation,newLocation)
            fileCount=fileCount+1

            # Now check for XMP files
            xmpOriginalFilename=f"{originalFilename}.xmp"
            xmpNewFilename=f"{newFilename}.xmp"
            xmpExistingLocation=os.path.join(CWD, xmpOriginalFilename)
            xmpNewLocation=os.path.join(CWD, (canonicalDate + "/" + xmpNewFilename))
            if os.path.exists(xmpExistingLocation):
                print(f"Will ALSO move:\n{xmpExistingLocation}\nto:\n{xmpNewLocation}\n")
                fileCount=fileCount+1
                os.rename(xmpExistingLocation,xmpNewLocation)
        except Exception as e:
            raise
    return fileCount


if __name__ == "__main__":

    parser = argparse.ArgumentParser(prog=sys.argv[0], usage="%(prog)s [options]")
    parser.add_argument(
        "-e",
        "--extension",
        action="append",
        help="File extensions to match",
        required=True,
    )
    args = parser.parse_args()

    ext = ["." + e for e in args.extension]
    print("Moving files with extensions:", ext)
    CWD = os.getcwd()
    files = create_file_list(CWD)
    create_directories(files)
    print("Moved %i files" % move_files_to_folders(files))
