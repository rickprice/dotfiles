#!/usr/bin/env python3

import argparse
import datetime
import errno
import glob
import os
from os.path import isfile
import re
import sys
import subprocess

image_file_extensions=['jpg','JPG','cr2','CR2','mpg','MPG']

def create_file_list(CWD):
    """takes string as path, returns tuple(files,date)"""

    return [f for f in glob.iglob(os.path.join(CWD,'**','*')) if os.path.splitext(f)[1] in ext]

def discover_and_fix_orphaned_control_files(files):

    fileCount=0
    for controlFileFilename in files:
        try:
            (controlFileFilenameFile,controlFileFilenameExtension)=os.path.splitext(controlFileFilename)


            # Look for exact match on image file
            existingLocation=os.path.join(CWD, controlFileFilenameFile)
            if not os.path.isfile(existingLocation):
                print(f"{controlFileFilename}")
                fileCount=fileCount+1

                # Look for possible CR2 files that it could go with
                (imageFilename,imageFileExtension)=os.path.splitext(os.path.basename(controlFileFilenameFile))

                # print(f"Originally checking for this name: {imageFilename}")

                # Check if the control file is a duplicate orphaned
                if re.fullmatch(r"[I_]MG_\d{4}_\d+",imageFilename):
                    imageFilename = re.sub(r"([I_]MG_\d{4})_\d+",r"\1",imageFilename)
                    # Okay, need to fix the filename
                    # print(f"Duplicate control file, fixing name to {imageFilename}")

                # print(f"Looking for an image file with this name: {imageFilename}")
                

                checkFiles = [f for f in glob.iglob(os.path.join(CWD,"**",f"{imageFilename}*{imageFileExtension}"))]
                # Check for easy match
                if len(checkFiles) == 1:
                    correctedFilePath=f"{checkFiles[0]}{controlFileFilenameExtension}"
                    print(f"Fixing {controlFileFilename}\nto: {correctedFilePath}")
                    # os.rename(controlFileFilename,correctedFilePath)

                else:
                    # Now remove any matches that couldn't happen because the images are after the control file
                    secondaryCheckFiles=[f for f in checkFiles if f < controlFileFilename]
                    if len(secondaryCheckFiles)==1:
                        correctedFilePath=f"{secondaryCheckFiles[0]}{controlFileFilenameExtension}"
                        print(f"Secondary Fixing {controlFileFilename}\nto: {correctedFilePath}")
                        # os.rename(controlFileFilename,correctedFilePath)
                    else:
                        if len(checkFiles) == 0:
                            print(f"No matches found, looking for duplicate image control files")
                            print(f"Original filename path {controlFileFilenameFile}")
                            print(f"Original filename {controlFileFilename}")
                        else:
                            list.sort(checkFiles)
                            list.reverse(checkFiles)
                            correctedFilePath=f"{checkFiles[0]}{controlFileFilenameExtension}"
                            print(f"Tertiary Fixing {controlFileFilename}\nto: {correctedFilePath}")
                            os.rename(controlFileFilename,correctedFilePath)


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
    print("Testing control files with extensions:", ext)
    CWD = os.getcwd()
    files = create_file_list(CWD)
    print("Discovered %i orphaned control files" % discover_and_fix_orphaned_control_files(files))
