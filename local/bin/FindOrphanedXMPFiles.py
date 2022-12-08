#!/usr/bin/env python3

import argparse
import datetime
import errno
import glob
import os
from os.path import isfile
import sys
import subprocess

image_file_extensions=['jpg','JPG','cr2','CR2','mpg','MPG']

def create_file_list(CWD):
    """takes string as path, returns tuple(files,date)"""

    return [f for f in glob.iglob(os.path.join(CWD,'**','*')) if os.path.splitext(f)[1] in ext]

def discover_and_fix_orphaned_control_files(files):

    fileCount=0
    for i in files:
        try:
            originalFilename=i
            (originalFilenameFile,originalFilenameExtension)=os.path.splitext(originalFilename)

            # This is the actual CR2 type file since we removed the extension
            existingLocation=os.path.join(CWD, originalFilenameFile)
            if not os.path.isfile(existingLocation):
                print(f"{originalFilename}")
                fileCount=fileCount+1

                # Look for possible CR2 files that it could go with
                (imageFilename,imageFileExtension)=os.path.splitext(os.path.basename(originalFilenameFile))
                checkFiles = [f for f in glob.iglob(os.path.join(CWD,"**",f"{imageFilename}*{imageFileExtension}"))]
                if len(checkFiles) == 1:
                    correctedFilePath=f"{checkFiles[0]}{originalFilenameExtension}"
                    print(f"Fixing {originalFilename}\nto: {correctedFilePath}")
                    os.rename(originalFilename,correctedFilePath)

                else:
                    secondaryCheckFiles=[f for f in checkFiles if f < originalFilename]
                    if len(secondaryCheckFiles)==1:
                        correctedFilePath=f"{secondaryCheckFiles[0]}{originalFilenameExtension}"
                        print(f"Secondary Fixing {originalFilename}\nto: {correctedFilePath}")
                        os.rename(originalFilename,correctedFilePath)
                    else:
                        print(f"Possible matches: {checkFiles}")


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
