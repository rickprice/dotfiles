#!/usr/bin/env python3

import argparse
import datetime
import errno
import glob
import os
from os.path import isfile
import sys
import subprocess


def create_file_list(CWD):
    """takes string as path, returns tuple(files,date)"""

    return [f for f in glob.iglob(os.path.join(CWD,'**','*')) if os.path.splitext(f)[1] in ext]

def discover_orphaned_control_files(files):
    """gets tuple(file,date) from create_file_list()"""

    fileCount=0
    for i in files:
        try:
            originalFilename=i
            (originalFilenameFile,_)=os.path.splitext(originalFilename)

            # This is the actual CR2 type file since we removed the extension
            existingLocation=os.path.join(CWD, originalFilenameFile)
            if not os.path.isfile(existingLocation):
                print(f"{originalFilename}")
                fileCount=fileCount+1

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
    print("Discovered %i orphaned control files" % discover_orphaned_control_files(files))
