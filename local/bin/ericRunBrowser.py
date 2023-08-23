#!/usr/bin/env python3

import subprocess
import logging
import os
from datetime import datetime
import datetime

waitTimeMinutes=30
badVideoLogFilename=os.path.expanduser("~/BadVideoLog.txt")
badVideoBackground=os.path.expanduser("~/.dotfiles/wallpaper/no-monster-allowd-sign.jpg")
goodBackground=os.path.expanduser("~/.dotfiles/wallpaper/no-monster-allowd-sign.jpg")
browserCommand="/usr/bin/firefox-developer-edition"

# Setup logging
logging.basicConfig(filename=badVideoLogFilename,encoding='utf-8',format='%(asctime)s:%(levelname)s:%(message)s')
# logging.basicConfig(level=logging.INFO,encoding='utf-8',format='%(asctime)s:%(levelname)s:%(message)s')


def stop_browser()->None:
    logging.info("Stopping browser")
    subprocess.run(["killall","-9","firefox"])

def start_browser()->None:
    logging.info("Starting browser")
    subprocess.Popen(browserCommand, close_fds=True)

def good_background()->None:
    logging.info(f"Setting background to [{goodBackground}]")
    subprocess.run(["nitrogen","--set-scaled",goodBackground])

def bad_background()->None:
    logging.info(f"Setting background to [{badVideoBackground}]")
    subprocess.run(["nitrogen","--set-scaled",badVideoBackground])

def successful_start()->None:
    good_background()
    start_browser()

def unsuccessful_start()->None:
    bad_background()

def main()->None:
    logging.info("Checking if we can run browser")
    cutoffTime=(datetime.now()+datetime.timedelta(minutes=waitTimeMinutes)).strftime("%Y-%m-%d %H:%M:%S,%f")

    if os.path.isfile(badVideoLogFilename):
        # Check if the last warning was long enough ago
        with open(badVideoLogFilename, 'r') as file:
            logContents = file.readlines()
            for logLine in logContents:
                segments=logLine.strip().split(":")
                if segments[1] == "ERROR":
                    if segments[0] <= cutoffTime:
                        unsuccessful_start()
                        return

        successful_start()

if __name__ == "__main__":
    main()
