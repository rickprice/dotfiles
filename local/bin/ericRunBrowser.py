#!/usr/bin/env python3

import subprocess
import logging
import os
from datetime import datetime, timedelta

waitTimeMinutes=20
badVideoLogFilename=os.path.expanduser("~/BadVideoLog.txt")
startBrowserLogFilename=os.path.expanduser("~/StartBrowserLog.txt")
badVideoBackground=os.path.expanduser("~/.dotfiles/wallpaper/no-monster-allowd-sign.jpg")
goodBackground=os.path.expanduser("~/.dotfiles/wallpaper/wallpaper.jpg")
browserCommand=["/usr/bin/firefox-developer-edition","https://www.youtube.com/results?search_query=power+rangers"]

# Setup logging
# logging.basicConfig(level=logging.INFO,encoding='utf-8',format='%(asctime)s|%(levelname)s|%(message)s')
logging.basicConfig(filename=startBrowserLogFilename,encoding='utf-8',format='%(asctime)s|%(levelname)s|%(message)s')


def stop_browser()->None:
    logging.info("Stopping browser")
    subprocess.run(["killall","-9","firefox"])

def start_browser()->None:
    logging.info("Starting browser")
    subprocess.Popen(browserCommand, close_fds=True)

def good_background()->None:
    logging.debug(f"Setting background to [{goodBackground}]")
    subprocess.run(["nitrogen","--set-scaled",goodBackground])

def bad_background()->None:
    logging.debug(f"Setting background to [{badVideoBackground}]")
    subprocess.run(["nitrogen","--set-scaled",badVideoBackground])

def successful_start()->None:
    logging.warning("Successful start")
    good_background()
    stop_browser()
    start_browser()

def unsuccessful_start()->None:
    logging.error("Unsuccessful start")
    bad_background()
    stop_browser()

def main()->None:
    logging.warning("Checking if we can run browser")

    cutoffTime=(datetime.now()-timedelta(minutes=waitTimeMinutes)).strftime("%Y-%m-%d %H:%M:%S,%f")
    logging.warning(f"Calculated cutoff time as [{cutoffTime}]")

    if os.path.isfile(badVideoLogFilename):
        # Check if the last warning was long enough ago
        with open(badVideoLogFilename, 'r') as file:
            logContents = file.readlines()
            for logLine in logContents:
                logging.debug(f"Found log line [{logLine}]")
                segments=logLine.strip().split("|")
                logging.debug(f"Found segments [{segments}]")
                if segments[1] == "ERROR":
                    logging.warning(f"Found log line time [{segments[0]}]")
                    if segments[0] >= cutoffTime:
                        unsuccessful_start()
                        return

    successful_start()

if __name__ == "__main__":
    main()
