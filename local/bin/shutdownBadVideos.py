#!/usr/bin/env python3

import subprocess
import time
import re
from typing import Optional
import logging
import os

tooManyBadVideos=3
badVideoLogFilename=os.path.expanduser("~/BadVideoLog.txt")
whitelistFilename=os.path.expanduser("~/.config/shutdownBadVideos/WhiteList.txt")
blacklistFilename=os.path.expanduser("~/.config/shutdownBadVideos/BlackList.txt")
badVideoBackground=os.path.expanduser("~/.dotfiles/wallpaper/no-monster-allowd-sign.jpg")
browserCommand="/usr/bin/firefox-developer-edition"

# Setup logging
logging.basicConfig(filename=badVideoLogFilename,encoding='utf-8',format='%(asctime)s:%(levelname)s:%(message)s')
# logging.basicConfig(level=logging.INFO,encoding='utf-8',format='%(asctime)s:%(levelname)s:%(message)s')


def does_string_match_list(list_to_check: list[str],string_to_check: str)->Optional[str]:
    logging.debug(f"Checking string [{string_to_check}]")
    for to_check in list_to_check:
        to_check = to_check.strip()
        logging.debug(f"Checking with regex [{to_check}]")
        if re.search(to_check,string_to_check,re.IGNORECASE|re.VERBOSE):
            logging.debug(f"Found match with regex [{to_check}]")
            return to_check

    logging.debug("Found no match")
    return None

def stop_browser()->None:
    logging.info("Stopping browser")
    subprocess.run(["killall","-9","firefox"])

def start_browser()->None:
    logging.info("Starting browser")
    subprocess.Popen(browserCommand, close_fds=True)

def bad_background()->None:
    logging.info(f"Setting background to [{badVideoBackground}]")
    subprocess.run(["nitrogen","--set-scaled",badVideoBackground])

badVideoCount=0
def deal_with_bad_video(window_name:str,regex_that_matched:str)->None:
    global badVideoCount
    global tooManyBadVideos

    badVideoCount+=1
    logging.warning(f"Found bad video window, processing: |{window_name}|{regex_that_matched}|{badVideoCount}") 

    stop_browser()

    if badVideoCount <= tooManyBadVideos:
        start_browser()
    else:
        logging.error("Stopping browser because of too many bad videos")
        bad_background()


def check_for_bad_videos()->None:
    logging.info("Starting check for bad videos")

    window_names=subprocess.run(["xdotool search --class firefox getwindowname %@"],capture_output=True,text=True,shell=True).stdout
    logging.debug(f"Got window names[{window_names}]")

    with open(whitelistFilename, 'r') as file:
        whitelist = file.readlines()
        logging.info(f"Opened WhiteList File [{whitelistFilename}]")

        with open(blacklistFilename, 'r') as file:
            blacklist = file.readlines()
            logging.info(f"Opened BlackList File [{blacklistFilename}]")

            for window_name in window_names.splitlines():
                # don't bother with windows whose title is only Firefox
                if window_name == "":
                    continue
                if window_name == "Close tab":
                    continue
                if window_name == "Firefox":
                    continue

                logging.info(f"Checking window name [{window_name}]")

                # If the window name matches a whitelist item, then let it go
                regex_that_matched = does_string_match_list(whitelist,window_name)
                if regex_that_matched:
                    logging.debug(f"Window [{window_name}] matched WhiteList regex [{regex_that_matched}]")
                    continue

                regex_that_matched= does_string_match_list(blacklist,window_name)
                if regex_that_matched is not None:
                    logging.debug(f"Window [{window_name}] matched BlackList regex [{regex_that_matched}]")
                    deal_with_bad_video(window_name,regex_that_matched)

    logging.info("Finished check for bad videos")

def mainloop()->None:
    logging.info("Starting pinforocess to check for bad videos")
    while True:
        check_for_bad_videos()
        time.sleep( 60)


if __name__ == "__main__":
    mainloop()
