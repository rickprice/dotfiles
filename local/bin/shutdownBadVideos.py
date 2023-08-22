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

# Setup logging
logging.basicConfig(filename=badVideoLogFilename,encoding='utf-8',format='%(asctime)s:%(levelname)s:%(message)s')


def does_string_match_list(list_to_check: list[str],string_to_check: str)->Optional[str]:
    for to_check in list_to_check:
        if re.search(to_check,string_to_check,re.IGNORECASE|re.VERBOSE):
            return to_check

    return None

def stop_browser()->None:
    pass

def start_browser()->None:
    pass

badVideoCount=0
def deal_with_bad_video(window_name:str,regex_that_matched:str)->None:
    global badVideoCount
    global tooManyBadVideos

    badVideoCount+=1
    logging.info(f"{window_name}|{regex_that_matched}|{badVideoCount}") 

    stop_browser()

    if badVideoCount <= tooManyBadVideos:
        start_browser()


def check_for_bad_videos()->None:
    window_names=subprocess.run(["xdotool search --class firefox getwindowname %@"],capture_output=True,text=True,shell=True).stdout

    with open(whitelistFilename, 'r') as file:
        whitelist = file.readlines()

        with open(blacklistFilename, 'r') as file:
            blacklist = file.readlines()

            for window_name in window_names:
                # don't bother with windows whose title is only Firefox
                if window_name == "Firefox":
                    continue

                # If the window name matches a whitelist item, then let it go
                regex_that_matched = does_string_match_list(whitelist,window_name)
                if regex_that_matched:
                    logging.debug(f"Window [{window_name}] matched WhiteList regex [{regex_that_matched}]")
                    continue

                regex_that_matched= does_string_match_list(blacklist,window_name)
                if regex_that_matched is not None:
                    deal_with_bad_video(window_name,regex_that_matched)


def mainloop()->None:
    logging.warning("Starting to check for bad videos")
    while True:
        check_for_bad_videos()
        time.sleep( 60)


if __name__ == "__main__":
    mainloop()
