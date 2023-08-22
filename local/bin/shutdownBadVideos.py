#!/usr/bin/env python3

import subprocess
import time
import re
from typing import Optional
from datetime import datetime

tooManyBadVideos=3
badVideoLogFilename="~/BadVideoLog.txt"

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
    with open('~/BadVideoLog.txt', 'wa') as file:
        when=datetime.now().isoformat()
        print(f"{when}|{window_name}|{regex_that_matched}|{badVideoCount}",file=file) 

    stop_browser()

    if badVideoCount <= tooManyBadVideos:
        start_browser()


def check_for_bad_videos()->None:
    window_names=subprocess.run(["xdotool search --class firefox getwindowname %@"],capture_output=True,text=True,shell=True).stdout

    with open('~/.config/shutdownBadVideos/whitelist.txt', 'r') as file:
        whitelist = file.readlines()

        with open('~/.config/shutdownBadVideos/blacklist.txt', 'r') as file:
            blacklist = file.readlines()

            for window_name in window_names:
                # don't bother with windows whose title is only Firefox
                if window_name == "Firefox":
                    continue

                # If the window name matches a whitelist item, then let it go
                regex_that_matched = does_string_match_list(whitelist,window_name)
                if regex_that_matched:
                    print(f"Window [{window_name}] matched WhiteList regex [{regex_that_matched}]")
                    continue

                regex_that_matched= does_string_match_list(blacklist,window_name)
                if regex_that_matched is not None:
                    deal_with_bad_video(window_name,regex_that_matched)


def mainloop()->None:
    while True:
        check_for_bad_videos()
        time.sleep( 60)


if __name__ == "__main__":
    print(window_names)
