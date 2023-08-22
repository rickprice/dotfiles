#!/usr/bin/env python3

import subprocess

window_names=subprocess.run(["xdotool search --class firefox getwindowname %@"],capture_output=True,text=True,shell=True).stdout

if __name__ == "__main__":
    print(window_names)
