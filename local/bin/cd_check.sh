#!/bin/bash

echo "Final CD Master Check"
echo "===================="

for file in *.wav; do
    # Check sample rate and bit depth
    format=$(ffprobe -v quiet -select_streams a:0 \
             -show_entries stream=sample_rate,bits_per_sample \
             -of csv=p=0 "$file")
    
    # Check for clipping
    peak_sample=$(ffmpeg -i "$file" -af "astats=metadata=1:reset=1" \
                  -f null - 2>&1 | grep "Peak level" | tail -1)
    
    echo "$(basename "$file"): Format=$format, $peak_sample"
done
