#!/bin/bash

TARGET_LUFS=${1:--16}
TARGET_PEAK=${2:--0.3}
OUTPUT_DIR="normalized"

echo "Individual track normalization to ${TARGET_LUFS} LUFS, Peak: ${TARGET_PEAK} dBTP"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Normalize each track individually
ffmpeg-normalize *.wav \
    -nt ebu \
    -t "$TARGET_LUFS" \
    -tp "$TARGET_PEAK" \
    --keep-lra-above 1 \
    -ar 44100 \
    -c:a pcm_s16le \
    -pr \
    -ext wav \
    -o "$OUTPUT_DIR/"

echo "Individual normalization complete!"

# Generate verification report
echo "Loudness Verification Report" > loudness_report.txt
echo "Target: ${TARGET_LUFS} LUFS" >> loudness_report.txt
echo "================================" >> loudness_report.txt

for file in "$OUTPUT_DIR"/*.wav; do
    echo "Checking $(basename "$file")..."
    loudness=$(ffmpeg -i "$file" -af loudnorm=print_format=json -f null - 2>&1 | \
               grep input_i | cut -d'"' -f4)
    peak=$(ffmpeg -i "$file" -af loudnorm=print_format=json -f null - 2>&1 | \
           grep input_tp | cut -d'"' -f4)
    echo "$(basename "$file"): ${loudness} LUFS, ${peak} dBTP" >> loudness_report.txt
done

echo "Check loudness_report.txt for verification"
