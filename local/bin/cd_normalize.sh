#!/bin/bash

TARGET_LUFS=${1:--16}
TARGET_PEAK=${2:--0.3}
OUTPUT_DIR="cd_masters"

echo "CD mastering normalization to ${TARGET_LUFS} LUFS, Peak: ${TARGET_PEAK} dBTP"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# CD-optimized normalization
ffmpeg-normalize *.wav \
    -nt ebu \
    -t "$TARGET_LUFS" \
    -tp "$TARGET_PEAK" \
    --two-pass \
    -pr \
    -ar 44100 \
    -c:a pcm_s16le \
    -ext wav \
    -o "$OUTPUT_DIR/"

echo "CD mastering complete!"

# Verification with intersample peak detection
echo "CD Master Verification Report" > cd_master_report.txt
echo "Target: ${TARGET_LUFS} LUFS, Peak: ${TARGET_PEAK} dBTP" >> cd_master_report.txt
echo "Sample Rate: 44.1kHz, Bit Depth: 16-bit" >> cd_master_report.txt
echo "=============================================" >> cd_master_report.txt

for file in "$OUTPUT_DIR"/*.wav; do
    echo "Analyzing $(basename "$file")..."
    
    # Get loudness and peak info
    analysis=$(ffmpeg -i "$file" -af loudnorm=print_format=json -f null - 2>&1)
    loudness=$(echo "$analysis" | grep input_i | cut -d'"' -f4)
    peak=$(echo "$analysis" | grep input_tp | cut -d'"' -f4)
    lra=$(echo "$analysis" | grep input_lra | cut -d'"' -f4)
    
    # Check file format
    format=$(ffprobe -v quiet -select_streams a:0 -show_entries stream=sample_rate,bits_per_sample -of csv=p=0 "$file")
    
    echo "$(basename "$file"):" >> cd_master_report.txt
    echo "  Loudness: ${loudness} LUFS" >> cd_master_report.txt
    echo "  True Peak: ${peak} dBTP" >> cd_master_report.txt
    echo "  LRA: ${lra} LU" >> cd_master_report.txt
    echo "  Format: ${format}" >> cd_master_report.txt
    echo "" >> cd_master_report.txt
done

echo "Check cd_master_report.txt for detailed analysis"
