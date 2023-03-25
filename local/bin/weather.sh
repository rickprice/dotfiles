#!/bin/sh
#
# Gets and parses the forecast from the Weather Office RSS
#

# Midland, Ontario, Canada
URL='https://dd.weather.gc.ca/citypage_weather/xml/ON/s0000281_e.xml'

TEMPERATURE_XPATH='//currentConditions/temperature'
WIND_SPEED_XPATH='//currentConditions/wind/speed'
WIND_DIRECTION_XPATH='//currentConditions/wind/direction'
PRESSURE_XPATH='//currentConditions/pressure'

curl -s $URL |
	xml sel -t \
		-o "Temp: " -v "$TEMPERATURE_XPATH" -v "$TEMPERATURE_XPATH/@units" \
		-o " Wind: " \
		-v "$WIND_DIRECTION_XPATH" -o " " \
		-v "$WIND_SPEED_XPATH" -v "$WIND_SPEED_XPATH/@units" \
		-o " Pressure: " \
		-v "$PRESSURE_XPATH" \
		-v "$PRESSURE_XPATH/@units" -o " " \
		-v "$PRESSURE_XPATH/@tendency"
