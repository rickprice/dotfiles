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
FORECAST_XPATH='//hourlyForecastGroup/hourlyForecast[1]/condition'

curl -s $URL |
	xml sel -t \
		-o "T: " -v "$TEMPERATURE_XPATH" -v "$TEMPERATURE_XPATH/@units" \
		-o " W: " \
		-v "$WIND_DIRECTION_XPATH" -o " " \
		-v "$WIND_SPEED_XPATH" -v "$WIND_SPEED_XPATH/@units" \
		-o " P: " \
		-v "$PRESSURE_XPATH" \
		-v "$PRESSURE_XPATH/@units" -o " " \
		-v "$PRESSURE_XPATH/@tendency" \
		-o " F: " \
		-v "$FORECAST_XPATH"
