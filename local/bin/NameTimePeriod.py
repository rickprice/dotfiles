#!/usr/bin/env python3

import yaml
import os
import argparse
from datetime import datetime, timedelta

SYSTEM_CONFIG = "/etc/NameTimePeriod/time_periods.yaml"
USER_CONFIG = os.path.expanduser("~/.config/NameTimePeriod/time_periods.yaml")

def load_yaml_file(path):
    if os.path.exists(path):
        with open(path, 'r') as file:
            return yaml.safe_load(file)
    return {}

def merge_time_periods(system_data, user_data):
    merged = {}
    def to_dict(data):
        if not data or 'TimePeriods' not in data:
            return {}
        return {list(item.keys())[0]: list(item.values())[0] for item in data['TimePeriods']}
    
    sys_dict = to_dict(system_data)
    usr_dict = to_dict(user_data)

    merged.update(sys_dict)
    merged.update(usr_dict)

    return [{'{}'.format(name): config} for name, config in merged.items()]

def is_within_period(current_date, base_date, days_before, days_after):
    start_date = base_date - timedelta(days=days_before)
    end_date = base_date + timedelta(days=days_after)
    return start_date <= current_date <= end_date

def get_current_period(time_periods, current_date):
    current_year = current_date.year

    for item in time_periods:
        for name, values in item.items():
            try:
                base_date = datetime.strptime(f"{values['Date']} {current_year}", "%B %d %Y").date()
                days_before = int(values.get('DaysBefore', 0))
                days_after = int(values.get('DaysAfter', 0))

                if is_within_period(current_date, base_date, days_before, days_after):
                    return name
            except Exception:
                continue

    return "Default"

def parse_args():
    parser = argparse.ArgumentParser(description="Determine current time period.")
    parser.add_argument("--date", type=str, help="Override current date (format: YYYY-MM-DD)")
    return parser.parse_args()

if __name__ == "__main__":
    args = parse_args()

    # Parse date from CLI or use today
    try:
        current_date = datetime.strptime(args.date, "%Y-%m-%d").date() if args.date else datetime.now().date()
    except ValueError:
        print("Invalid date format. Use YYYY-MM-DD.")
        exit(1)

    system_data = load_yaml_file(SYSTEM_CONFIG)
    user_data = load_yaml_file(USER_CONFIG)

    if not system_data and not user_data:
        print("Default")
    else:
        merged_periods = merge_time_periods(system_data, user_data)
        print(get_current_period(merged_periods, current_date))
