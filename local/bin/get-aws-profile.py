#! /usr/bin/env python3

from os.path import expanduser

home_directory = expanduser("~")

aws_dir = home_directory + "/.aws"
config_file = aws_dir + "/config"
credentials_file = aws_dir + "/credentials"

try:
    from configparser import ConfigParser
except ImportError:
    from ConfigParser import ConfigParser  # ver. < 3.0

# Find credentials items
config = ConfigParser()
config.read(credentials_file)

aws_access_key_id = config.get("default", "aws_access_key_id")
print(f'export AWS_ACCESS_KEY_ID="{aws_access_key_id}"')
aws_secret_access_key = config.get("default", "aws_secret_access_key")
print(f'export AWS_SECRET_ACCESS_KEY="{aws_secret_access_key}"')

if config.has_option("default", "aws_session_token"):
    aws_session_token = config.get("default", "aws_session_token")
    print(f'export AWS_SESSION_TOKEN="{aws_session_token}"')

# Find config items
config = ConfigParser()
config.read(config_file)

aws_region = config.get("default", "region")
print(f'export AWS_DEFAULT_REGION="{aws_region}"')
print(f'export AWS_REGION="{aws_region}"')
