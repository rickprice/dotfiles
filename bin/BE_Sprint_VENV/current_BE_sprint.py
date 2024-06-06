#!/usr/bin/env python3

import logging
import os
from typing import Dict, Union

import click_log  # type: ignore
import typer
import yaml
from atlassian import Jira  # type: ignore

# The board we use to find sprints on
be_board_id = 17

logger = logging.getLogger("current_BE_sprint")
click_log.basic_config(logger)

# We use Typer to handle CLI stuff
app = typer.Typer()


def get_jira_api(
    jira_config_file: Union[str, typer.FileText, None], api_version: str = "2"
) -> Jira:
    filename: str
    jira_config: dict
    if jira_config_file is None:
        filename = os.path.expanduser("~/.config/jira/jira.yml")
        with open(filename) as f:
            jira_config = yaml.full_load(f)
    elif isinstance(jira_config_file, str):
        with open(jira_config_file) as f:
            jira_config = yaml.full_load(f)
    else:
        jira_config = yaml.full_load(jira_config_file)
    kwargs: Dict = {
        "api_version": api_version,
    }
    url = jira_config["url"]
    username = jira_config["username"]
    token = jira_config["token"]

    jira_api = Jira(url=url, username=username, password=token, cloud=True, **kwargs)

    return jira_api


# Type options
jira_configuration_file_option = typer.Option(os.path.expanduser("~/.config/jira/jira.yml"))


def verbosity_callback(value: str) -> str:
    value_list = ["CRITICAL", "ERROR", "WARNING", "INFO", "DEBUG"]
    if value not in value_list:
        raise typer.BadParameter(f"Value must be in {value_list}")
    return value


verbosity_arg = typer.Option(
    "ERROR",
    "--verbosity",
    "-v",
    help=""" Either CRITICAL, ERROR, WARNING, INFO or DEBUG """,
    callback=verbosity_callback,
)

@app.command()  # type: ignore
def current_sprint(
    jira_configuration_file: typer.FileText = jira_configuration_file_option,
    verbosity: str = verbosity_arg,
) -> None:
    logger.setLevel(verbosity)

    jira_api = get_jira_api(jira_configuration_file)

    active_sprints = jira_api.get_all_sprints_from_board(be_board_id,state="active")
    active_sprint_names = [ x["name"].split(" ")[2] for x in active_sprints["values"]]

    print(";".join(active_sprint_names))
if __name__ == "__main__":
    app()
