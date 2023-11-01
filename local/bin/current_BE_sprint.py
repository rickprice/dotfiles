#!/usr/bin/env python3

import logging
import os
import sys
from typing import Any, Dict, List, Optional, Union, cast

import click_log  # type: ignore
import typer
import yaml
from atlassian import Jira  # type: ignore

from .conversion_rules import copy_to_jira, copy_to_metadata
from .metadata import Metadata

logger = logging.getLogger("story_toolkit")
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
file_option = typer.Argument(...)
pre_option = typer.Option(default=[])
post_option = typer.Option(default=[])
output_file_option = typer.Option(default=sys.stdout)
yaml_input_option = typer.Option(False)
yaml_output_option = typer.Option(False)
jira_configuration_file_option = typer.Option(os.path.expanduser("~/.config/jira/jira.yml"))
ticket_id_option = typer.Option(None)


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

stdin_sentinel = "STDIN"


def get_lines_from_stdin() -> List[str]:
    return [line.rstrip() for line in sys.stdin]


def get_lines_from_file(filename: str) -> List[str]:
    global stdin_sentinel

    if filename == stdin_sentinel:
        return get_lines_from_stdin()
    else:
        with open(filename) as file_object:
            return [line.rstrip() for line in file_object]


def get_yaml_from_filename(filename: str) -> dict[str, List[str]]:
    with open(filename) as file:
        return yaml.full_load(file)


@app.command()  # type: ignore
def copy(
    file: List[str] = file_option,
    pre: List[str] = pre_option,
    post: List[str] = post_option,
    output_file: typer.FileTextWrite = output_file_option,
    yaml_input: bool = yaml_input_option,
    yaml_output: bool = yaml_output_option,
    verbosity: str = verbosity_arg,
) -> None:
    logger.setLevel(verbosity)

    metadata = Metadata()

    if yaml_input:
        metadata.load_yaml_files(file, pre, post)
    else:
        metadata.load_csv_files(file, pre, post)

    if yaml_output:
        metadata.output_yaml(output_file)
    else:
        metadata.output_csv(output_file)


@app.command()  # type: ignore
def create_ticket(
    file: List[str] = file_option,
    pre: List[str] = pre_option,
    post: List[str] = post_option,
    output_file: typer.FileTextWrite = output_file_option,
    yaml_input: bool = yaml_input_option,
    yaml_output: bool = yaml_output_option,
    jira_configuration_file: typer.FileText = jira_configuration_file_option,
    verbosity: str = verbosity_arg,
) -> None:
    logger.setLevel(verbosity)

    metadata = Metadata()

    logger.debug("Raw PRE data: %(raw_pre_data)s", {"raw_pre_data": pre})
    logger.debug("Raw POST data: %(raw_post_data)s", {"raw_post_data": post})

    if yaml_input:
        metadata.load_yaml_files(file, pre, post)
    else:
        metadata.load_csv_files(file, pre, post)

    jira_api = get_jira_api(jira_configuration_file)

    # Toss any key if it exists since we are creating a new ticket
    metadata.metadata_delete("key")

    jira_issue: Dict[str, Any] = {}
    copy_to_jira(jira_api, jira_issue, metadata)

    logger.debug(
        "Raw Jira issue before create: %(jira_issue_as_yaml)s",
        {"jira_issue_as_yaml": yaml.dump(jira_issue)},
    )

    new_issue = cast(dict, jira_api.create_issue(fields=jira_issue["fields"]))

    key = new_issue["key"]

    # Now find newly created issue and pull it back
    jira_issue = cast(dict, jira_api.issue(key))
    logger.debug(
        "Raw Jira issue: %(jira_issue_as_yaml)s", {"jira_issue_as_yaml": yaml.dump(jira_issue)}
    )
    if jira_issue:
        metadata = Metadata()
        copy_to_metadata(jira_api, jira_issue, metadata)

        if yaml_output:
            metadata.output_yaml(output_file)
        else:
            metadata.output_csv(output_file)
    else:
        raise LookupError("Unable to find ticket that was created in Jira")


@app.command()  # type: ignore
def search_ticket(
    search_jiraql: str,
    output_file: typer.FileTextWrite = output_file_option,
    yaml_output: bool = yaml_output_option,
    jira_configuration_file: typer.FileText = jira_configuration_file_option,
    verbosity: str = verbosity_arg,
) -> None:
    logger.setLevel(verbosity)

    jira_api = get_jira_api(jira_configuration_file)

    # Get issue by key
    result = cast(Dict, jira_api.jql(search_jiraql))
    jira_issues = result["issues"]

    if len(jira_issues) != 1:
        raise ValueError("JiraQL search didn't return exactly one ticket")

    jira_issue = jira_issues[0]

    logger.debug(
        "Raw Jira issue: %(jira_issue_as_yaml)s", {"jira_issue_as_yaml": yaml.dump(jira_issue)}
    )
    if jira_issue:
        metadata = Metadata()
        copy_to_metadata(jira_api, jira_issue, metadata)

        if yaml_output:
            metadata.output_yaml(output_file)
        else:
            metadata.output_csv(output_file)


@app.command()  # type: ignore
def retrieve_ticket(
    ticket_id: str,
    output_file: typer.FileTextWrite = output_file_option,
    yaml_output: bool = yaml_output_option,
    jira_configuration_file: typer.FileText = jira_configuration_file_option,
    verbosity: str = verbosity_arg,
) -> None:
    logger.setLevel(verbosity)

    jira_api = get_jira_api(jira_configuration_file)

    # Get issue by key
    jira_issue = jira_api.issue(ticket_id)
    logger.debug(
        "Raw Jira issue: %(jira_issue_as_yaml)s", {"jira_issue_as_yaml": yaml.dump(jira_issue)}
    )
    if jira_issue:
        metadata = Metadata()
        copy_to_metadata(jira_api, jira_issue, metadata)

        if yaml_output:
            metadata.output_yaml(output_file)
        else:
            metadata.output_csv(output_file)


@app.command()  # type: ignore
def add_comment(
    ticket_id: str,
    comment_to_add: str,
    jira_configuration_file: typer.FileText = jira_configuration_file_option,
    verbosity: str = verbosity_arg,
) -> None:
    logger.setLevel(verbosity)

    jira_api = get_jira_api(jira_configuration_file)

    logger.debug("Adding comment to Jira ticket: %(jira_ticket_id)s", {"jira_ticket_id": ticket_id})

    new_comment = jira_api.issue_add_comment(ticket_id, comment_to_add)

    logger.debug("New comment added: %(jira_comment)s", {"jira_comment": new_comment})


@app.command()  # type: ignore
def update_ticket(
    file: List[str] = file_option,
    pre: List[str] = pre_option,
    post: List[str] = post_option,
    output_file: typer.FileTextWrite = output_file_option,
    yaml_input: bool = yaml_input_option,
    yaml_output: bool = yaml_output_option,
    jira_configuration_file: typer.FileText = jira_configuration_file_option,
    ticket_id: Optional[str] = ticket_id_option,
    verbosity: str = verbosity_arg,
) -> None:
    logger.setLevel(verbosity)

    metadata = Metadata()

    if yaml_input:
        metadata.load_yaml_files(file, pre, post)
    else:
        metadata.load_csv_files(file, pre, post)

    jira_api = get_jira_api(jira_configuration_file)

    if ticket_id:
        metadata.metadata_set_scalar("key", ticket_id)
    else:
        retrieved_ticket_id = metadata.metadata_get("key")
        if retrieved_ticket_id is None:
            raise ValueError("Need an issue ID to update")
        else:
            if isinstance(retrieved_ticket_id, str):
                ticket_id = retrieved_ticket_id
            else:
                raise TypeError("Invalid ticked id type")

    jira_issue: Dict[str, Any] = {}
    copy_to_jira(jira_api, jira_issue, metadata)

    logger.debug(
        "Raw Jira issue: %(jira_issue_as_yaml)s", {"jira_issue_as_yaml": yaml.dump(jira_issue)}
    )
    jira_api.issue_update(issue_key=ticket_id, fields=jira_issue["fields"])

    if yaml_output:
        metadata.output_yaml(output_file)
    else:
        metadata.output_csv(output_file)


# @app.command()
# def lint(
#     file: List[str] = file_option,
#     pre: List[str] = pre_option,
#     post: List[str] = post_option,
#     yaml_input: bool = yaml_input_option,
# ) -> None:
#     logger.setLevel(verbosity)
#
#     if yaml_input:
#         load_yaml_files(file, pre, post)
#     else:
#         load_csv_files(file, pre, post)
#
#     # TODO: Do whatever linting of data we can do


if __name__ == "__main__":
    app()
