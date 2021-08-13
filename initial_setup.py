#!/usr/bin/env python3

"""Given a YAML configuration file, setup a development environment."""

import argparse
import os
import re
import subprocess
import urllib.error
import urllib.request
from pathlib import Path
from sys import exit
from typing import Any, Callable, List, Union

import yaml

script_author = "rickp@activestate.com"

parser = argparse.ArgumentParser(
    description="Initial setup of configuration for ActiveState developer Docker image."
)
parser.add_argument("--offline", action="store_true", help="Skip VPN Check")
parser.add_argument("configuration_file")

args = parser.parse_args()
configuration_file = args.configuration_file


def printSSHCreationMessage(dot_ssh_directory: Path) -> None:
    """Print the message that explains how to setup SSH."""
    print(f"Couldn't find your .ssh directory or file [{dot_ssh_directory}]")
    print()
    print(
        "You need to create SSH keys to work with ActiveState, you might want to follow these instructions:"
    )
    print(
        "https://docs.github.com/en/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent"
    )
    print()
    print(
        "You should also add your SSH keys to your github account because the ActiveState tooling doesn't log in"
    )
    print(
        "https://docs.github.com/en/github/authenticating-to-github/adding-a-new-ssh-key-to-your-github-account"
    )


def compare_versions(version1: str, version2: str) -> int:
    """Function that is used to compare version strings, for example '1.2.3'."""

    def cmp(a: List[int], b: List[int]) -> int:
        return (a > b) - (a < b)

    def normalize(v: str) -> List[int]:
        return [int(x) for x in re.sub(r"(\.0+)*$", "", re.sub(r"[^0-9.]", "", v)).split(".")]

    return cmp(normalize(version1), normalize(version2))


def checkForDirectory(
    directory: Path, failure_message: Union[str, Callable[[Path], None]], exit_code: int
) -> None:
    """Check for the existence of a directory, and if it does not exist, either print the passed string, or call the Callable with the path."""
    if not os.path.isdir(directory):
        print()
        print()
        if callable(failure_message):
            failure_message(directory)
        else:
            print(failure_message)
        exit(exit_code)


def checkForFile(
    file_path: Path, failure_message: Union[str, Callable[[Path], None]], exit_code: int
) -> None:
    """Check for the existence of a file, and if it does not exist, either print the passed string, or call the Callable with the file path."""
    if not os.path.isfile(file_path):
        print()
        print()
        if callable(failure_message):
            failure_message(file_path)
        else:
            print(failure_message)
        exit(exit_code)


def check_for_software(
    program: str,
    minimum_version: str,
    version_regex: str,
    fix_function: Union[str, Callable[[str, str], None]],
) -> None:
    """Check that the software is installed.

    Check if the software is installed, and what version it is, if it is installed.
    If the software is installed, make sure the version is new enough.
    If there is a problem, either print the message string or call the fix_function to fix things.
    """

    print(f"... Checking that the {program} version is greater than or equal to: {minimum_version}")
    current_version = ""
    old_version = False
    no_version = False
    try:
        program_call = subprocess.run(
            [program, "--version"], stdout=subprocess.PIPE, text=True, check=True
        )
        match = re.search(version_regex, program_call.stdout, re.MULTILINE)
        if match:
            current_version = match.group(1)
        if compare_versions(minimum_version, current_version) > 0:
            old_version = True
    except (FileNotFoundError, subprocess.CalledProcessError):
        no_version = True

    if isinstance(fix_function, str):
        print(fix_function)
    else:
        if no_version:
            fix_function(program, current_version)
        if old_version:
            fix_function(program, current_version)


def deal_with_software_apt(program: str, current_version: str) -> None:
    """Explain what needs to be done to install the missing software."""

    if not current_version:
        print()
        print()
        print(
            f"Failed to run the {program} command, {program} is needed for work at ActiveState..."
        )
        print(f"Try installing {program} with 'sudo apt-get install {program}'")
        exit(101)
    else:
        print()
        print()
        print(f"Your version of {program} [{current_version}] is too old, try upgrading with:")
        print("sudo apt-get update && sudo apt-get upgrade")
        exit(100)


def deal_with_software_state_tool(program: str, current_version: str) -> None:
    """Install the State tool software.

    We would only have been called if it was out of date or not installed.
    """

    if not current_version:
        print()
        print("... Installing state tool")
        try:
            subprocess.run(
                [
                    "curl",
                    "-o",
                    "/tmp/state_install.sh",
                    "-q",
                    "https://s3.ca-central-1.amazonaws.com/cli-update/update/state/install.sh",
                ],
                check=True,
            )
            subprocess.run(["chmod", "a+x", "/tmp/state_install.sh"], check=True)
            subprocess.run(["sed", "-i", "s/\r//g", "/tmp/state_install.sh"], check=True)
            subprocess.run(["/tmp/state_install.sh", "-n"], check=True)
        except subprocess.CalledProcessError:
            install_state_command = "sh <(curl -q https://s3.ca-central-1.amazonaws.com/cli-update/update/state/install.sh)"
            print("Failed to run command to install state, you may need to run this manually:")
            print(install_state_command)
            exit(201)
        print("... State tool installed")
    else:
        print("... Upgrading state tool")
        subprocess.run(["state", "update"], check=True)
        print("... State tool upgraded")


def ensure_repository_checked_out(directory: Path, URL: str) -> None:
    """Ensure that the Git repository is checked out."""

    print(f"... Detecting if the directory [{directory}] has a git checkout")
    if not os.path.isdir(os.path.join(directory, ".git")):
        print("...... Directory does not have anything checked out yet")
        print(".........running git clone")
        subprocess.run(["git", "clone", URL, directory], check=True)
        print(".........running git lfs install")
        subprocess.run(["git", "lfs", "install"], cwd=directory, check=True)
        print(".........running git lfs fetch")
        subprocess.run(["git", "lfs", "fetch"], cwd=directory, check=True)
        print(".........running git lfs checkout")
        subprocess.run(["git", "lfs", "checkout"], cwd=directory, check=True)
    else:
        print("...... Source code already checked out, all good")


def ensure_directory_exists(directory: Path) -> None:
    """Ensure that a directory exists, creating if needed."""

    print(f"... Ensuring directory [{directory}] exists")
    if not os.path.isdir(directory):
        print(f"...... Creating {directory}")
        os.makedirs(directory)


def ensure_link_exists(source_path: Path, destination_path: Path) -> None:
    """Ensure that a link exists, creating if needed."""

    print(f"... Ensuring link [{source_path}] exists")
    if not os.path.islink(destination_path):
        print(f"...... Creating link {source_path} to {destination_path}")
        os.symlink(source_path, destination_path)


def does_file_exist(file_path: Path) -> bool:
    """Check for file existence."""

    print(f"... Checking if file [{file_path}] exists")
    if os.path.isfile(file_path):
        print("...... File exists...")
        return True
    else:
        return False


def are_we_on_activestate_vpn() -> bool:
    """Use a heuristic to determine if we are on the ActiveState VPN."""

    try:
        urllib.request.urlopen("https://staging.activestate.build/")
        return True
    except urllib.error.URLError:
        return False


def are_we_on_activestate_office_vpn() -> bool:
    """Use a heuristic to determine if we are on the ActiveState Office VPN."""

    try:
        urllib.request.urlopen("https://officegw1.activestate.com/")
        return True
    except urllib.error.URLError:
        return False


def write_gitconfig_file(home_directory: Path, configuration: Any) -> None:
    """Write the .gitconfig configuration file."""

    file_path = Path(os.path.join(home_directory, ".gitconfig"))
    if does_file_exist(file_path):
        print("... exists, not creating")
        return

    print("... creating file")
    email = configuration["user"]["email"]
    name = configuration["user"]["fullname"]
    pivotal_user = configuration["pivotal"]["user"]
    pivotal_token = configuration["pivotal"]["token"]
    hub_user = configuration["hub"]["user"]

    contents = f"""[filter "lfs"]
    clean = git-lfs clean -- %f
    smudge = git-lfs smudge -- %f
    process = git-lfs filter-process
    required = true
[user]
    email = {email}
    name = {name}
[submit-work "pivotaltracker"]
    username = {pivotal_user}
    token = {pivotal_token}
[hub]
    host = github.com
[pull]
    rebase = true
[merge]
    tool = vimdiff
    conflictsstyle = diff3
[mergetool]
    prompt = false
[init]
    defaultBranch = master
[github]
	user = {hub_user}
[rebase]
	autoStash = true
"""

    with open(file_path, "w") as file:
        file.write(contents)


def write_editorchoice_file(home_directory: Path, configuration: Any) -> None:
    """Write the editor choice configuration file."""

    file_path = Path(os.path.join(home_directory, ".editorchoice"))
    if does_file_exist(file_path):
        print("... exists, not creating")
        return

    print("... creating file")
    editor = configuration["user"].get("editor", "vi")

    contents = f"""export EDITOR={editor}"""

    with open(file_path, "w") as file:
        file.write(contents)


def write_shellchoice_file(home_directory: Path, configuration: Any) -> None:
    """Write the shell choice configuration file."""

    file_path = Path(os.path.join(home_directory, ".shellchoice"))
    if does_file_exist(file_path):
        print("... exists, not creating")
        return

    print("... creating file")
    shell_path = configuration["user"].get("shell", "/bin/bash")

    contents = f"""export SHELL={shell_path}"""

    with open(file_path, "w") as file:
        file.write(contents)


def write_pivotaltrackerc_file(home_directory: Path, configuration: Any) -> None:
    """Write the .pivotaltrackerrc configuration file."""

    file_path = Path(os.path.join(home_directory, ".pivotaltrackerrc"))
    if does_file_exist(file_path):
        print("... exists, not creating")
        return

    print("... creating file")
    pivotal_user = configuration["pivotal"]["user"]
    project_id = configuration["pivotal"]["project_id"]
    pivotal_token = configuration["pivotal"]["token"]

    contents = f"""token = {pivotal_token}
project_id = {project_id}
user_id = {pivotal_user}
"""

    with open(file_path, "w") as file:
        file.write(contents)


def write_aws_config_file(home_directory: Path, configuration: Any) -> None:
    """Write the AWS config configuration file."""

    file_path = Path(os.path.join(home_directory, ".aws", "config"))
    if does_file_exist(file_path):
        print("... exists, not creating")
        return

    print("... creating file")
    aws_region = configuration["aws"]["region"]

    contents = f"""[default]
output = json
region = {aws_region}
"""

    with open(file_path, "w") as file:
        file.write(contents)


def write_aws_credentials_file(home_directory: Path, configuration: Any) -> None:
    """Write the AWS credentials configuration file."""

    file_path = Path(os.path.join(home_directory, ".aws", "credentials"))
    if does_file_exist(file_path):
        print("... exists, not creating")
        return

    print("... creating file")
    aws_access_key_id = configuration["aws"]["aws_access_key_id"]
    aws_secret_access_key = configuration["aws"]["aws_secret_access_key"]

    contents = f"""[default]
aws_access_key_id = {aws_access_key_id}
aws_secret_access_key = {aws_secret_access_key}
"""

    with open(file_path, "w") as file:
        file.write(contents)


def write_dotconfig_hub_file(home_directory: Path, configuration: Any) -> None:
    """Write the hub .config configuration file."""

    file_path = Path(os.path.join(home_directory, ".config", "hub"))
    if does_file_exist(file_path):
        print("... exists, not creating")
        return

    print("... creating file")
    hub_user = configuration["hub"]["user"]
    hub_oauth_token = configuration["hub"]["oauth_token"]

    contents = f"""github.com:
- user: {hub_user}
  oauth_token: {hub_oauth_token}
  protocol: https

"""

    with open(file_path, "w") as file:
        file.write(contents)


def write_dotcamel_file(home_directory: Path, configuration: Any) -> None:
    """Write the .camel configuration file."""

    file_path = Path(os.path.join(home_directory, ".camel"))
    if does_file_exist(file_path):
        print("... exists, not creating")
        return

    print("... creating file")
    notify_user = configuration["activestate_slack"]["notify_user"]

    contents = f"notify slack:@{notify_user}"

    with open(file_path, "w") as file:
        file.write(contents)


def write_pr_user_file(home_directory: Path, configuration: Any) -> None:
    """Write out AS configuration script to .local/bin."""

    file_path = Path(os.path.join(home_directory, ".local", "bin", "set_as_config.sh"))
    if does_file_exist(file_path):
        print("... exists, not creating")
        return

    print("... creating file")
    user = configuration["activestate"]["user"]
    code_signing_password = configuration["activestate"]["code_signing_password"]
    code_signing_cert = configuration["activestate"]["code_signing_cert"]

    contents = f"""
    #!/usr/bin/sh -f

    export AS_USER={user}
    export CODE_SIGNING_PASSWD={code_signing_password}
    export CODE_SIGNING_CERT={code_signing_cert}
    """

    with open(file_path, "w") as file:
        file.write(contents)

    subprocess.run(["chmod", "a+x", file_path], check=True)


def write_Ultisnips_config_file(home_directory: Path, configuration: Any) -> None:
    """Write the .gitconfig configuration file."""

    file_path = Path(os.path.join(home_directory, ".config", "nvim", "ultisnips.vim"))
    if does_file_exist(file_path):
        print("... exists, not creating")
        return

    print("... creating file")
    email = configuration["user"]["email"]
    name = configuration["user"]["fullname"]
    hub_user = configuration["hub"]["user"]

    contents = f"""" Configuration created by initial_setup.py
    let g:snips_author = "{name}"
    let g:snips_email = "{email}"
    let g:snips_github = "https://hithub.com/{hub_user}"
    """

    with open(file_path, "w") as file:
        file.write(contents)


def main() -> None:
    """Run through all the steps to setup the development environment."""
    print("Initial setup of docker image for ActiveState development")

    print(
        "You need to be on the 'dev' VPN to do your work, the office VPN will be slower and give you problems..."
    )

    print("Doing initial checks")

    if not args.offline:
        print("... Checking for VPN connectivity")
        if not are_we_on_activestate_vpn():
            print(
                "You need to connect to the ActiveState VPN before running this script, preferably the Dev VPN because it will work better"
            )
            exit(6)
        if are_we_on_activestate_office_vpn():
            print(
                "WARNING: You are connected to the ActiveState Office VPN, it would be bettter to use the Dev VPN"
            )

    print("... Checking for home directory")
    home_directory = Path.home()
    checkForDirectory(
        home_directory,
        f"Couldn't find your home directory [{home_directory}], something is probably very wrong with the script, please contact {script_author}",
        1,
    )

    print("... Checking for .ssh directory")
    dot_ssh_directory = Path(os.path.join(home_directory, ".ssh"))
    checkForDirectory(dot_ssh_directory, printSSHCreationMessage, 2)

    dot_ssh_pub_file = Path(os.path.join(home_directory, ".ssh", "id_ed25519.pub"))
    print("... Checking for id_ed25519.pub file")
    checkForFile(dot_ssh_pub_file, printSSHCreationMessage, 3)

    print("Seems all the inital checks passed")
    print()

    print("Ensure required directories exist and are populated")
    # ensure_directory_exists(
    # Path(os.path.join(home_directory, "Documents", "ActiveState", "DockerMount"))
    # )
    # ensure_directory_exists(
    # Path(os.path.join(home_directory, "Documents", "ActiveState", "CommandScriptLogs"))
    # )
    # ensure_directory_exists(
    # Path(os.path.join(home_directory, "Documents", "ActiveState", "ConfigurationBackups"))
    # )
    ensure_directory_exists(Path(os.path.join(home_directory, ".local", "bin")))
    # ensure_directory_exists(Path(os.path.join(home_directory, "camel")))
    # ensure_directory_exists(Path(os.path.join(home_directory, "TheHomeRepot")))
    ensure_directory_exists(Path(os.path.join(home_directory, ".aws")))
    ensure_directory_exists(Path(os.path.join(home_directory, ".config")))
    print("Required directories exist")
    print()

    print("Check environment variables")
    print("... Checking PATH")
    if "PATH" in os.environ:
        print("...... PATH environment variable exists")
        path_value = os.environ.get("PATH")
        should_be_in_path = os.path.join(home_directory, ".local", "bin")
        if path_value is not None and should_be_in_path in path_value:
            print(f"...... PATH seems to contain {should_be_in_path}, good...")
        else:
            print(
                f"PATH needs to contain {should_be_in_path} so that state tool can install somewhere"
            )
            print("Try running the following:")
            print(f"echo 'export PATH={should_be_in_path}:$PATH' >> ~/.bashrc")
            print("source ~/.bashrc")
            exit(5)

    print("Seems environment variables are good")
    print()

    print("Reading configuration file: ", configuration_file)
    with open(configuration_file) as f:
        data = yaml.load(f, Loader=yaml.FullLoader)
    print()
    print("Creating files if they don't exist")
    write_gitconfig_file(home_directory, data)
    write_aws_config_file(home_directory, data)
    # write_aws_credentials_file(home_directory, data)
    write_dotconfig_hub_file(home_directory, data)
    write_dotcamel_file(home_directory, data)
    write_pr_user_file(home_directory, data)
    write_pivotaltrackerc_file(home_directory, data)
    write_shellchoice_file(home_directory, data)
    write_editorchoice_file(home_directory, data)
    write_Ultisnips_config_file(home_directory, data)
    print("Configuration files now handled")
    print()

    print("Check that required software is installed")
    # check_for_software("curl", "7.68.0", r"^curl (\S*).*", deal_with_software_apt)

    # check_for_software("git", "2.24.3", r"^git version (\S*)", deal_with_software_apt)
    # check_for_software("git-lfs", "2.9.2", r"^git-lfs/(\S*)", deal_with_software_apt)
    # check_for_software("pipenv", "11.9.0", r"^pipenv, version (\S*)", deal_with_software_apt)
    # check_for_software("java", "11.0.0", r"openjdk (\S*)", deal_with_software_apt)

    # check_for_software("stow", "2.3.1", r"^stow.*version (\S*)", deal_with_software_state_tool)
    # print("Seems that required software is installed")
    # print()

    # check_for_software("state", "0.13.31", r"^Version (\S*)", deal_with_software_state_tool)
    # print("Seems that required software is installed")
    # print()

    print("Ensuring code checked out")
    destination_directory = Path(os.path.join(home_directory, "Documents", "ActiveState", "camel"))
    link_destination = Path(os.path.join(home_directory, "camel"))
    ensure_repository_checked_out(destination_directory, "git@github.com:ActiveState/camel.git")
    ensure_link_exists(destination_directory, link_destination)

    destination_directory = Path(
        os.path.join(home_directory, "Documents", "ActiveState", "TheHomeRepot")
    )
    link_destination = Path(os.path.join(home_directory, "TheHomeRepot"))
    ensure_repository_checked_out(
        destination_directory, "git@github.com:ActiveState/TheHomeRepot.git"
    )
    ensure_link_exists(destination_directory, link_destination)


if __name__ == "__main__":
    # execute only if run as a script
    main()
