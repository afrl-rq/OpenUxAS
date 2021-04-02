#! /usr/bin/env python3

"""
Install Java script.

This script automates the install of Java, which is required to build
OpenAMASE and LmcpGen.

Run this script via `infrastructure/install` from the root of your repository,
like this:

    OpenUxAS$ infrastructure/install

To get more information and to better control the install, run:

    OpenUxAS$ infrastructure/install --help
"""

from __future__ import annotations

from support.arguments import (
    add_logging_group,
    add_interactive_group,
    add_apt_group,
    add_force_argument,
    add_dry_run_argument,
)
from support.commands import Command, run_command_and_exit_on_fail
from support.log import configure_logging


APT_UPDATE = Command(
    cmd=["sudo", "apt", "update"],
    description="Updating apt",
)

APT_INSTALL = Command(
    cmd=[
        "sudo",
        "apt",
        "install",
        "-y",
        "openjdk-11-jdk",
        "ant",
    ],
    description="Installing OpenJDK 11",
)


DESCRIPTION = """\
This script automates the installation of Java, which is required to use
OpenAMASE and LcmpGen. OpenJDK 11 will be installed.

*WARNING* If you already have a version of Java >= 11 installed, you probably
*should not* run this script. If you're unsure, you can find out by trying:

    $ java -version

You should run this script like this:

    OpenUxAS$ infrastructure/install
"""


if __name__ == "__main__":
    from argparse import ArgumentParser

    argument_parser = ArgumentParser(
        description=DESCRIPTION,
    )

    add_dry_run_argument(argument_parser)
    add_force_argument(argument_parser)
    add_interactive_group(argument_parser)
    add_apt_group(argument_parser)
    add_logging_group(argument_parser)

    (args, _) = argument_parser.parse_known_args()

    configure_logging(args)

    if args.update_apt and (
        not args.interactive
        or input("Update apt before installing Java (OpenJDK 11)? [Y/n] ") != "n"
    ):
        run_command_and_exit_on_fail(APT_UPDATE, args.dry_run)

    if args.install_packages and (
        not args.interactive or input("Install Java (OpenJDK 11)? " "[Y/n] ") != "n"
    ):
        run_command_and_exit_on_fail(APT_INSTALL, args.dry_run)
