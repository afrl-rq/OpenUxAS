#! /usr/bin/env python3

"""
Install front-end script.

This script provides a front-end for installing components needed to be able to
run the anod build of OpenUxAS.

Run this script from the root of bootstrap, like this:

    python3 util/install

To get more information and to control the script's behavior, run:

    python3 util/install --help
"""

from __future__ import annotations

import os
import sys

from support.arguments import (
    add_logging_group,
    add_interactive_group,
    add_force_argument,
    add_dry_run_argument,
)
from support.commands import Command, run_command_and_exit_on_fail
from support.log import configure_logging
from support.paths import INSTALL_LIBEXEC_DIR


GNAT_INSTALL = Command(
    cmd=[sys.executable, os.path.join(INSTALL_LIBEXEC_DIR, "install-gnat.py")],
    description="Install gnat",
)

VENV_INSTALL = Command(
    cmd=[sys.executable, os.path.join(INSTALL_LIBEXEC_DIR, "install-anod-venv.py")],
    description="Install anod venv",
)

JAVA_INSTALL = Command(
    cmd=[sys.executable, os.path.join(INSTALL_LIBEXEC_DIR, "install-java.py"), "-y"],
    description="Install Java",
)


def pass_args(command: Command) -> Command:
    """Add all arguments provided to this script to a command."""
    command.cmd += sys.argv[1:]
    return command


JAVA_PROMPT = """\
Install Java (OpenJDK 11) using apt?

*WARNING* If you already have a version of Java >= 11 installed, you probably
*should not* install Java using this script. If you're unsure, you can find out
by trying:

    $ java -version

Proceed and install Java? [Y/n] """


DESCRIPTION = """\
This script automates the installation of components needed to be able to run
the anod build of OpenUxAS. You should generally run this script from the root
of bootstra, like this:

    `python3 util/install`
"""

if __name__ == "__main__":
    from argparse import ArgumentParser

    argument_parser = ArgumentParser(
        description=DESCRIPTION,
    )

    add_dry_run_argument(argument_parser)
    add_force_argument(argument_parser)

    add_interactive_group(argument_parser)

    meta_gnat_group = argument_parser.add_argument_group("GNAT Community installation")
    gnat_group = meta_gnat_group.add_mutually_exclusive_group()
    gnat_group.add_argument(
        "--gnat",
        dest="install_gnat",
        action="store_true",
        default=True,
        help="install GNAT community",
    )
    gnat_group.add_argument(
        "--no-gnat",
        dest="install_gnat",
        action="store_false",
        help="do not install GNAT community",
    )

    meta_anod_group = argument_parser.add_argument_group(
        "anod virtual environment installation"
    )
    anod_group = meta_anod_group.add_mutually_exclusive_group()
    anod_group.add_argument(
        "--anod",
        dest="install_anod_venv",
        action="store_true",
        default=True,
        help="install anod virtual environment",
    )
    anod_group.add_argument(
        "--no-anod",
        dest="install_anod_venv",
        action="store_false",
        help="do not install anod virtual environment",
    )

    meta_java_group = argument_parser.add_argument_group(
        "java (OpenJDK 11) installation"
    )
    java_group = meta_java_group.add_mutually_exclusive_group()
    java_group.add_argument(
        "--java",
        dest="install_java",
        action="store_true",
        default=True,
        help="install Java (OpenJDK 11)",
    )
    java_group.add_argument(
        "--no-java",
        dest="install_java",
        action="store_false",
        help="do not install Java",
    )

    add_logging_group(argument_parser)

    (args, _) = argument_parser.parse_known_args()

    configure_logging(args)

    set_no_update = False

    if args.install_anod_venv and (
        not args.interactive or input("Install anod virtual environment? [Y/n] ") != "n"
    ):
        command = pass_args(VENV_INSTALL)

        if set_no_update:
            command.cmd.append("--no-update")

        run_command_and_exit_on_fail(command)
        set_no_update = True

    if args.install_java and (not args.interactive or input(JAVA_PROMPT) != "n"):
        command = pass_args(JAVA_INSTALL)

        if set_no_update:
            command.cmd.append("--no-update")

        run_command_and_exit_on_fail(command)
        set_no_update = True

    if args.install_gnat and (
        not args.interactive
        or input(
            "Install GNAT community (optional; needed to build Ada services)? [Y/n] "
        )
        != "n"
    ):
        command = pass_args(GNAT_INSTALL)

        if set_no_update:
            command.cmd.append("--no-update")

        run_command_and_exit_on_fail(command)
        set_no_update = True
