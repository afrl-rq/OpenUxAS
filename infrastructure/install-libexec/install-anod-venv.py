#! /usr/bin/env python3

"""
Install anod virtual environment script.

This script automates the installation of the anod virtual environment, which
is required to build OpenUxAS using anod. You should generally run this script
from the root of your repository, like this:

Run this script via `infrastructure/install` from the root of your repository,
like this:

    OpenUxAS$ infrastructure/install

To get more information and to better control the install, run:

    OpenUxAS$ infrastructure/install --help
"""

from __future__ import annotations

import logging
import os
import sys
import shutil

from support.arguments import (
    add_logging_group,
    add_interactive_group,
    add_apt_group,
    add_force_argument,
    add_dry_run_argument,
)
from support.commands import Command, run_command_and_exit_on_fail
from support.log import configure_logging, log_wrap
from support.paths import INFRASTRUCTURE_UXAS, VPYTHON_DIR


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
        "cmake",
        "g++",
        "pkg-config",
        "uuid-dev",
        "libyaml-dev",
        "python3-dev",
        "python3-distutils",
        "python3-venv",
        "python3-pip",
    ],
    description="Installing dependencies",
)

VENV_PIP = os.path.join(VPYTHON_DIR, "bin", "pip")
VENV_INSTALL = Command(
    cmd=[sys.executable, "-m", "venv", "--clear", VPYTHON_DIR],
    description="Create python virtual environment",
)

PIP_UPDATE = Command(
    cmd=[VENV_PIP, "install", "--upgrade", "pip"],
    description="Updating pip in the virtual environment",
)

WHEEL_INSTALL = Command(
    cmd=[VENV_PIP, "install", "wheel"],
    description="Installing wheel in the virtual environment",
)

UXAS_INSTALL = Command(cmd=[VENV_PIP, "install", INFRASTRUCTURE_UXAS])

DESCRIPTION = """\
This script automates the installation of the anod virtual environment, which
is required to build OpenUxAS using anod. You should run this script like this:

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

    skip_install_venv = False

    if os.path.exists(VPYTHON_DIR):
        if args.force:
            if args.dry_run:
                print(f"rm -rf {VPYTHON_DIR}")
            else:
                shutil.rmtree(VPYTHON_DIR)
        else:
            logging.warning(
                log_wrap(
                    """\
                The anod virtual environment already exists; skipping this
                step. Remove it manually or use `--force` if you wish to
                reinstall the virtual environment.\
                """
                )
            )
            skip_install_venv = True

    if skip_install_venv:
        exit(0)

    if args.update_apt and (
        not args.interactive
        or input("Update apt before installing packages? [Y/n] ") != "n"
    ):
        run_command_and_exit_on_fail(APT_UPDATE, args.dry_run)

    if args.install_packages and (
        not args.interactive
        or input("Install packages needed for anod virtual environment? " "[Y/n] ")
        != "n"
    ):
        run_command_and_exit_on_fail(APT_INSTALL, args.dry_run)

    if sys.platform == "linux":
        assert os.path.isfile("/usr/include/uuid/uuid.h"), "Missing uuid.h"

    run_command_and_exit_on_fail(VENV_INSTALL, args.dry_run)
    run_command_and_exit_on_fail(PIP_UPDATE, args.dry_run)
    run_command_and_exit_on_fail(WHEEL_INSTALL, args.dry_run)
    run_command_and_exit_on_fail(UXAS_INSTALL, args.dry_run)
