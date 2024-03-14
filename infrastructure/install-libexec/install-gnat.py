#! /usr/bin/env python3

"""
Install GNAT FSF script.

This script automates the download and install of GNAT FSF and gnatprove FSF,
which are required to build the Ada services for OpenUxAS.

Run this script via `infrastructure/install` from the root of your repository,
like this:

    OpenUxAS$ infrastructure/install

To get more information and to better control the install, run:

    OpenUxAS$ infrastructure/install --help
"""

from __future__ import annotations

import logging
import os
import pathlib
import shutil

from support.arguments import (
    add_logging_group,
    add_interactive_group,
    add_apt_group,
    add_force_argument,
    add_dry_run_argument,
)
from support.commands import (
    Command,
    run_command_and_exit_on_fail,
    run_command_getting_result_and_exit_on_fail,
)
from support.log import configure_logging, log_wrap
from support.paths import ALR_DIR, OPENUXAS_ROOT


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
        "unzip",
        "libsodium-dev",  # TBD if we want this here
    ],
    description="Installing dependencies",
)


ALR_SETTINGS_DIR = os.path.join(ALR_DIR, "settings")
ALR_BIN = os.path.join("bin", "alr")


ALR_DOWNLOAD_LINK = (
    "https://github.com/alire-project/alire/releases/download/"
    "v2.0.0/alr-2.0.0-bin-x86_64-linux.zip"
)
ALR_DOWNLOAD_FILE = "alr.zip"
ALR_DOWNLOAD_CMD = Command(
    cmd=[
        "wget",
        ALR_DOWNLOAD_LINK,
        "-O",
        ALR_DOWNLOAD_FILE,
    ],
    description="Downloading alr",
    cwd=ALR_DIR,
)


ALR_UNZIP_CMD = Command(
    cmd=["unzip", ALR_DOWNLOAD_FILE],
    description="Unzipping alr",
    cwd=ALR_DIR,
)


ALR_TOOLCHAIN_CMD = Command(
    cmd=[
        ALR_BIN,
        "-s",
        ALR_SETTINGS_DIR,
        "toolchain",
        "--select",
        "gnat_native^13",
        "gprbuild^22",
    ],
    description="Install GNAT toolchain using alr",
    cwd=ALR_DIR,
)


ALR_GNATPROVE_CMD = Command(
    cmd=[
        ALR_BIN,
        "-s",
        ALR_SETTINGS_DIR,
        "get",
        "gnatprove^13",
    ],
    description="Install GNATprove using alr",
    cwd=ALR_DIR,
)


ALR_GNATPROVE_DIRNAME_CMD = Command(
    cmd=[
        ALR_BIN,
        "-s",
        ALR_SETTINGS_DIR,
        "get",
        "--dirname",
        "gnatprove",
    ],
    description="Get install directory of GNATprove using alr",
    cwd=ALR_DIR,
)


DESCRIPTION = """\
This script automates the installation of GNAT FSF and gnatprove FSF, which is
required to build the Ada services for OpenUxAS and run the proofs. You should
run this script like this:

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

    skip_install_gnat = False

    if os.path.exists(ALR_DIR):
        if args.force:
            if args.dry_run:
                print(f"rm -rf {os.path.relpath(ALR_DIR, OPENUXAS_ROOT)}")
            else:
                shutil.rmtree(ALR_DIR)
        else:
            logging.warning(
                log_wrap(
                    """\
                GNAT FSF appears to have already been installed; skipping this
                step. Remove it manually or use `--force` if you wish to
                reinstall GNAT FSF and gnatprove FSF.\
                """
                )
            )
            skip_install_gnat = True

    if skip_install_gnat:
        exit(0)

    if args.update_apt and (
        not args.interactive
        or input("Update apt before installing packages? [Y/n] ") != "n"
    ):
        run_command_and_exit_on_fail(APT_UPDATE, args.dry_run)

    if args.install_packages and (
        not args.interactive
        or input("Install packages needed for GNAT FSF install using apt? [Y/n] ")
        != "n"
    ):
        run_command_and_exit_on_fail(APT_INSTALL, args.dry_run)

    if args.dry_run:
        # This is a bit awkward, but illustrates what we will do.
        print("mkdir -p " + os.path.relpath(ALR_DIR, OPENUXAS_ROOT))

    else:
        pathlib.Path(ALR_DIR).mkdir(parents=True, exist_ok=True)
        pathlib.Path(ALR_SETTINGS_DIR).mkdir(parents=True, exist_ok=True)

    run_command_and_exit_on_fail(ALR_DOWNLOAD_CMD, args.dry_run)
    run_command_and_exit_on_fail(ALR_UNZIP_CMD, args.dry_run)
    run_command_and_exit_on_fail(ALR_TOOLCHAIN_CMD, args.dry_run)

    # Now install gnatprove
    run_command_and_exit_on_fail(ALR_GNATPROVE_CMD, args.dry_run)

    GNATPROVE_SRC_DIR = os.path.join(
        ALR_DIR,
        run_command_getting_result_and_exit_on_fail(
            ALR_GNATPROVE_DIRNAME_CMD, args.dry_run
        ).strip(),
    )
    GNATPROVE_DST_DIR = os.path.join(ALR_DIR, "gnatprove")

    if args.dry_run:
        print(
            f"mv {os.path.relpath(GNATPROVE_SRC_DIR, OPENUXAS_ROOT)}"
            f"{os.path.relpath(GNATPROVE_DST_DIR, OPENUXAS_ROOT)}"
        )
    else:
        shutil.move(GNATPROVE_SRC_DIR, GNATPROVE_DST_DIR)
