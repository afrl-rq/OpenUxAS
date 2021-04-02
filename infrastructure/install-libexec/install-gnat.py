#! /usr/bin/env python3

"""
Install GNAT script.

This script automates the download and install of GNAT Community, which is
required to build the Ada services for OpenUxAS.

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
from support.commands import Command, run_command_and_exit_on_fail
from support.log import configure_logging, log_wrap
from support.paths import OPENUXAS_ROOT, SOFTWARE_DIR, GNAT_DIR


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
        "fontconfig",
        "libx11-xcb1",
        "libx11-6",
        "libncurses5",
    ],
    description="Installing dependencies",
)

GNAT_DOWNLOAD_DIR = os.path.join(SOFTWARE_DIR, "gnat_community")
GNAT_DOWNLOAD_FILE = "gnat-bin"
GNAT_DOWNLOAD_LINK = (
    "https://community.download.adacore.com/v1/"
    "4d99b7b2f212c8efdab2ba8ede474bb9fa15888d?"
    "filename=gnat-2020-20200429-x86_64-linux-bin"
)

GNAT_DOWNLOAD = Command(
    cmd=[
        "wget",
        "-O",
        os.path.join(GNAT_DOWNLOAD_DIR, GNAT_DOWNLOAD_FILE),
        GNAT_DOWNLOAD_LINK,
    ],
    description="Downloading GNAT community",
)

GNAT_INSTALLER_LINK = "https://github.com/AdaCore/gnat_community_install_script"
GNAT_INSTALLER_DIR = os.path.join(SOFTWARE_DIR, "gnat_community_install_script")
GNAT_INSTALLER_CLONE = Command(
    cmd=["git", "clone", GNAT_INSTALLER_LINK],
    description="Cloning GNAT community install script",
    cwd=SOFTWARE_DIR,
)
GNAT_INSTALLER_BIN = "install_package.sh"

GNAT_INSTALL = Command(
    cmd=[
        "sh",
        GNAT_INSTALLER_BIN,
        os.path.join(GNAT_DOWNLOAD_DIR, GNAT_DOWNLOAD_FILE),
        GNAT_DIR,
    ],
    description="Install GNAT community",
    cwd=GNAT_INSTALLER_DIR,
)

DESCRIPTION = """\
This script automates the installation of GNAT Community, which is required to
build the Ada services for OpenUxAS. You should run this script like this:

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

    gnat_group = argument_parser.add_argument_group("gnat install control")
    download_group = gnat_group.add_mutually_exclusive_group()
    download_group.add_argument(
        "--download-gnat",
        dest="download_gnat",
        action="store_true",
        default=True,
        help="download GNAT community",
    )
    download_group.add_argument(
        "--no-download-gnat",
        dest="download_gnat",
        action="store_false",
        help="do not download GNAT community",
    )

    clone_group = gnat_group.add_mutually_exclusive_group()
    clone_group.add_argument(
        "--clone-installer",
        dest="clone_gnat_installer",
        action="store_true",
        default=True,
        help="clone GNAT community installer",
    )
    clone_group.add_argument(
        "--no-clone-installer",
        dest="clone_gnat_installer",
        action="store_false",
        help="do not clone GNAT community installer",
    )

    add_logging_group(argument_parser)

    (args, _) = argument_parser.parse_known_args()

    configure_logging(args)

    skip_install_gnat = False

    if os.path.exists(GNAT_DIR):
        if args.force:
            if args.dry_run:
                print(f"rm -rf {GNAT_DIR}")
            else:
                shutil.rmtree(GNAT_DIR)
        else:
            logging.warning(
                log_wrap(
                    """\
                GNAT Community appears to have already been installed; skipping
                this step. Remove it manually or use `--force` if you wish to
                reinstall GNAT Community.\
                """
                )
            )
            skip_install_gnat = True

    if skip_install_gnat:
        exit(0)

    if args.dry_run:
        # This is a bit awkward, but illustrates what we will do.
        print("mkdir -p " + os.path.relpath(GNAT_DOWNLOAD_DIR, OPENUXAS_ROOT))
    else:
        pathlib.Path(GNAT_DOWNLOAD_DIR).mkdir(parents=True, exist_ok=True)

    if args.download_gnat and (
        not args.interactive or input("Download gnat? [Y/n] ") != "n"
    ):

        skip_gnat_download = False

        gnat_bin_file = os.path.join(GNAT_DOWNLOAD_DIR, GNAT_DOWNLOAD_FILE)

        if os.path.exists(gnat_bin_file):
            if args.force:
                if args.dry_run:
                    print(f"rm {gnat_bin_file}")
                else:
                    os.remove(gnat_bin_file)
            else:
                logging.warning(
                    log_wrap(
                        """\
                    GNAT Community seems to have already been downloaded;
                    skipping this step. Remove it manually or use `--force` if
                    you wish to redownload GNAT Community.\
                    """
                    )
                )
                skip_gnat_download = True

        if not skip_gnat_download:
            run_command_and_exit_on_fail(GNAT_DOWNLOAD, args.dry_run)

    if args.clone_gnat_installer and (
        not args.interactive or input("Clone gnat installer? [Y/n] ") != "n"
    ):

        skip_installer_download = False

        if os.path.exists(GNAT_INSTALLER_DIR):
            if args.force:
                if args.dry_run:
                    print(f"rm -rf {GNAT_INSTALLER_DIR}")
                else:
                    shutil.rmtree(GNAT_INSTALLER_DIR)
            else:
                logging.warning(
                    log_wrap(
                        """\
                    The GNAT Community installer seems to have already been
                    cloned; skipping this step. Remove it manually or use
                    `--force` if you wish to re-clone the installer.\
                    """
                    )
                )
                skip_installer_download = True

        if not skip_installer_download:
            run_command_and_exit_on_fail(GNAT_INSTALLER_CLONE, args.dry_run)

    if args.update_apt and (
        not args.interactive
        or input("Update apt before installing packages? [Y/n] ") != "n"
    ):
        run_command_and_exit_on_fail(APT_UPDATE, args.dry_run)

    if args.install_packages and (
        not args.interactive
        or input("Install packages needed for GNAT Community install? " "[Y/n] ") != "n"
    ):
        run_command_and_exit_on_fail(APT_INSTALL, args.dry_run)

    print("Installing GNAT Community Edition; this may take a while.")
    run_command_and_exit_on_fail(GNAT_INSTALL, args.dry_run)
