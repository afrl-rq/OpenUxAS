"""Common command-line argument configuration for install scripts."""

from __future__ import annotations

from argparse import ArgumentParser
import logging

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from argparse import _ArgumentGroup
    from typing import Union


def add_logging_group(argument_parser: ArgumentParser) -> None:
    """
    Add a group and arguments to control the log.

    Use with `support.log.configure_logging`.
    """
    log_group = argument_parser.add_argument_group(title="logging arguments")
    log_group.add_argument(
        "-v",
        "--verbose",
        action="count",
        default=0,
        help="make the log output on the console more verbose",
    )
    log_group.add_argument(
        "--log-file",
        metavar="FILE",
        default=None,
        help="store all the logs into the specified file",
    )
    log_group.add_argument(
        "--loglevel",
        default=logging.WARNING,
        help="set the console log level",
        choices={
            "DEBUG": logging.DEBUG,
            "INFO": logging.INFO,
            "WARN": logging.WARN,
            "ERROR": logging.ERROR,
            "CRITICAL": logging.CRITICAL,
        },
    )


def add_interactive_group(argument_parser: ArgumentParser) -> None:
    """Add a group and arguments to control interactivity."""
    meta_interactive_group = argument_parser.add_argument_group("interactivity options")
    interactive_group = meta_interactive_group.add_mutually_exclusive_group()
    interactive_group.add_argument(
        "--interactive",
        action="store_true",
        default=True,
        help="run in interactive mode: ask before executing all actions",
    )
    interactive_group.add_argument(
        "-y",
        "--yes",
        "--automatic",
        action="store_false",
        dest="interactive",
        help="run in automatic mode",
    )


def add_apt_group(argument_parser: ArgumentParser) -> None:
    """Add a group and arguments to control apt updating and installing."""
    apt_group = argument_parser.add_argument_group("apt control")
    add_update_group(apt_group)
    add_package_group(apt_group)


def add_update_group(argument_parser: Union[ArgumentParser, _ArgumentGroup]) -> None:
    """Add arguments for updating apt."""
    update_group = argument_parser.add_mutually_exclusive_group()
    update_group.add_argument(
        "--update",
        dest="update_apt",
        action="store_true",
        default=True,
        help="update apt",
    )
    update_group.add_argument(
        "--no-update",
        dest="update_apt",
        action="store_false",
        help="do not update apt",
    )


def add_package_group(argument_parser: Union[ArgumentParser, _ArgumentGroup]) -> None:
    """Add arguments for apt packages."""
    package_group = argument_parser.add_mutually_exclusive_group()
    package_group.add_argument(
        "--packages",
        dest="install_packages",
        action="store_true",
        default=True,
        help="install packages needed by e3-core",
    )
    package_group.add_argument(
        "--no-packages",
        dest="install_packages",
        action="store_false",
        help="do not install packages needed by e3-core",
    )


def add_dry_run_argument(argument_parser: ArgumentParser) -> None:
    """Add an argument to set dry-run mode."""
    argument_parser.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        default=False,
        help="print out actions to be taken, but do not make any changes",
    )


def add_force_argument(argument_parser: ArgumentParser) -> None:
    """Add an argument to set force."""
    argument_parser.add_argument(
        "-f",
        "--force",
        action="store_true",
        default=False,
        help="force redownload and reinstallation of existing components",
    )
