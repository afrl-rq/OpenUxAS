#! /usr/bin/env python3
"""Build LMCPgen and generate code and docs from MDMs."""

from __future__ import annotations

import os
import shutil
import logging
import subprocess

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from argparse import Namespace
    from typing import List


DESCRIPTION = """\
This script will build LMCPgen and generate cpp, docs, and python from all
MDMs in the OpenUxAS repository.
"""

# Directory in which this script is executing.
ROOT_DIR = os.path.dirname(os.path.abspath(__file__))
OPENUXAS_DIR = os.path.abspath(os.path.join(ROOT_DIR, ".."))

MDM_DIR = os.path.join(OPENUXAS_DIR, "mdms")

LMCP_DIR = os.path.abspath(os.path.join(OPENUXAS_DIR, "..", "LmcpGen"))
AMASE_DIR = os.path.abspath(os.path.join(OPENUXAS_DIR, "..", "OpenAMASE"))
AMASE_LMCP_DIR = os.path.join(AMASE_DIR, "OpenAMASE", "lib", "LMCP")

ANT_CMD = ["ant", "-q", "jar"]

LMCP_JAR = os.path.join(LMCP_DIR, "dist", "LmcpGen.jar")
LMCP_CMD = ["java", "-Xmx2048m", "-jar", LMCP_JAR, "-mdmdir", MDM_DIR]

SRC_CPP_LMCP = os.path.join(OPENUXAS_DIR, "src", "cpp", "LMCP")
DOC_LMCP = os.path.join(OPENUXAS_DIR, "doc", "LMCP")
PY_LMCP = os.path.join(OPENUXAS_DIR, "src", "cpp", "LMCP", "py")

# For log consistency with our other usages.
STREAM_FMT = "%(levelname)-8s %(message)s"
FILE_FMT = "%(asctime)s: %(name)-24s: %(levelname)-8s %(message)s"


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


def configure_logging(args: Namespace) -> None:
    """
    Configure the log based on parsed command-line arguments.

    To be used with `support.arguments.add_logging_group`.
    """
    if args.verbose == 1:
        level = logging.INFO
    elif args.verbose == 2:
        level = logging.DEBUG
    else:
        level = args.loglevel

    logging.getLogger("").setLevel(logging.DEBUG)

    streamHandler = logging.StreamHandler()
    streamHandler.setFormatter(logging.Formatter(STREAM_FMT))
    streamHandler.setLevel(level)
    logging.getLogger("").addHandler(streamHandler)

    if args.log_file:
        fileHandler = logging.FileHandler(args.log_file)
        fileHandler.setFormatter(logging.Formatter(FILE_FMT))
        fileHandler.setLevel(min(level, logging.DEBUG))
        logging.getLogger("").addHandler(fileHandler)


def log_call(cmd: List[str], cwd: str) -> None:
    """Log to debug and call."""
    logging.debug(f"Run `{' '.join(cmd)}` in {cwd}")
    subprocess.run(cmd, cwd=cwd)


if __name__ == "__main__":
    from argparse import ArgumentParser

    argument_parser = ArgumentParser(description=DESCRIPTION,)
    add_logging_group(argument_parser)

    argument_parser.add_argument(
        "-a",
        dest="build_amase",
        action="store_true",
        default=False,
        help="build java library for OpenAMASE",
    )

    args = argument_parser.parse_args()

    configure_logging(args)

    if not os.path.isdir(LMCP_DIR):
        logging.critical(
            "LmcpGen must be present and located adjacent to OpenUxAS"
        )
        exit(1)

    logging.info("Building LmcpGen")
    log_call(ANT_CMD, LMCP_DIR)
    logging.info("Done building LmcpGen")

    logging.info("Processing MDMs")
    log_call(LMCP_CMD + ["-cpp", "-dir", SRC_CPP_LMCP], OPENUXAS_DIR)

    if os.path.isdir(AMASE_DIR):
        log_call(LMCP_CMD + ["-java", "-dir", AMASE_LMCP_DIR], OPENUXAS_DIR)
    else:
        logging.warning(
            "OpenAMASE is expected to be present and located adjacent to OpenUxAS"
        )

    log_call(LMCP_CMD + ["-doc", "-dir", DOC_LMCP], OPENUXAS_DIR)
    log_call(LMCP_CMD + ["-py", "-dir", PY_LMCP], OPENUXAS_DIR)
    logging.info("Done processing MDMs")

    if args.build_amase:
        if not os.path.isdir(AMASE_DIR):
            logging.critical(
                "OpenAMASE must be present and located adjacent to OpenUxAS"
            )
            exit(2)

        logging.info("Building java library for OpenAMASE")
        log_call(ANT_CMD, AMASE_LMCP_DIR)
        shutil.copy(
            os.path.join(AMASE_LMCP_DIR, "dist", "lmcplib.jar"),
            os.path.join(AMASE_DIR, "OpenAMASE", "lib"),
        )
        log_call(ANT_CMD, os.path.join(AMASE_DIR, "OpenAMASE"))
        logging.info("Done building java library for OpenAMASE")
