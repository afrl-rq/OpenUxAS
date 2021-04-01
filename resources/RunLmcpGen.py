#! /usr/bin/env python3
"""Build LMCPgen and generate code and docs from MDMs."""

from __future__ import annotations

import os
import shutil
import logging
import re
import subprocess
import sys

from typing import TYPE_CHECKING

from uxas.paths import (
    OPENUXAS_ROOT,
    ANOD_BIN,
    MDMS_DIR,
    LMCP_DIR,
    AMASE_DIR,
    CPP_DIR,
    DOC_DIR,
)

if TYPE_CHECKING:
    from argparse import Namespace
    from typing import List
    from os import _Environ


DESCRIPTION = """\
This script will build LMCPgen and generate cpp, docs, and python from all
MDMs in the OpenUxAS repository.
"""

# Path variables. The assumption is that these should be read from the
# environment, but we provide fallbacks just in case.
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

AMASE_LMCP_DIR = os.path.join(AMASE_DIR, "OpenAMASE", "lib", "LMCP")

ANT_CMD = ["ant", "-q", "jar"]

LMCP_JAR = os.path.join(LMCP_DIR, "dist", "LmcpGen.jar")
LMCP_CMD = ["java", "-Xmx2048m", "-jar", LMCP_JAR, "-mdmdir", MDMS_DIR]

SRC_CPP_LMCP = os.path.join(CPP_DIR, "LMCP")

DOC_LMCP = os.path.join(DOC_DIR, "LMCP")

PY_LMCP = os.path.join(SRC_CPP_LMCP, "py")

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


def log_call(cmd: List[str], cwd: str, env: _Environ) -> None:
    """Log to debug and call."""
    logging.debug(f"Run `{' '.join(cmd)}` in {cwd}")
    subprocess.run(cmd, cwd=cwd, env=env)


def compute_env() -> _Environ:
    """Compute the environment for running commands."""
    base_env = os.environ

    anod_cmd = [ANOD_BIN, "printenv", "lmcpgen", "--build-env", "--inline"]

    if os.path.exists(ANOD_BIN):
        result = subprocess.run(anod_cmd, capture_output=True)
        a = re.split(
            r"[ =]",
            re.sub(
                r'"', "", result.stdout.decode(sys.stdout.encoding)
            ).strip(),
        )

        for i in range(0, len(a), 2):
            base_env[a[i]] = a[i + 1]

    return base_env


if __name__ == "__main__":
    from argparse import ArgumentParser

    argument_parser = ArgumentParser(
        description=DESCRIPTION,
    )
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
        print(LMCP_DIR)
        logging.critical(
            "LmcpGen must be present and located adjacent to OpenUxAS"
        )
        exit(1)

    env = compute_env()

    logging.info("Building LmcpGen")
    log_call(ANT_CMD, LMCP_DIR, env)
    logging.info("Done building LmcpGen")

    logging.info("Processing MDMs")
    log_call(LMCP_CMD + ["-cpp", "-dir", SRC_CPP_LMCP], OPENUXAS_ROOT, env)

    if os.path.isdir(AMASE_DIR):
        log_call(
            LMCP_CMD + ["-java", "-dir", AMASE_LMCP_DIR], OPENUXAS_ROOT, env
        )
    else:
        logging.warning(
            "OpenAMASE is expected to be present and located adjacent to OpenUxAS"
        )

    log_call(LMCP_CMD + ["-doc", "-dir", DOC_LMCP], OPENUXAS_ROOT, env)
    log_call(LMCP_CMD + ["-py", "-dir", PY_LMCP], OPENUXAS_ROOT, env)
    logging.info("Done processing MDMs")

    if args.build_amase:
        if not os.path.isdir(AMASE_DIR):
            logging.critical(
                "OpenAMASE must be present and located adjacent to OpenUxAS"
            )
            exit(2)

        logging.info("Building java library for OpenAMASE")
        log_call(ANT_CMD, AMASE_LMCP_DIR, env)
        shutil.copy(
            os.path.join(AMASE_LMCP_DIR, "dist", "lmcplib.jar"),
            os.path.join(AMASE_DIR, "OpenAMASE", "lib"),
        )
        log_call(ANT_CMD, os.path.join(AMASE_DIR, "OpenAMASE"), env)
        logging.info("Done building java library for OpenAMASE")
