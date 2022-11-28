"""Build LmcpGen and generate code and docs from MDMs."""

from __future__ import annotations

from argparse import ArgumentParser
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
    CPP_DIR,
    DOC_DIR,
    add_amase_dir_argument,
    resolve_amase_devel_dir,
    add_lmcp_dir_argument,
    resolve_lmcp_devel_dir,
)

from uxas.util.logging import (
    add_logging_group,
    activate_logger,
    get_logging_level,
)

if TYPE_CHECKING:
    from argparse import Namespace
    from typing import List
    from os import _Environ


DESCRIPTION = """\
This script will build LmcpGen and generate cpp, docs, and python from all
MDMs in the OpenUxAS repository.
"""

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
"""Path to the script directory."""

ANT_CMD = ["ant", "-q", "jar"]
"""Command to run Ant."""

SRC_CPP_LMCP = os.path.join(CPP_DIR, "LMCP")
"""Path to the C++ LMCP directory."""

DOC_LMCP = os.path.join(DOC_DIR, "LMCP")
"""Path to the LMCP documentation directory."""

PY_LMCP = os.path.join(SRC_CPP_LMCP, "py")
"""Path to the Python LMCP directory."""


def log_call(cmd: List[str], cwd: str, env: _Environ) -> None:
    """Log to debug and call."""
    logging.debug("Run `%s` in %s", " ".join(cmd), cwd)
    subprocess.run(cmd, cwd=cwd, env=env, check=True)


def compute_env() -> _Environ:
    """Compute the environment for running commands."""
    base_env = os.environ

    anod_cmd = [ANOD_BIN, "printenv", "lmcpgen", "--build-env", "--inline"]

    if os.path.exists(ANOD_BIN):
        result = subprocess.run(anod_cmd, capture_output=True, check=True)
        env_parts = re.split(
            r"[ =]",
            re.sub(r'"', "", result.stdout.decode(sys.stdout.encoding)).strip(),
        )

        if len(env_parts) % 2 == 0:
            for i in range(0, len(env_parts), 2):
                base_env[env_parts[i]] = env_parts[i + 1]

    return base_env


UNBUILT_SPECIFIED_LMCPGEN = """\
The LmcpGen path `%s` exists, but it hasn't been built. You should:

    cd "%s" && %s

You may need to put ant on your path first, like this:

    eval "$( %s printenv ant )"
"""

MISSING_LMCPGEN = """\
Before you can run LmcpGen, you need to build it. You should:

    %s build lmcpgen
"""


def check_lmcp_jar(path: str) -> bool:
    """Check if the LMCP jar exists."""
    return os.path.exists(os.path.join(path, "dist", "LmcpGen.jar"))


def build_lmcpgen(path: str, env: _Environ) -> None:
    """Build LmcpGen."""
    logging.info("Building LmcpGen")
    log_call(ANT_CMD, path, env)
    logging.info("Done building LmcpGen")


def resolve_lmcp_dir(args: Namespace, env: _Environ) -> str:
    """
    Resolve the LMCP directory.

    Find the LMCP directory in the following order:
      1. Using `resolve_lmcp_devel_dir`, if the development directory exists,
         use that.
      2. Use the default LMCP_DIR.
    """
    lmcp_devel_dir = resolve_lmcp_devel_dir(args)

    if os.path.exists(lmcp_devel_dir):
        logging.info("Using LmcpGen development directory: %s", lmcp_devel_dir)

        if not check_lmcp_jar(lmcp_devel_dir):
            build_lmcpgen(lmcp_devel_dir, env)

        return lmcp_devel_dir

    if check_lmcp_jar(LMCP_DIR):
        return LMCP_DIR

    logging.critical(MISSING_LMCPGEN, ANOD_BIN)
    sys.exit(1)


def run_lmcp_gen_main() -> int:
    """Run main script-like functionality for LmcpGen."""
    argument_parser = ArgumentParser(
        description=DESCRIPTION,
    )
    argument_parser.add_argument(
        "-a",
        "--build-amase",
        dest="build_amase",
        action="store_true",
        default=False,
        help="build all languages, including  the java library for OpenAMASE",
    )
    add_lmcp_dir_argument(argument_parser)
    add_amase_dir_argument(argument_parser)

    add_logging_group(argument_parser)

    args = argument_parser.parse_args()

    activate_logger(args, get_logging_level(args))

    env = compute_env()

    if args.build_amase:
        amase_devel_dir = resolve_amase_devel_dir(args)

        if not os.path.isdir(amase_devel_dir):
            logging.critical(
                "Cannot generate Java bindings or build java library for "
                "OpenAMASE - OpenAMASE not found in %s\n"
                "        Run `%s devel-setup amase` and try again.",
                amase_devel_dir,
                ANOD_BIN,
            )
            return 2

    lmcp_dir = resolve_lmcp_dir(args, env)
    lmcp_jar = os.path.join(lmcp_dir, "dist", "LmcpGen.jar")

    logging.info("Processing MDMs from %s...", MDMS_DIR)
    lmcp_cmd = ["java", "-Xmx2048m", "-jar", lmcp_jar, "-mdmdir", MDMS_DIR]

    logging.info("Generating CPP bindings in %s", SRC_CPP_LMCP)
    log_call(lmcp_cmd + ["-cpp", "-dir", SRC_CPP_LMCP], OPENUXAS_ROOT, env)

    logging.info("Generating documentation in %s", DOC_LMCP)
    log_call(lmcp_cmd + ["-doc", "-dir", DOC_LMCP], OPENUXAS_ROOT, env)

    logging.info("Generating python bindings in %s", PY_LMCP)
    log_call(lmcp_cmd + ["-py", "-dir", PY_LMCP], OPENUXAS_ROOT, env)
    logging.info("Done processing MDMs")

    if args.build_amase:
        logging.info("Using OpenAMASE development directory: %s", amase_devel_dir)
        amase_lmcp_dir = os.path.join(amase_devel_dir, "OpenAMASE", "lib", "LMCP")

        logging.info("Generating Java bindings in %s", amase_lmcp_dir)
        log_call(lmcp_cmd + ["-java", "-dir", amase_lmcp_dir], OPENUXAS_ROOT, env)

        logging.info("Building java library for OpenAMASE")
        log_call(ANT_CMD, amase_lmcp_dir, env)
        shutil.copy(
            os.path.join(amase_lmcp_dir, "dist", "lmcplib.jar"),
            os.path.join(amase_devel_dir, "OpenAMASE", "lib"),
        )
        log_call(ANT_CMD, os.path.join(amase_devel_dir, "OpenAMASE"), env)
        logging.info("Done building java library for OpenAMASE")

    return 0


if __name__ == "__main__":
    sys.exit(run_lmcp_gen_main())
