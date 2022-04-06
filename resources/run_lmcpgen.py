"""Build LMCPgen and generate code and docs from MDMs."""

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
    AMASE_DIR,
    CPP_DIR,
    DOC_DIR,
)

from uxas.util.logging import (
    add_logging_group,
    activate_logger,
    get_logging_level,
)

if TYPE_CHECKING:
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


def log_call(cmd: List[str], cwd: str, env: _Environ) -> None:
    """Log to debug and call."""
    logging.debug("Run `%s{}` in %s", ' '.join(env), cwd)
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


def run_lmcp_gen_main() -> int:
    """Run main script-like functionality for lmcpgen."""
    argument_parser = ArgumentParser(
        description=DESCRIPTION,
    )
    argument_parser.add_argument(
        "-a",
        dest="build_amase",
        action="store_true",
        default=False,
        help="build all languages, including  the java library for OpenAMASE",
    )

    add_logging_group(argument_parser)

    args = argument_parser.parse_args()

    activate_logger(args, get_logging_level(args))

    if not os.path.isdir(LMCP_DIR):
        logging.critical(
            "LmcpGen is critical but not found at %s\n"
            "        Run `anod devel-setup lmcp` and try again.",
            LMCP_DIR
        )
        return 1

    env = compute_env()

    logging.info("Building LmcpGen")
    log_call(ANT_CMD, LMCP_DIR, env)
    logging.info("Done building LmcpGen")

    logging.info("Processing MDMs from %s...", MDMS_DIR)
    logging.info("Generating CPP bindings in %s", SRC_CPP_LMCP)
    log_call(LMCP_CMD + ["-cpp", "-dir", SRC_CPP_LMCP], OPENUXAS_ROOT, env)

    if os.path.isdir(AMASE_DIR):
        logging.info("Generating Java bindings in %s", AMASE_LMCP_DIR)
        log_call(LMCP_CMD + ["-java", "-dir", AMASE_LMCP_DIR], OPENUXAS_ROOT, env)
    else:
        logging.warning(
            "Cannot generate Java bindings - OpenAMASE not found in %s\n"
            "        Run `anod devel-setup amase` and try again.",
            AMASE_DIR
        )

    logging.info("Generating documentation in %s", DOC_LMCP)
    log_call(LMCP_CMD + ["-doc", "-dir", DOC_LMCP], OPENUXAS_ROOT, env)

    logging.info("Generating python bindings in %s", PY_LMCP)
    log_call(LMCP_CMD + ["-py", "-dir", PY_LMCP], OPENUXAS_ROOT, env)
    logging.info("Done processing MDMs")

    if args.build_amase:
        if not os.path.isdir(AMASE_DIR):
            logging.critical(
                "Cannot build java library for OpenAMASE - OpenAMASE not found in %s\n"
                "        Run `anod devel-setup amase` and try again.",
                AMASE_DIR
            )
            return 2

        logging.info("Building java library for OpenAMASE")
        log_call(ANT_CMD, AMASE_LMCP_DIR, env)
        shutil.copy(
            os.path.join(AMASE_LMCP_DIR, "dist", "lmcplib.jar"),
            os.path.join(AMASE_DIR, "OpenAMASE", "lib"),
        )
        log_call(ANT_CMD, os.path.join(AMASE_DIR, "OpenAMASE"), env)
        logging.info("Done building java library for OpenAMASE")

    return 0


if __name__ == "__main__":
    sys.exit(run_lmcp_gen_main())
