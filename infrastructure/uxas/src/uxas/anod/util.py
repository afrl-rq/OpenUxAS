from __future__ import annotations

from e3.anod.loader import AnodSpecRepository
from e3.anod.context import AnodContext
from e3.anod.sandbox import SandBox
from e3.os.process import Run

import logging
import os
import sys


def check_tool(tool: str) -> str:
    """Check tool version and return its version.

    The function will force exit if the tool is not found.

    :param tool: tool name
    :type tool: str
    :return: the tool version
    :rtype: str
    """
    try:
        p = Run([tool, "--version"])
        version = p.out.splitlines()[0]
        logging.info("%s version: %s", tool, version)
        return version
    except Exception:
        logging.critical("cannot find %s", tool)
        sys.exit(1)


def check_common_tools() -> None:
    # The following variables are used to force recompilation
    # in case of some tool change.
    gcc_version = check_tool("gcc")
    os.environ["OPENUXAS_COMPILER_VERSION"] = gcc_version
    cmake_version = check_tool("cmake")
    os.environ["OPENUXAS_CMAKE_VERSION"] = cmake_version
    check_tool("pkg-config")


def create_anod_context(spec_dir: str) -> AnodContext:
    return AnodContext(AnodSpecRepository(spec_dir))


def create_anod_sandbox(sbx_dir: str, spec_dir: str) -> SandBox:
    sbx = SandBox()
    sbx.root_dir = sbx_dir
    sbx.specs_dir = spec_dir

    return sbx
