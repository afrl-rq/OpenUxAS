#!/usr/bin/env python3

"""Anod environment printer."""

from __future__ import annotations

from uxas.anod.util import check_common_tools, create_anod_context, create_anod_sandbox
from uxas.paths import SPEC_DIR, SBX_DIR

from e3.main import Main
from e3.env import BaseEnv

import logging
import os


# Help users who forget to use eval.
BANNER = """
# ----------------------------------------------------------------------------
# If you are seeing this, then you forgot eval.
#
# You need to run anod printenv like this:
#
#   eval "$( ./anod printenv %s )"
#
# Otherwise, no changes will be made to your environment.
# ----------------------------------------------------------------------------
"""


def do_printenv(m: Main, set_prog: bool = True) -> int:
    """Gather options and print the environment for the given spec."""
    if set_prog:
        m.argument_parser.prog = m.argument_parser.prog + " printenv"

    m.argument_parser.add_argument(
        "spec_name",
        help="spec for which environment should be printed. This is "
        "the basename of an .anod file (without the extension)",
    )
    m.argument_parser.add_argument("--qualifier", help="optional qualifier")
    m.argument_parser.add_argument(
        "--sandbox-dir",
        help="directory in which build artifacts are stored",
        default=SBX_DIR,
    )
    m.argument_parser.add_argument(
        "--build-env",
        help="print build environment",
        action="store_true",
        default=False,
    )

    m.argument_parser.add_argument(
        "--inline",
        help="print variable definitions on a single line without exports",
        action="store_true",
        default=False,
    )
    m.parse_args()

    # Disable logging messages except errors
    logging.getLogger("").setLevel(logging.ERROR)

    check_common_tools()

    ac = create_anod_context(SPEC_DIR)
    sbx = create_anod_sandbox(m.args.sandbox_dir, SPEC_DIR)

    anod_instance = ac.add_anod_action(
        name=m.args.spec_name,
        primitive="build",
        qualifier=m.args.qualifier,
        sandbox=sbx,
        upload=False,
        env=BaseEnv.from_env(),
    ).anod_instance

    saved_env = {k: v for k, v in os.environ.items()}

    if m.args.build_env:
        if hasattr(anod_instance, "build_setenv"):
            anod_instance.build_setenv()
    else:
        if hasattr(anod_instance, "setenv"):
            anod_instance.setenv()

    for var, value in os.environ.items():
        if var not in saved_env or saved_env[var] != os.environ[var]:
            if m.args.inline:
                print('%s="%s"' % (var, value), end=" ")
            else:
                print('export %s="%s";' % (var, value))

                if m.args.verbose >= 1:
                    print('printf "I set %s=\\"%s\\"\\n\\n";' % (var, value))

                print(" ")

    if not m.args.inline:
        print(BANNER % m.args.spec_name)

    return 0


if __name__ == "__main__":
    exit(do_printenv(Main(), set_prog=False))
