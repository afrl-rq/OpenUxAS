#! /usr/bin/env python3

"""Script to configure OpenUxAS anod environment for development work."""

from __future__ import annotations

from uxas.paths import OPENUXAS_ROOT, SPEC_DIR

from e3.main import Main

from argparse import ArgumentParser, RawDescriptionHelpFormatter
import logging
import os
import subprocess
import sys
import yaml


SPEC_DIR_NAME = "specs"

CONFIG_DIR_NAME = "config"
REPOSITORIES_FILENAME = "repositories.yaml"
REPOSITORIES_YAML_PATH = os.path.join(SPEC_DIR, CONFIG_DIR_NAME, REPOSITORIES_FILENAME)

DEFAULT_REPO_DIR = os.path.join(OPENUXAS_ROOT, "develop")

DEFAULT_LMCP_DIR = os.path.join(DEFAULT_REPO_DIR, "LmcpGen")
LMCP_YAML_KEY = "lmcpgen"

DEFAULT_AMASE_DIR = os.path.join(DEFAULT_REPO_DIR, "OpenAMASE")
AMASE_YAML_KEY = "amase"

VCS_YAML_KEY = "vcs"
VCS_YAML_VALUE = "external"

URL_YAML_KEY = "url"

REV_YAML_KEY = "revision"
REV_YAML_VALUE = "None"

DESCRIPTION = """
Check out OpenUxAS, LmcpGen, and/or OpenAMASE for development work. Configure
anod so that the checked out repositories will be used during the build.

For example, run:

  ./anod devel-setup uxas

to check out OpenUxAS for development. By default, the checkout will be in
`develop/OpenUxAS`.

This results in a "development" configuration for the anod build, rather than
the default "release" configuration.

In the "release" configuration, anod will always ensure that all repositories
are at the specified refspec of their remote, stashing any local changes as
needed. This ensures that the build is up-to-date with all remotes, and is
thus appropriate for a release build. This configuration is not, however,
appropriate for a development build, as local changes are always stashed before
the build.

In the "development" configuration, however, anod will not manage the
repositories that are checked out by this script; instead, it will use their
current state for the build.
"""


def update_yaml(yaml_filename: str, key: str, clone_dir: str) -> None:
    """Update the given yaml file for the specified component."""
    with open(yaml_filename, "r") as yaml_file:
        loaded_yaml = yaml.safe_load(yaml_file.read())

    loaded_yaml[key][VCS_YAML_KEY] = VCS_YAML_VALUE
    loaded_yaml[key][URL_YAML_KEY] = clone_dir
    loaded_yaml[key][REV_YAML_KEY] = REV_YAML_VALUE

    with open(yaml_filename, "w") as yaml_file:
        yaml_file.write(yaml.dump(loaded_yaml))


def check_out(name: str, remote: str, refspec: str, clone_dir: str) -> None:
    """
    Check out the given repository.

    This is a deep (non-shallow) checkout, which differs from e3's typical
    operation.
    """
    if not os.path.exists(clone_dir):
        logging.info(
            "Checking out %s\n        from %s %s\n        to %s"
            % (name, remote, refspec, clone_dir)
        )

        subprocess.run(["git", "clone", remote, clone_dir])
        subprocess.run(["git", "checkout", refspec], cwd=clone_dir)


def configure_argparse_for_component(
    ap: ArgumentParser,
    key: str,
    name: str,
    default_dir: str,
    default_remote: str,
    default_refspec: str,
) -> None:
    """Add arguments to argparse for the given component."""
    ap.add_argument(
        "--%s-clone-dir" % key,
        default=default_dir,
        help=("absolute path where the %s repository has been " + "or should be cloned")
        % name,
    )

    ap.add_argument(
        "--%s-remote" % key,
        default=default_remote,
        help="the remote %s repository to clone" % name,
    )

    ap.add_argument(
        "--%s-refspec" % key,
        default=default_refspec,
        help="the %s refspec to clone" % name,
    )


def do_devel_setup(m: Main, set_prog: bool = True) -> int:
    """Execute the main functionality of the script."""
    if set_prog:
        m.argument_parser.prog = m.argument_parser.prog + " devel-setup"

    m.argument_parser.formatter_class = RawDescriptionHelpFormatter
    m.argument_parser.description = DESCRIPTION

    if not os.path.exists(REPOSITORIES_YAML_PATH):
        logging.error(
            "Cannot find `repositories.yaml` under `specs/config`.\n"
            + "Are you running in the right location?",
            file=sys.stderr,
        )
        exit(1)

    with open(REPOSITORIES_YAML_PATH, "r") as yaml_file:
        loaded_yaml = yaml.safe_load(yaml_file.read())

    m.argument_parser.add_argument(
        "component",
        choices=["lmcp", "amase"],
        nargs="+",
        help="the component to configure for development",
    )

    # Some defensiveness could be added here by wrapping in try-except to
    # account for possible missing keys in the repositories.yaml
    configure_argparse_for_component(
        m.argument_parser,
        "lmcp",
        "LmcpGen",
        DEFAULT_LMCP_DIR,
        loaded_yaml[LMCP_YAML_KEY][URL_YAML_KEY],
        loaded_yaml[LMCP_YAML_KEY][REV_YAML_KEY],
    )

    configure_argparse_for_component(
        m.argument_parser,
        "amase",
        "OpenAMASE",
        DEFAULT_AMASE_DIR,
        loaded_yaml[AMASE_YAML_KEY][URL_YAML_KEY],
        loaded_yaml[AMASE_YAML_KEY][REV_YAML_KEY],
    )

    args = m.argument_parser.parse_args()

    try:
        if "lmcp" in args.component:
            update_yaml(REPOSITORIES_YAML_PATH, LMCP_YAML_KEY, args.lmcp_clone_dir)
            check_out(
                "LmcpGen", args.lmcp_remote, args.lmcp_refspec, args.lmcp_clone_dir
            )

        if "amase" in args.component:
            update_yaml(REPOSITORIES_YAML_PATH, AMASE_YAML_KEY, args.amase_clone_dir)
            check_out(
                "OpenAMASE", args.amase_remote, args.amase_refspec, args.amase_clone_dir
            )

        return 0

    except Exception as e:
        print(e, file=sys.stderr)
        print(" ", file=sys.stderr)
        m.argument_parser.print_usage()

        return 1


if __name__ == "__main__":
    exit(do_devel_setup(Main(), set_prog=False))
