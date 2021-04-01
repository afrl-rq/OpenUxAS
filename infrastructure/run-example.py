#! /usr/bin/env python3

"""Script to run OpenUxAS examples."""

from __future__ import annotations

import argparse
import logging
import os
import pathlib
import shutil
import subprocess
import time
import traceback
from typing import TYPE_CHECKING
import yaml

from uxas.paths import (
    OPENUXAS_ROOT,
    EXAMPLES_DIR,
    AMASE_DIR,
    UXAS_BIN,
    UXAS_ADA_BIN,
    SBX_DIR,
)

if TYPE_CHECKING:
    from argparse import ArgumentParser, Namespace
    from typing import Any, Dict, List, Optional, Tuple


# Relativized root
REL_OPENUXAS_ROOT = os.path.relpath(OPENUXAS_ROOT)

# Anod command
ANOD_CMD = os.path.join(REL_OPENUXAS_ROOT, "anod")


# Allow the environment to specify how long we should wait after starting an
# instance of OpenAMASE; default to 0 seconds.
AMASE_DELAY = os.environ.get("AMASE_DELAY", 0)

DESCRIPTION = """
Run OpenUxAS, OpenAMASE or both, using the configuration in the specified
example.

This script is a frontend for both OpenUxAS and OpenAMASE that simplifies
running examples contained in the `examples` directory of the OpenUxAS
repository. You run an example by providing the path to its directory, under
`examples`. For example, run:

  run-example 02_Example_WaterwaySearch

to run the Waterways example from a single terminal session.

To get a list of available examples, run:

  run-example --list
"""

# Hard-code the expected name of the YAML file the defines an example, for now.
CONFIG_FILE = "config.yaml"

# The key for specifying OpenAMASE configuration.
AMASE_YAML_KEY = "amase"

# The key for specifying an OpenAMASE scenario file.
SCENARIO_YAML_KEY = "scenario"

# The key for specying the delay after OpenAMASE opens.
DELAY_YAML_KEY = "delay"

# The key for specifying a single OpenUxAS configuration.
UXAS_YAML_KEY = "uxas"

# The key for specifying multiple OpenUxAS configurations.
UXASES_YAML_KEY = "uxases"

# The key for specifying an OpenUxAS configuration file.
CONFIG_YAML_KEY = "config"

# The key for specifying an OpenUxAS run directory.
RUNDIR_YAML_KEY = "rundir"

# The key for specifying the name of the OpenUxAS binary.
BIN_YAML_KEY = "bin"

# The default prefix for an OpenUxAS run directory.
RUN_DIR = "RUNDIR"

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


def configure_logging(args: Namespace, level: int) -> None:
    """
    Configure the log based on parsed command-line arguments.

    To be used with `support.arguments.add_logging_group`.
    """
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


def read_yaml(yaml_filename: str) -> Dict[str, Any]:
    """Read and parse a YAML file, returning the content as a yaml object."""
    with open(yaml_filename) as yaml_file:
        loaded_yaml = yaml.safe_load(yaml_file.read())

    return loaded_yaml


def resolve_examples_dir(args: Namespace) -> str:
    """Resolve the absolute path to the examples directory."""
    if args.examples_dir:
        return args.examples_dir
    else:
        return os.path.join(args.uxas_dir, "examples")


MISSING_AMASE = """\
Before you can run examples that use OpenAMASE, you need to build it. You
should:

    {anod} build amase
"""

UNBUILT_SPECIFIED_AMASE = """\
The OpenAMASE path `{path}` exists, but hasn't been built. You should:

    cd "{path}/OpenAMASE" && ant

You may need to put ant on your path first, like this:

    eval "$( {anod} printenv ant )"
"""


UNBUILT_LOCAL_AMASE = """\
There is an OpenAMASE in {path}, but it hasn't been built. If you
want to use this version of OpenAMASE, you should:

    cd "{path}/OpenAMASE" && ant

You may need to put ant on your path first, like this:

    eval "$( {anod} printenv ant )"

Trying the anod-built OpenAMASE as a fall back.
"""


def check_amase_dir(path: str) -> bool:
    """Test to make sure a path has the OpenUxAS build."""
    return os.path.exists(os.path.join(path, "OpenAMASE", "build"))


def resolve_amase_dir(args: Namespace) -> str:
    """
    Resolve the absolute path to the OpenAMASE source directory.

    1. if we've been given an absolute path in the arguments, check and use
       that.
    2. see if there's a local OpenAMASE, check and use that.
    3. try to use the anod-built OpenAMASE.

    If the OpenAMASE directory exists but doesn't appear to be built,
    immediately exit.
    """
    if args.amase_dir:
        if check_amase_dir(args.amase_dir):
            return args.amase_dir
        else:
            logging.critical(
                UNBUILT_SPECIFIED_AMASE.format(path=args.amase_dir, anod=ANOD_CMD)
            )
            exit(1)

    if os.path.exists(AMASE_DIR):
        if check_amase_dir(AMASE_DIR):
            return AMASE_DIR
        else:
            logging.warning(
                UNBUILT_LOCAL_AMASE.format(path=AMASE_DIR, anod=ANOD_CMD)
            )

    anod_amase_dir = os.path.join(SBX_DIR, "x86_64-linux", "amase", "src")
    if os.path.exists(anod_amase_dir) and check_amase_dir(anod_amase_dir):
        return anod_amase_dir
    else:
        logging.critical(MISSING_AMASE.format(anod=ANOD_CMD))
        exit(1)


def list_examples(examples_dir: str) -> None:
    """List all of the examples that have a configuration file."""
    full_paths = list()
    for (dirpath, _, filenames) in os.walk(examples_dir):
        if CONFIG_FILE in filenames:
            full_paths += [dirpath]

    short_paths = [os.path.relpath(path, examples_dir) for path in full_paths]
    short_paths.sort()

    for path in short_paths:
        print(f"  {path}")


def check_amase(
    loaded_yaml: Dict[str, Any], example_dir: str, args: Namespace
) -> Tuple[Optional[str], int]:
    """
    Check the OpenAMASE configuration in the YAML and return the scenario file.

    If any errors are encountered, report them and immediately exit.
    """
    if AMASE_YAML_KEY not in loaded_yaml.keys():
        return (None, 0)

    if SCENARIO_YAML_KEY not in loaded_yaml[AMASE_YAML_KEY].keys():
        logging.critical("OpenAMASE configuration must specify a scenario file.")
        exit(1)

    scenario_file = loaded_yaml[AMASE_YAML_KEY][SCENARIO_YAML_KEY]
    if not os.path.exists(os.path.join(example_dir, scenario_file)):
        logging.critical(f"Specified scenario file '{scenario_file}' does not exist.")
        exit(1)

    if args.amase_delay is not None:
        amase_delay = args.amase_delay
    elif DELAY_YAML_KEY in loaded_yaml[AMASE_YAML_KEY].keys():
        amase_delay = int(loaded_yaml[AMASE_YAML_KEY][DELAY_YAML_KEY])
    else:
        amase_delay = AMASE_DELAY

    return (scenario_file, amase_delay)


def run_amase(scenario_file: str, example_dir: str, amase_dir: str) -> subprocess.Popen:
    """Run the OpenAMASE part of the example."""
    amase_cmd = [
        "java",
        "-Xmx2048m",
        "-splash:%s" % os.path.join("data", "amase_splash.png"),
        "-classpath",
        "%s:%s" % (os.path.join("dist", "*"), os.path.join("lib", "*")),
        "avtas.app.Application",
        "--config",
        os.path.join("config", "amase"),
        "--scenario",
        os.path.join(example_dir, scenario_file),
    ]

    logging.info(
        "Running OpenAMASE in\n"
        f"             {amase_dir}\n"
        f"         with scenario '{scenario_file}'."
    )
    logging.debug(
        f"Run: cd {os.path.join(amase_dir, 'OpenAMASE')}; {' '.join(amase_cmd)}"
    )

    return subprocess.Popen(amase_cmd, cwd=os.path.join(amase_dir, "OpenAMASE"))


def check_uxas(loaded_yaml: Dict[str, Any], args: Namespace) -> List[Dict[str, str]]:
    """
    Check the OpenUxAS configuration in the YAML and return a list of configs.

    Calls `check_one_uxas` and thus may immediately exit.
    """
    uxas_configs = list()

    if UXASES_YAML_KEY in loaded_yaml.keys():
        for record in loaded_yaml[UXASES_YAML_KEY]:
            uxas_configs += [check_one_uxas(record, args)]
    elif UXAS_YAML_KEY in loaded_yaml.keys():
        uxas_configs += [check_one_uxas(loaded_yaml[UXAS_YAML_KEY], args)]

    return uxas_configs


def find_uxas_bin(bin_name: str) -> Optional[str]:
    """
    Attempt to find the path the given binary.

    Look for the binary in this order:
      1. on the user's path
      2. in the expected locally-built location
      3. in the expected release anod directory

    Note that items 2 and 3 are language specific.
    """
    if shutil.which(bin_name) is not None:
        return shutil.which(bin_name)

    if bin_name == "uxas":
        local_bin_path = UXAS_BIN
        anod_bin_path = os.path.join(
            SBX_DIR, "x86_64-linux", "uxas-release", "install", "bin", "uxas"
        )
    elif bin_name == "uxas-ada":
        local_bin_path = UXAS_ADA_BIN
        anod_bin_path = os.path.join(
            SBX_DIR, "x86_64-linux", "uxas-ada-release", "install", "bin", "uxas-ada"
        )
    else:
        # We don't know how to handle this language
        return None

    if os.path.exists(local_bin_path):
        return local_bin_path
    elif os.path.exists(anod_bin_path):
        return anod_bin_path
    else:
        return None


MISSING_BIN = """\
The command `{bin}` cannot be found on your path. Either specify the absolute
path to the desired OpenUXAS binary in the config file (*not recommended*),
manually add the desired OpenUxAS binary to your path, perfom a local build of
the binary (e.g., for C++, `make -j all`) or use anod to build the desired
version of OpenUxAS with, e.g., for C++:

    {anod} build {bin}
"""


def check_one_uxas(record: Dict[str, str], args: Namespace) -> Dict[str, str]:
    """
    Check one OpenUxAS configuration from the YAML and return the config.

    If any errors are found, print a message and immediately exit.
    """
    if CONFIG_YAML_KEY not in record.keys():
        logging.critical("OpenUxAS configuration must specify a config file.")
        exit(1)

    config_file = record[CONFIG_YAML_KEY]
    if not os.path.exists(os.path.join(example_dir, config_file)):
        logging.critical(f"Specified config file '{config_file}' does not exist.")
        exit(1)

    run_dir_name = RUN_DIR
    if RUNDIR_YAML_KEY in record.keys():
        run_dir_name = record[RUNDIR_YAML_KEY]

    if BIN_YAML_KEY in record.keys():
        bin_name = record[BIN_YAML_KEY]
    else:
        bin_name = "uxas"

    uxas_binary = find_uxas_bin(bin_name)
    if uxas_binary is None:
        logging.critical(MISSING_BIN.format(bin=bin_name, anod=ANOD_CMD))
        exit(1)

    return {
        "config_file": config_file,
        "run_dir_name": run_dir_name,
        "bin": uxas_binary,
    }


def run_uxas(
    uxas_configs: List[Dict[str, str]], example_dir: str, popen: bool
) -> List[subprocess.Popen]:
    """Run an OpenUxAS instance for each configuration."""
    pids = list()
    for config in uxas_configs:
        pid = run_one_uxas(config, example_dir, popen)
        if pid is not None:
            pids.append(pid)

    if popen:
        return pids
    else:
        return list()


def run_one_uxas(
    uxas_config: Dict[str, str], example_dir: str, popen: bool
) -> Optional[subprocess.Popen]:
    """Run one OpenUxAS instance."""
    config_file = uxas_config["config_file"]
    run_dir = uxas_config["run_dir_name"]
    uxas_bin = uxas_config["bin"]

    uxas_cmd = [uxas_bin, "-cfgPath", os.path.join(example_dir, config_file)]

    run_dir = os.path.join(example_dir, run_dir)
    pathlib.Path(run_dir).mkdir(parents=True, exist_ok=True)

    if popen:
        logging.info(
            "Running OpenUxAS binary\n"
            f"             {uxas_bin}\n"
            "         in a separate process with configuration\n"
            f"             {config_file}\n"
            "         Data and logfiles are in:\n"
            f"             {run_dir}"
        )
        logging.debug(f"Run: cd {run_dir}; {' '.join(uxas_cmd)}")

        return subprocess.Popen(uxas_cmd, cwd=run_dir)
    else:
        logging.info(
            "Running OpenUxAS binary\n"
            f"             {uxas_bin}\n"
            "         with configuration\n"
            f"             {config_file}\n"
            "         Data and logfiles are in:\n"
            f"             {run_dir}"
        )
        logging.debug(f"Run: cd {run_dir}; {' '.join(uxas_cmd)}")

        subprocess.run(uxas_cmd, cwd=run_dir)
        return None


def killall_uxases(popen: bool, pids: List[subprocess.Popen]) -> None:
    """Gracefully quit any instances of OpenUxAS that we started."""
    # We only worry about killing off UxAS instances if we Popened them
    if popen and len(pids) != 0:
        logging.info("Shutting down forked OpenUxAS processes.")
        for pid in pids:
            pid.terminate()

            # Seems as though we may need to wait a moment before
            # polling.
            time.sleep(0.1)
            if pid.poll() is None:
                pid.kill()

                # Here especially I've seen "Unable to kill ..." when
                # the process was actually killed, so we wait a moment.
                time.sleep(0.1)
                if pid.poll() is None:
                    logging.error(f"  Unable to kill PID {pid.pid}.")


# From
# https://stackoverflow.com/questions/34352405/python-argparse-help-like-option
class _ListAction(argparse.Action):
    def __init__(
        self,
        option_strings,
        dest=argparse.SUPPRESS,
        default=argparse.SUPPRESS,
        help=None,
    ):
        super(_ListAction, self).__init__(
            option_strings=option_strings,
            dest=dest,
            default=default,
            nargs=0,
            help=help,
        )

    def __call__(self, parser, namespace, values, option_string=None):
        # namespace contains arguments parsed thus far
        examples_dir = resolve_examples_dir(namespace)

        print(f"In '{examples_dir}' the following examples are available:\n")

        list_examples(examples_dir)

        # Note that this terminates the program.
        parser.exit()


class _CompleteOptionsAction(argparse.Action):
    def __init__(
        self,
        option_strings,
        dest=argparse.SUPPRESS,
        default=argparse.SUPPRESS,
        help=None,
    ):
        super(_CompleteOptionsAction, self).__init__(
            option_strings=option_strings,
            dest=dest,
            default=default,
            nargs=0,
            help=help,
        )

    def __call__(self, parser, namespace, values, option_string=None):
        # namespace contains arguments parsed thus far
        list_examples(resolve_examples_dir(namespace))

        # Note that this terminates the program.
        parser.exit()


# Script processing.
if __name__ == "__main__":
    ap = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=DESCRIPTION,
    )

    ap.add_argument(
        "--delay",
        dest="amase_delay",
        type=int,
        help="number of seconds to wait after starting OpenAMASE"
        "before starting instances of OpenUxAS",
    )

    ap.add_argument(
        "--amase-dir",
        help="absolute path to the OpenAMASE repository containing build outputs",
    )

    ap.add_argument("--uxas-bin", help="absolute path to the OpenUxAS binary")

    ap.add_argument(
        "--uxas-dir",
        default=OPENUXAS_ROOT,
        help="absolute path to the OpenUxAS repository containing build outputs",
    )

    ap.add_argument("--examples-dir", help="absolute path to the root of the examples")

    ap.add_argument(
        "-l",
        "--list",
        dest="list_examples",
        default=False,
        action=_ListAction,
        help="list known examples. Subsequent arguments are ignored, so put "
        "after `--examples-dir` if it is used.",
    )

    # This is only for driving autocomplete
    ap.add_argument(
        "--complete-options",
        default=False,
        action=_CompleteOptionsAction,
        help=argparse.SUPPRESS,
    )

    ap.add_argument(
        "example",
        help="the example directory",
    )

    add_logging_group(ap)

    args = ap.parse_args()

    # Logging level
    if args.verbose == 1:
        level = logging.INFO
    elif args.verbose == 2:
        level = logging.DEBUG
    else:
        level = args.loglevel

    # Try to use e3 for logging, but don't require that the user have e3.
    try:
        import e3.log

        e3.log.activate(
            level=level,
            filename=args.log_file,
            e3_debug=(level == logging.DEBUG),
        )
    except ImportError:
        configure_logging(args, level)

    # For KeyboardInterrupt handling
    popen = False
    pids = list()

    try:
        examples_dir = resolve_examples_dir(args)

        # Allow the user to specify a complete absolute or relative path to
        # the example, rather than relying on the example search path. The
        # advantage to the user here is that they can leverage autocomplete
        if os.path.isabs(args.example):
            example_dir = args.example
        else:
            example_dir = os.path.join(EXAMPLES_DIR, args.example)

        logging.info("Running example in\n" f"           {example_dir}")

        if not os.path.exists(example_dir):
            example_dir = os.path.join(examples_dir, args.example)

            if not os.path.exists(example_dir):
                logging.critical(
                    f"Example '{args.example}' does not exist in\n"
                    f"           {examples_dir}\n"
                    "        Use the `--list` option for a list of available examples."
                )
                exit(1)

        yaml_filename = os.path.join(example_dir, CONFIG_FILE)
        if not os.path.exists(yaml_filename):
            logging.critical(
                f"Example '{args.example}' is not property configured.\n"
                f"        There is no '{CONFIG_FILE}' in the example directory.\n"
                "        Use the `--list` option for a list of available examples."
            )
            exit(1)

        loaded_yaml = read_yaml(yaml_filename)

        amase_dir = resolve_amase_dir(args)
        (scenario_file, amase_delay) = check_amase(loaded_yaml, example_dir, args)

        uxas_configs = check_uxas(loaded_yaml, args)

        if scenario_file:
            amase_pid = run_amase(scenario_file, example_dir, amase_dir)

            if amase_delay > 0:
                print(
                    f"Waiting for {amase_delay} seconds while AMASE starts; "
                    "press 'Play' as soon as it does."
                )
                time.sleep(amase_delay)

        popen = scenario_file is not None
        pids = run_uxas(uxas_configs, example_dir, popen)

        if scenario_file:
            # Wait for the user to close AMASE
            amase_pid.wait()

        killall_uxases(popen, pids)

    except KeyboardInterrupt:
        print(" ")
        killall_uxases(popen, pids)

    except Exception as e:
        logging.critical(f"Got an exception {e}")
        traceback.print_exc()
        ap.print_usage()
