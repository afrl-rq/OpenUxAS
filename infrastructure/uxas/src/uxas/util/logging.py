"""
Logging support for OpenUxAS.

These are meant to be used by scripts that are not wholly dependent on e3.

Typical usage will create an instance of an argument parser and then call
`add_logging_group` after setting all other arguments. Then, after parsing
arguments, call `activate_logger` using `get_logging_level` for the second
parameter.
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from argparse import ArgumentParser, Namespace


# For log consistency with our other usages.
STREAM_FMT = "%(levelname)-8s %(message)s"
FILE_FMT = "%(asctime)s: %(name)-24s: %(levelname)-8s %(message)s"


def add_logging_group(argument_parser: ArgumentParser) -> None:
    """
    Add a group and arguments to control the log.

    Use with `uxas.logging.configure_logging`.
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

    To be used with `uxas.logging.add_logging_group`.
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


def get_logging_level(args: Namespace) -> int:
    """
    Get the logging level from the arguments.

    If `logging.WARNING` is not the default logging level, you should not call
    this and instead work out the log-level logic yourself.
    """
    # Logging level
    if args.verbose == 1:
        return logging.INFO
    elif args.verbose == 2:
        return logging.DEBUG
    else:
        return args.loglevel


def activate_logger(args: Namespace, level: int) -> None:
    """
    Select between e3's or python's logger and set the logging level.

    To be used with `uxas.logging.add_logging_group`.
    """
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
