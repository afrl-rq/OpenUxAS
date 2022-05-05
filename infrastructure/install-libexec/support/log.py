"""
Logging configuration for install scripts.

This module also contains text-wrapping functions.
"""

from __future__ import annotations

import logging
import textwrap

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from argparse import Namespace

STREAM_FMT = "%(levelname)-8s %(message)s"
FILE_FMT = "%(asctime)s: %(name)-24s: %(levelname)-8s %(message)s"


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


def wrap(s: str) -> str:
    """Dedent and wrap a string to 79 characters."""
    return textwrap.fill(textwrap.dedent(s), width=79)


def log_wrap(s: str) -> str:
    """Dedent and wrap a string to 70 characters with a 9-space hanging indent."""
    return textwrap.fill(textwrap.dedent(s), width=70, subsequent_indent=" " * 9)
