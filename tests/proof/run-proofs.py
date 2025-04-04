#! /usr/bin/env python3

import sys
import os
from e3.testsuite import Testsuite
from e3.testsuite.driver.diff import DiffTestDriver
from uxas.paths import ADA_DIR


class GnatproveDriver(DiffTestDriver):
    """Driver to run GNATprove"""

    def run(self):
        filenames = self.test_env.get ("filenames")
        gnatprove_level = self.test_env.get ("level")
        gnatprove_timeout = self.test_env.get ("timeout")

        if filenames != None:
            if self.env.options.no_replay:
                proof_switches = []
                if gnatprove_level != None:
                    proof_switches+= ["--level="+str(gnatprove_level)]
                if gnatprove_timeout != None:
                    proof_switches+=["--timeout="+str(gnatprove_timeout)]
            else:
                proof_switches=["--replay"]

            gpr_file = os.path.join (ADA_DIR, "afrl_ada_dev.gpr")

            self.shell(["gnatprove", "-q", "-P", gpr_file, "-j"+str(self.env.options.gnatprove_jobs)] + filenames + proof_switches,timeout=self.env.options.timeout)


class GnatproveTestsuite(Testsuite):
    test_driver_map = {"gnatprove": GnatproveDriver}
    default_driver = "gnatprove"

    @property
    def tests_subdir(self):
        return "proofs"

    # Add a command-line flag to the testsuite script to allow users to
    # trigger baseline rewriting.
    def add_options(self, ArgumentParser):
        self.main.argument_parser.add_argument(
            "--rewrite", action="store_true",
            help="Rewrite test baselines according to current outputs"
        )
        self.main.argument_parser.add_argument(
            "--timeout",
            dest="timeout",
            type=int,
            metavar="N",
            default=300,
            help="Modify the timeout of processes running gnatprove (not related to gnatprove's --timeout option)"
        )
        self.main.argument_parser.add_argument(
            "-g",
            "--gnatprove-jobs",
            dest="gnatprove_jobs",
            type=int,
            metavar="N",
            default=1,
            help="Specify the number of jobs to run simultaneously on a gnatprove instance"
        )
        self.main.argument_parser.add_argument(
            "--no-replay",
            action="store_true",
            dest="no_replay",
            help="Do not use replay mode when running the testsuite to generate new session files"
        )

    # Before running the testsuite, keep track in the environment of our
    # desire to rewrite baselines. DiffTestDriver instances will pick it up
    # automatically from there.
    def set_up(self):
        super(GnatproveTestsuite, self).set_up()
        self.env.rewrite_baselines = self.main.args.rewrite


if __name__ == "__main__":
    sys.exit(GnatproveTestsuite().testsuite_main())
