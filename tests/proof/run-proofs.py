#! /usr/bin/env python3

import sys
import os
from e3.testsuite import Testsuite
from e3.testsuite.driver.diff import DiffTestDriver


class GnatproveDriver(DiffTestDriver):
    """Driver to run GNATprove"""

    def run(self):
        filenames = self.test_env.get ("filenames")

        if filenames != None:
            with open(self.working_dir("test.gpr"), 'w') as f_prj:
                f_prj.write('with "xmlada";\n')
                f_prj.write('with "zmq.gpr";\n')
                f_prj.write('with "lmcp_generated_messages.gpr";\n')
                f_prj.write('project Test is\n')
                f_prj.write('   for Main use ("uxas_ada.adb");\n')
                f_prj.write('   package Naming is\n')
                f_prj.write('      for Specification ("Ctrl_C_Handler") use "ctrl_c_handler.ads";\n')
                f_prj.write('      for Implementation ("Ctrl_C_Handler") use "ctrl_c_handler__dummy.adb";\n')
                f_prj.write('   end Naming;\n')
                f_prj.write('  for Source_Dirs\n')
                f_prj.write('     use ("'+self.test_env["test_dir"]+'/../../../../src/ada/src/**");\n')
                f_prj.write('   package Compiler is\n')
                f_prj.write('      for Default_Switches ("ada") use ("-O2", "-gnatn", "-gnatp", "-fdata-sections","-ffunction-sections");\n')
                f_prj.write('   end Compiler;\n')
                f_prj.write('   package Prove is\n')
                f_prj.write('      for Proof_Switches ("Ada") use ("--no-counterexample", "-q", "--replay", "-u");\n')
                f_prj.write('      for Proof_Dir use "../../..'+self.test_env["test_dir"]+'/../../../../src/ada/proof";\n')
                f_prj.write('   end Prove;\n')
                f_prj.write('end Test;\n')


            self.shell(["gnatprove", "-P", self.working_dir("test.gpr"), "-j"+str(self.env.options.gnatprove_jobs)] + filenames ,timeout=self.env.options.timeout)


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
            help="Specify the number of jobs to run simultaneously on a gnatprove instance",
        )

    # Before running the testsuite, keep track in the environment of our
    # desire to rewrite baselines. DiffTestDriver instances will pick it up
    # automatically from there.
    def set_up(self):
        super(GnatproveTestsuite, self).set_up()
        self.env.rewrite_baselines = self.main.args.rewrite


if __name__ == "__main__":
    sys.exit(GnatproveTestsuite().testsuite_main())
