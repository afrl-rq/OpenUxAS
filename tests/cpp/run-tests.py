#!/usr/bin/env python
import os
import re
import sys
import logging
import socket
from queue import Queue

from e3.main import Main
from e3.fs import find, rm, mkdir, cp, ls, mv
from e3.os.fs import which
from e3.collection.dag import DAG
from e3.job.walk import Walk
from e3.job import ProcessJob
from e3.env import Env
from e3.os.process import Run

# Directory in which the run-tests script is located
ROOT_DIR = os.path.dirname(os.path.abspath(__file__))

# Directory in which tests are found
TEST_DIR = os.path.join(ROOT_DIR, 'tests')

# Result dir
RESULT_DIR = os.path.join(ROOT_DIR, 'results')

sys.path.insert(0, ROOT_DIR)


def dump_gcov_summary(source_dir: str,
                      build_dir: str,
                      gcda_dir: str,
                      display_includes_coverage: bool) -> None:
    """Display a coverage summary.

    :param source_dir: root source dir
    :param gcda_dir: directory containing the gcda files
    :param display_includes_coverage: if True display coverage summary for
        include files
    :param gcda_files: a list of gcda files to process with gcov
    :param source_files: a list of source files to consider. Coverage
        information about files not in this list will not be displayed.
    """

    # Reset the directory containing the gcov files
    gcr = os.path.join(RESULT_DIR, 'gcov')
    rm(gcr, recursive=True)
    mkdir(gcr)

    # Run gcov to produce de gcov files
    for f in find(root=build_dir, pattern='*.gcno'):
        cp(f, os.path.join(gcda_dir, os.path.relpath(f, build_dir)))

    gcno_files = find(root=gcda_dir, pattern='*.gcno')

    Run(['gcov', '-p'] + gcno_files, cwd=source_dir,
        output=os.path.join(gcr, 'gcov.out'))
    mv(os.path.join(source_dir, '*.gcov'), gcr)

    total_sources = 0
    total_covered = 0

    for gcov_file in ls(os.path.join(gcr, '*.gcov')):
        # Decode original source paths (-p option of gcov)
        source_file = os.path.basename(gcov_file).replace('#', '/')[:-5]

        # Ignores all source that are not part of the project
        if os.path.isabs(source_file):
            continue

        if not display_includes_coverage and \
                (source_file.endswith('.h') or source_file.endswith('.hpp')):
            continue

        with open(gcov_file) as fd:
            total = 0
            covered = 0
            for line in fd:
                if re.match(r' *-:', line):
                    pass
                elif re.match(r' *[#=]{5}:', line):
                    total += 1
                else:
                    total += 1
                    covered += 1

        # Update global counters
        total_sources += total
        total_covered += covered

        # Display file information
        if total == 0:
            percent = 0.0
        else:
            percent = float(covered) * 100.0 / float(total)

        logging.info('%6.2f %% %8d/%-8d %s',
                     percent,
                     covered,
                     total,
                     source_file)

    # Display global counters
    if total_sources == 0:
        percent = 0.0
    else:
        percent = float(total_covered) * 100.0 / float(total_sources)

    logging.info('%6.2f %% %8d/%-8d %s',
                 float(total_covered) * 100.0 / float(total_sources),
                 total_covered,
                 total_sources,
                 'TOTAL')


class TestJob(ProcessJob):
    """Handle a test execution."""

    PORTS: Queue = Queue()

    @property
    def cmdline(self):
        """See e3.job.ProcessJob."""
        return [sys.executable, self.data.test_path]

    def on_start(self, scheduler):
        logging.info("[%-10s %-9s %4ds] %s",
                     self.queue_name, 'start', 0, self.data)
        self.in_port = self.PORTS.get()
        self.out_port = self.PORTS.get()
        logging.debug('Reserve ports: %s, %s', self.in_port, self.out_port)

    def on_finish(self, scheduler):
        self.PORTS.put(self.in_port)
        self.PORTS.put(self.out_port)
        logging.debug('Release ports: %s, %s', self.in_port, self.out_port)

    @property
    def cmd_options(self):
        """See e3.job.ProcessJob."""

        # Compute the default urls used by the bridge
        return {'output': os.path.join(RESULT_DIR, self.uid + '.out'),
                'ignore_environ': False,
                'env': {'IN_SERVER_URL':
                        'tcp://127.0.0.1:%s' % self.in_port,
                        'OUT_SERVER_URL':
                        'tcp://127.0.0.1:%s' % self.out_port}}


class TestData(object):
    """Handle test data related to a given test.

    This the data associated with each test job
    """

    def __init__(self, uid: str, test_path: str) -> None:
        """Initialize a test data.

        :param uid: the test uid
        :param test_path: path to the test.py file
        """
        self.uid = uid
        self.test_path = test_path

    def __str__(self) -> str:
        """Compute a string representation for display purpose."""
        return self.uid


class TestsuiteLoop(Walk):
    """The testsuite test scheduler."""

    def __init__(self, actions, jobs):
        self.jobs = jobs
        self.next_port = 5560
        super(TestsuiteLoop, self).__init__(actions)

    def create_job(self, uid, data, predecessors, notify_end):
        """See Walk.create_job doc."""
        return TestJob(uid, data, notify_end)

    def find_port(self) -> int:
        """Find the next available port.

        :return: an available port
        """
        port = None
        start_port = self.next_port

        # Do a maximum of 100 attempts.
        for _ in range(100):
            try:
                s = socket.socket()
                s.bind(('127.0.0.1', self.next_port))
                s.close()
                port = self.next_port
                self.next_port += 1
                break
            except OSError:
                self.next_port += 1

        if port is None:
            raise OSError("cannot find a valid port in range: %s - %s" %
                          (start_port, self.next_port - 1))
        logging.debug('allocate port: %s', port)
        return port

    def set_scheduling_params(self):
        """See Walk.set_scheduling_params doc."""
        super(TestsuiteLoop, self).set_scheduling_params()
        self.tokens = self.jobs

        for _ in range(self.jobs * 4):
            # For each worker we allocate 4 ports. Each job will use 2 ports.
            # By allocating twice the necessary number we ensure that ports
            # are not reused immediatly. This ensure that the system has time
            # to release the port once the process using it is finished and
            # thus avoid "Port in used" errors
            TestJob.PORTS.put(self.find_port())

        self.job_timeout = 60


def get_test_uid(path: str) -> str:
    """Compute the test uid from its path.

    :param path: path to the test.py implementing the test
    :return: an unique uid that does not contain path separators
    """
    return os.path.dirname(
        os.path.relpath(path,
                        TEST_DIR)).replace('/', '.').replace('\\', '.')


def get_test_list() -> DAG:
    """Fetch the list of tests and return a DAG.

    :return: a dag representing the tests to perform.
    """
    test_dag = DAG()
    test_list = find(root=TEST_DIR, pattern='test.py')

    for test in test_list:
        test_dag.add_vertex(
            get_test_uid(test),
            data=TestData(uid=get_test_uid(test), test_path=test))
    return test_dag


def main() -> int:
    """Main of the testsuite driver.

    :return: 0 in case of success
    """
    m = Main()
    m.argument_parser.add_argument(
        '--source-dir',
        metavar="DIR",
        default=os.environ.get('UXAS_SOURCE_DIR'),
        help="root directory containing uxas sources. When set a coverage "
        "summary will be displayed. Default is the value of the env var "
        "UXAS_SOURCE_DIR: %s" % os.environ.get('UXAS_SOURCE_DIR'))
    m.argument_parser.add_argument(
        '--build-dir',
        metavar="DIR",
        default=os.environ.get('UXAS_BUILD_DIR'),
        help="root directory containing uxas build. When set a coverage "
        "summary will be displayed. Default is the value of the env var "
        "UXAS_BUILD_DIR: %s" % os.environ.get('UXAS_BUILD_DIR'))
    m.argument_parser.add_argument(
        '--jobs',
        type=int,
        default=Env().build.cpu.cores,
        help="Set parallelism (default: %s)" % Env().build.cpu.cores)
    m.argument_parser.add_argument(
        '--display-includes-coverage',
        default=False,
        action="store_true",
        help="If used coverage summary will show coverage information of "
        "include files (.h)")

    m.parse_args()

    try:
        import zmq  # noqa: F401 (ignore warning from flake8)
    except ImportError:
        logging.critical("zmp package is required. do pip install zmq")
        return 1

    uxas_bin = which('uxas')
    if not uxas_bin:
        logging.critical("uxas executable should be in the path")
        return 1
    else:
        logging.info("uxas found in %s", uxas_bin)

    rm(RESULT_DIR, recursive=True)
    mkdir(RESULT_DIR)
    Env().add_search_path('PYTHONPATH', ROOT_DIR)

    # Ensure gcda are stored in the testsuite dir. It ensures that we don't
    # pollute uxas build dir and ease reset of coverage info on each run.
    if (m.args.source_dir and m.args.build_dir
            and len(find(m.args.build_dir, "*.gc*")) > 0):
        logging.info('Enable coverage mode')
        logging.info('Sources: %s', m.args.source_dir)
        logging.info('Objects: %s', m.args.build_dir)
        gcda_dir = os.path.join(RESULT_DIR, 'gcda')
        rm(gcda_dir, recursive=True)
        mkdir(gcda_dir)
        os.environ['GCOV_PREFIX'] = gcda_dir
        os.environ['GCOV_PREFIX_STRIP'] = \
            str(len(m.args.source_dir.split(os.sep)) - 1)

    TestsuiteLoop(actions=get_test_list(), jobs=m.args.jobs)

    if (m.args.source_dir is not None and m.args.build_dir is not None
            and len(find(m.args.build_dir, "*.gc*")) > 0):
        dump_gcov_summary(m.args.source_dir,
                          m.args.build_dir,
                          gcda_dir,
                          m.args.display_includes_coverage)

    return 0


if __name__ == '__main__':
    sys.exit(main())
