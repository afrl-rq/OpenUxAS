"""Common path definitions for anod."""
import os

# Note that the utility of this particular variable is limited, because it will
# be relative to *the installation* of the module in the user's vpython. The
# best we can do is make some assumptions.
#
# An alternative would be for paths to fail fast if the environment variables
# aren't set. I'm not sure how I feel about that.
__ROOT_DIR = os.path.dirname(os.path.abspath(__file__))

FALLBACK_REPO_DIR = os.path.realpath(
    os.path.join(
        __ROOT_DIR,
        "..",
        "..",
        "..",
        "..",
        "..",
        "..",
    )
)

# Now we read the environment and fall back on rebuilding the paths manually.
OPENUXAS_ROOT = os.environ.get("OPENUXAS_ROOT", FALLBACK_REPO_DIR)

ANOD_BIN = os.environ.get("ANOD_BIN", os.path.join(OPENUXAS_ROOT, "anod"))

DOC_DIR = os.environ.get("DOC_DIR", os.path.join(OPENUXAS_ROOT, "doc"))
EXAMPLES_DIR = os.environ.get("EXAMPLES_DIR", os.path.join(OPENUXAS_ROOT, "examples"))
INFRASTRUCTURE_DIR = os.environ.get(
    "INFRASTRUCTURE_DIR", os.path.join(OPENUXAS_ROOT, "infrastructure")
)
MDMS_DIR = os.environ.get("MDMS_DIR", os.path.join(OPENUXAS_ROOT, "mdms"))
OBJ_DIR = os.environ.get("OBJ_DIR", os.path.join(OPENUXAS_ROOT, "obj"))
RESOURCES_DIR = os.environ.get(
    "RESOURCES_DIR", os.path.join(OPENUXAS_ROOT, "resources")
)
SRC_DIR = os.environ.get("SRC_DIR", os.path.join(OPENUXAS_ROOT, "src"))
TESTS_DIR = os.environ.get("TESTS_DIR", os.path.join(OPENUXAS_ROOT, "tests"))

CPP_DIR = os.environ.get("CPP_DIR", os.path.join(SRC_DIR, "cpp"))
ADA_DIR = os.environ.get("ADA_DIR", os.path.join(SRC_DIR, "ada"))

UXAS_BIN = os.environ.get("UXAS_BIN", os.path.join(OPENUXAS_ROOT, "cpp", "uxas"))
UXAS_ADA_BIN = os.environ.get("UXAS_ADA_BIN", os.path.join(ADA_DIR, "uxas-ada"))

# For LmcpGen and OpenAMASE
SUPPORT_DIR = os.environ.get("SUPPORT_DIR", os.path.join(OPENUXAS_ROOT, "develop"))
LMCP_DIR = os.environ.get("LMCP_DIR", os.path.join(SUPPORT_DIR, "LmcpGen"))
AMASE_DIR = os.environ.get("AMASE_DIR", os.path.join(SUPPORT_DIR, "OpenAMASE"))

VPYTHON_DIR = os.environ.get("VPYTHON_DIR", os.path.join(OPENUXAS_ROOT, ".vpython"))
VPYTHON_ACTIVATE = os.environ.get(
    "VPYTHON_ACTIVATE", os.path.join(VPYTHON_DIR, "bin", "activate")
)

SOFTWARE_DIR = os.environ.get(
    "SOFTWARE_DIR", os.path.join(INFRASTRUCTURE_DIR, "software")
)
GNAT_DIR = os.environ.get("GNAT_DIR", os.path.join(SOFTWARE_DIR, "gnat"))

SPEC_DIR = os.environ.get("SPEC_DIR", os.path.join(INFRASTRUCTURE_DIR, "specs"))
SBX_DIR = os.environ.get("SBX_DIR", os.path.join(INFRASTRUCTURE_DIR, "sbx"))
