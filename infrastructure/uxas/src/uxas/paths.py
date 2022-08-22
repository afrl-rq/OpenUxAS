"""Common path definitions for anod."""
import os

# Note that the utility of this particular variable is limited, because it will
# be relative to *the installation* of the module in the user's vpython. The
# best we can do is make some assumptions.
#
# An alternative would be for paths to fail fast if the environment variables
# aren't set. I'm not sure how I feel about that.
__ROOT_DIR = os.path.dirname(os.path.abspath(__file__))

# Fall back by splitting our path on .vpython and taking the front part. This
# should dump us at the root of the OpenUxAS repo and is hopefully more robust
# than walking a relative path a fixed number of steps.
FALLBACK_REPO_DIR = os.path.realpath(__ROOT_DIR.split(".vpython")[0])

# Now we read the environment and fall back on rebuilding the paths manually.
OPENUXAS_ROOT = os.environ.get("OPENUXAS_ROOT", FALLBACK_REPO_DIR)
"""Path to the root of the OpenUxAS repository."""

ANOD_BIN = os.environ.get("ANOD_BIN", os.path.join(OPENUXAS_ROOT, "anod"))
"""Path to the anod binary."""

DOC_DIR = os.environ.get("DOC_DIR", os.path.join(OPENUXAS_ROOT, "doc"))
"""Path to the documentation directory."""

EXAMPLES_DIR = os.environ.get("EXAMPLES_DIR", os.path.join(OPENUXAS_ROOT, "examples"))
"""Path to the examples directory."""

INFRASTRUCTURE_DIR = os.environ.get(
    "INFRASTRUCTURE_DIR", os.path.join(OPENUXAS_ROOT, "infrastructure")
)
"""Path to the infrastructure directory."""

MDMS_DIR = os.environ.get("MDMS_DIR", os.path.join(OPENUXAS_ROOT, "mdms"))
"""Path to the (root) MDMs directory."""

OBJ_DIR = os.environ.get("OBJ_DIR", os.path.join(OPENUXAS_ROOT, "obj"))
"""Path to the object directory."""

RESOURCES_DIR = os.environ.get(
    "RESOURCES_DIR", os.path.join(OPENUXAS_ROOT, "resources")
)
"""Path to the resources directory."""

SRC_DIR = os.environ.get("SRC_DIR", os.path.join(OPENUXAS_ROOT, "src"))
"""Path to the source directory."""

TESTS_DIR = os.environ.get("TESTS_DIR", os.path.join(OPENUXAS_ROOT, "tests"))
"""Path to the tests directory."""

CPP_DIR = os.environ.get("CPP_DIR", os.path.join(SRC_DIR, "cpp"))
"""Path to the C++ sources directory."""

ADA_DIR = os.environ.get("ADA_DIR", os.path.join(SRC_DIR, "ada"))
"""Path to the Ada sources directory."""

UXAS_BIN = os.environ.get("UXAS_BIN", os.path.join(OPENUXAS_ROOT, "cpp", "uxas"))
"""Path to the uxas binary."""

UXAS_ADA_BIN = os.environ.get("UXAS_ADA_BIN", os.path.join(ADA_DIR, "uxas-ada"))
"""Path to the uxas-ada binary."""

SUPPORT_DIR = os.environ.get("SUPPORT_DIR", os.path.join(OPENUXAS_ROOT, "develop"))
"""Path to the support/development directory."""

DEFAULT_LMCP_DEVEL_DIR = os.path.join(SUPPORT_DIR, "LmcpGen")
"""Path the the default location of the LMCP development directory."""

LMCP_DEVEL_DIR = os.environ.get("LMCP_DEVEL_DIR", DEFAULT_LMCP_DEVEL_DIR)
"""Path to the LMCPgen development directory."""

DEFAULT_AMASE_DEVEL_DIR = os.path.join(SUPPORT_DIR, "OpenAMASE")
"""Path the the default location of the AMASE development directory."""

AMASE_DEVEL_DIR = os.environ.get("AMASE_DEVEL_DIR", DEFAULT_AMASE_DEVEL_DIR)
"""Path to the OpenAMASE development directory."""

VPYTHON_DIR = os.environ.get("VPYTHON_DIR", os.path.join(OPENUXAS_ROOT, ".vpython"))
"""Path to the vpython directory."""

VPYTHON_ACTIVATE = os.environ.get(
    "VPYTHON_ACTIVATE", os.path.join(VPYTHON_DIR, "bin", "activate")
)
"""Path to the vpython activate script."""

SOFTWARE_DIR = os.environ.get(
    "SOFTWARE_DIR", os.path.join(INFRASTRUCTURE_DIR, "software")
)
"""Path to the installed software directory."""

GNAT_DIR = os.environ.get("GNAT_DIR", os.path.join(SOFTWARE_DIR, "gnat"))
"""Path to the GNAT installation directory."""

SPEC_DIR = os.environ.get("SPEC_DIR", os.path.join(INFRASTRUCTURE_DIR, "specs"))
"""Path to the anod specs directory."""

SBX_DIR = os.environ.get("SBX_DIR", os.path.join(INFRASTRUCTURE_DIR, "sbx"))
"""Path to the sandbox directory."""

AMASE_DIR = os.environ.get("AMASE_DIR", os.path.join(SBX_DIR, "amase", "src"))
"""Path to the OpenAMASE source directory (in the sandbox)."""

LMCP_DIR = os.environ.get("LMCP_DIR", os.path.join(SBX_DIR, "lmcpgen", "src"))
"""Path to the LMCPgen source directory (in the sandbox)."""

REPOSITORIES_YAML = os.environ.get(
    "REPOSITORIES_YAML", os.path.join(SPEC_DIR, "config", "repositories.yaml")
)
"""Path to the repositories.yaml file."""
