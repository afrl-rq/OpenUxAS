"""Common path definitions for installing."""
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
    )
)

# Now we read the environment and fall back on rebuilding the paths manually.
OPENUXAS_ROOT = os.environ.get("OPENUXAS_ROOT", FALLBACK_REPO_DIR)
INFRASTRUCTURE_DIR = os.environ.get(
    "INFRASTRUCTURE_DIR", os.path.join(OPENUXAS_ROOT, "infrastructure")
)
INFRASTRUCTURE_UXAS = os.path.join(INFRASTRUCTURE_DIR, "uxas")
INSTALL_LIBEXEC_DIR = os.environ.get(
    "INSTALL_LIBEXEC_DIR", os.path.join(INFRASTRUCTURE_DIR, "install-libexec")
)
SOFTWARE_DIR = os.environ.get(
    "SOFTWARE_PATH", os.path.join(INFRASTRUCTURE_DIR, "software")
)
GNAT_DIR = os.environ.get("GNAT_DIR", os.path.join(SOFTWARE_DIR, "gnat"))

VPYTHON_DIR = os.environ.get("VPYTHON_DIR", os.path.join(OPENUXAS_ROOT, ".vpython"))
