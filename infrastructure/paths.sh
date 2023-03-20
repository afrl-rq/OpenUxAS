# Globally-defined repository paths and helper functions.
#
# This file should be sourced by any script that needs to work with repository
# paths: this way, if the layout changes, we can more easily manage the impact.

# The root of the OpenUxAS repository. For this, because we may get here via
# a source, we need to be careful about how we compute the path. Note no
# support for KSH at present.
if [[ -n $ZSH_EVAL_CONTEXT ]]; then
    export OPENUXAS_ROOT="$( cd "${0:a:h}/.." && pwd )"
else
    export OPENUXAS_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/.. >/dev/null 2>&1 && pwd )"
fi

: "${ANOD_BIN:=${OPENUXAS_ROOT}/anod}"

: "${DOC_DIR:=${OPENUXAS_ROOT}/doc}"
: "${EXAMPLES_DIR:=${OPENUXAS_ROOT}/examples}"
: "${INFRASTRUCTURE_DIR:=${OPENUXAS_ROOT}/infrastructure}"
: "${MDMS_DIR:=${OPENUXAS_ROOT}/mdms}"
: "${OBJ_DIR:=${OPENUXAS_ROOT}/obj}"
: "${RESOURCES_DIR:=${OPENUXAS_ROOT}/resources}"
: "${SRC_DIR:=${OPENUXAS_ROOT}/src}"
: "${TESTS_DIR:=${OPENUXAS_ROOT}/tests}"

: "${CPP_DIR:=${SRC_DIR}/cpp}"
: "${ADA_DIR:=${SRC_DIR}/ada}"

: "${UXAS_BIN:=${OBJ_DIR}/cpp/uxas}"
: "${UXAS_ADA_BIN:=${ADA_DIR}/uxas-ada}"

: "${VPYTHON_DIR:=${OPENUXAS_ROOT}/.vpython}"
: "${VPYTHON_ACTIVATE:=${VPYTHON_DIR}/bin/activate}"

: "${INSTALL_LIBEXEC_DIR:=${INFRASTRUCTURE_DIR}/install-libexec}"
: "${SOFTWARE_DIR:=${INFRASTRUCTURE_DIR}/software}"
: "${GNAT_DIR:=${SOFTWARE_DIR}/gnat}"

: "${SPEC_DIR:=${INFRASTRUCTURE_DIR}/specs}"
: "${SBX_DIR:=${INFRASTRUCTURE_DIR}/sbx}"
: "${ARCH_DIR:=${SBX_DIR}/x86_64-linux}"

: "${LMCP_DIR:=${ARCH_DIR}/lmcpgen/src}"
: "${AMASE_DIR:=${ARCH_DIR}/amase/src}"

: "${SUPPORT_DIR:=${OPENUXAS_ROOT}/develop}"
: "${LMCP_DEVEL_DIR:=${SUPPORT_DIR}/LmcpGen}"
: "${AMASE_DEVEL_DIR:=${SUPPORT_DIR}/OpenAMASE}"


# Try to activate the python venv.
#
# If the venv does not exist, try to install the venv and then activate it.
# If the venv cannot be activated, exit with an error.
function activate_venv {
    if [ -f ${VPYTHON_ACTIVATE} ]; then
        # Activate the virtual environment
        debug_and_run "source \"${VPYTHON_ACTIVATE}\""
    else
        # This is here so that make doesn't halt with no visible output while
        # anod tries to install itself.
        if [[ -n $NO_INSTALL_VENV ]]; then
            exit 1
        fi

        echo "It looks like this is your first time running scripts in this OpenUxAS clone."
        echo "Let's install the infrastructure support on which anod & scripts depend."
        echo " "
        echo "To do this, we will use apt. We will update the index and install all needed"
        echo "packages automatically. If you would prefer more control over how dependencies"
        echo "are installed, simply say 'n' and manually run the installer to be guided"
        echo "through all of the options."
        echo " "
        echo "Note: as part of this process, we will install Java 11 using apt. If you have"
        echo "already installed any other version of Java, you may wish to say 'n' and"
        echo "manually run the installer so that you can skip this step."
        echo " "
        echo -n "Okay to proceed? [Y/n]: "
        read _response

        if [[ "${_response}" != "n" ]]; then
            debug_and_run "${INFRASTRUCTURE_DIR}/install --no-gnat --automatic"

            if [ -f ${VPYTHON_ACTIVATE} ]; then
                debug_and_run "source \"${VPYTHON_ACTIVATE}\""
            else
                echo "Installing infrastructure support appears to have failed."
                exit 1
            fi
        else
            echo "Okay. You should run \`${INFRASTRUCTURE_DIR}/install\` manually."
            echo " "
            exit 1
        fi
    fi
}

# Make sure gnat is on the path or put a local GNAT CE on the path.
#
# Check for gnat availability with which. If this fails, check to see if
# GNAT CE is installed locally. If not, try to install GNAT CE. If GNAT CE
# cannot be installed, exit with an error.
function ensure_gnat {
    which gnat >/dev/null 2>&1

    if [ $? -ne 0 ]; then
        if [ -d "${GNAT_DIR}" ]; then
            debug_and_run "export PATH=\"${GNAT_DIR}/bin:${PATH}\""
        else
            echo "For this step, you need an Ada compiler to continue."
            echo "Let's install the GNAT Community compiler and support on which it depends."
            echo " "
            echo "To do this, we will use apt. We will update the index and install all needed"
            echo "packages automatically. If you would prefer more control over how dependencies"
            echo "are installed, simply say 'n' and manually run the installer to be guided"
            echo "through all of the options."
            echo " "
            echo -n "Okay to proceed? [Y/n]: "
            read _response

            if [[ "${_response}" != "n" ]]; then
                debug_and_run "${INFRASTRUCTURE_DIR}/install --no-anod --no-java --automatic"

                if [ -d "${GNAT_DIR}" ]; then
                    debug_and_run "export PATH=\"${GNAT_DIR}/bin:${PATH}\""
                else
                    echo "Installing GNAT appears to have failed."
                    exit 1
                fi
            else
                echo "Okay. You should run \`${INFRASTRUCTURE_DIR}/install\` manually."
                echo " "
                exit 1
            fi
        fi
    fi
}

# Check to see if the first positional argument in a list matches parameter.
#
# For example, to check if the first positional argument is "build", call like
# this:
#
#       if is_first_positional_arg "$*" "build"; then ...
#
# Note that "$*" is important; "$@" gets flattened into arguments (the list is
# not passed as a single argument).
function is_first_positional_arg {
    for arg in $1; do
        case ${arg} in
            -*)
                ;;

            $2)
                return 0
                ;;

            *)
                return 1
                ;;
        esac
    done
}

# Check to see if the second argument is anywhere in the list given.
#
# For example, to check to see if "uxas-ada" is present, call like this:
#
#      if contains "$*" "uxas-ada"; then
#
# Note that "$*" is important; "$@" gets flattened into arguments (the list is
# not passed as a single argument).
function contains {
    if [[ "$1" =~ (^|[[:space:]])"$2"($|[[:space:]]) ]]; then
        return 0
    else
        return 1
    fi
}

# Check to see if any argument anywhere in the list given starts with a prefix.
#
# For example, to check to see if "--qualifier=(anything)" is present:
#
#      if any_argument_starts_with "$*" "--qualifier="
#
# Note that "$*" is important; "$@" gets flattened into arguments (the list is
# not passed as a single argument).
function any_argument_starts_with {
    for arg in $1; do
        case ${arg} in
            $2*)
                return 0
                ;;

            *)
                ;;
        esac
    done

    return 1
}

# Retrieve an option from a list by its prefix.
#
# For example, to get the value of the option starting with --qualifier=:
#
#       qualifier=$( get_option "$*"" "--qualifier" )
#
# Note that "$*" is important; "$@" gets flattened into arguments (the list is
# not passed as a single argument).
function get_option {
    for arg in $1; do
        case ${arg} in
            $2*)
                echo ${arg}
                return 0
                ;;

            *)
                ;;
        esac
    done

    return 1
}

# Retrieve the first positional argument in a list.
#
# For example, to get the first positional argument:
#
#       command=$( get_first_positional_arg "$*" )
#
# Note that "$*"" is important; "$@" gets flattened into arguments (the list is
# not passed as a single argument).
function get_first_positional_arg {
    for arg in $1; do
        case ${arg} in
            -*)
                ;;

            *)
                echo ${arg}
                return 0
                ;;
        esac
    done

    return 1
}


# Determine the logging level
#
# There's a bit of a weakness here, because if the user passes -v -v, it won't
# be interpreted correctly. This could be addressed, but doesn't seem worth
# the effort at the moment.
if contains "$*" "-v"; then
    _LOGLEVEL=8  # info
elif contains "$*" "-vv"; then
    _LOGLEVEL=10 # debug
else
    _LOGLEVEL=6  # warn
fi

# Provide informational output
function info {
    if [ ${_LOGLEVEL} -ge 8 ]; then
        printf "\033[1;30mINFO\033[0m\t $*\n"
    fi
}

# Provide debug output
function debug {
    if [ ${_LOGLEVEL} -ge 10  ]; then
        printf "\033[0;36mDEBUG\033[0m\t $*\n"
    fi
}

# Print a command and run it.
#
# NOTE: the command *must* be passed as a string, not an array / sequence of
# arguments.
function debug_and_run {
    debug "Run: " $1

    if [ -z ${_DRY_RUN} ]; then
        eval $1
    fi
}
