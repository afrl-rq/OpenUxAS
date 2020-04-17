set -e
set -x

mkdir -p $TOOLS_DIR
cd $TOOLS_DIR

echo $TRAVIS_BUILD_DIR

# Clone OpenUxAS-bootstrap repository, pull the last changes if it exists
if ! [ -d OpenUxAS-bootstrap ]
then
    git clone https://github.com/AdaCore/OpenUxAS-bootstrap.git;
    envsubst < $TRAVIS_BUILD_DIR/.travis-bootstrap.patch
    patch -d$TOOLS_DIR/OpenUxAS-bootstrap/ -p0 < $TRAVIS_BUILD_DIR/.travis-bootstrap.patch;

else
   (cd OpenUxAS-bootstrap && git pull)
fi

# Install the sandbox (we're not using the install_env script because
# Travis python is already a virtual environment.
pip install wheel
pip install zmq
pip install git+https://github.com/AdaCore/e3-core.git

# If not already present, download the GNAT Community installer and the helper
# scripts to use it headless.
if ! [ -f $GNAT_INSTALLER ]
then
    wget $GNAT_LINUX_INSTALLER_URL -O $GNAT_INSTALLER
fi
if ! [ -d gnat_community_install_script ]
then
   git clone https://github.com/AdaCore/gnat_community_install_script.git;
else
   (cd gnat_community_install_script && git pull)
fi

# If not already installed, install GNAT Community. The script does not work if
# the installation directory already exists, so remove it first.
if ! [ -f "$INSTALL_DIR/bin/gprbuild" ]
then
   rm -rf "$INSTALL_DIR"
   sh gnat_community_install_script/install_package.sh \
      "$GNAT_INSTALLER" "$INSTALL_DIR"
fi

# Log info about the toolchain we use
which gprbuild && gprbuild --version
