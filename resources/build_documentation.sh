#!/bin/bash
# Copyright © 2017 Government of the United States of America, as represented by the Secretary of the Air Force.
# No copyright is claimed in the United States under Title 17, U. S. Code. All Other Rights Reserved.
# Copyright 2017 University of Cincinnati. All rights reserved. See LICENSE.md file at:
# https://github.com/afrl-rq/OpenUxAS
# Additional copyright may be held by others, as reflected in the commit history.

ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
OPENUXAS_ROOT="$( realpath "${ROOT_DIR}/.." )"


echo "Installing dependencies ..."

# references:
# * http://stackoverflow.com/questions/3466166/how-to-check-if-running-in-cygwin-mac-or-linux/17072017#17072017
# * https://serverfault.com/questions/501230/can-not-seem-to-get-expr-substr-to-work

if [ "$(uname)" == "Darwin" ]; then
    # Install doxygen and related packages: in terminal
    brew install doxygen graphviz
    brew cask install mactex
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    # Install doxygen and related packages: in terminal
    sudo apt -y install doxygen graphviz texlive-full
else
    echo "Unsupported platform! Script install only for Linux and Mac"
    exit 1
fi

echo "Generating User Manual..."
# run this at: OpenUxAS/doc/reference/UserManual
cd "${OPENUXAS_ROOT}/doc/reference/UserManual"
pdflatex UxAS_UserManual.tex

echo "Creating HTML Doxygen reference documentation..."
# run this at: OpenUxAS/doc/doxygen
cd "${OPENUXAS_ROOT}/doc/doxygen"
sh RunDoxygen.sh

echo "...Congratulations! You're done with building the documentation!"
echo " "
echo "To view the user manual, load "
echo "  \"${OPENUXAS_ROOT}/doc/reference/UserManual/UxAS_UserManual.pdf\""
echo " "
echo "To view the Doxygen reference document, load "
echo "  \"${OPENUXAS_ROOT}/doc/doxygen/html/index.html\""
echo "in your browser. E.g.:"
echo "  firefox \"${OPENUXAS_ROOT}/doc/doxygen/html/index.html\" &"
echo " "
