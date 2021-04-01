#! /bin/bash

SAVE_DIR=$(pwd)

RM_DATAWORK="rm -R ./datawork"
RM_LOG="rm -R ./log"

mkdir -p RUNDIR_Template
cd RUNDIR_Template
$RM_DATAWORK
$RM_LOG
uxas -cfgPath ../cfg_Template.xml

