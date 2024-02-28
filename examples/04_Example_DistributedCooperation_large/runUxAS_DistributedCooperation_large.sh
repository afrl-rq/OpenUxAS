#! /bin/bash

#save the current directory
SAVE_DIR=$(pwd)

#location of OpenAMASE
BIN2="../../infrastructure/sbx/x86_64-linux/amase/src/OpenAMASE"
# run OpenAMASE in separate terminal.  Note: requires "gnome-terminal"
cd $BIN2
/usr/bin/gnome-terminal -x java -Xmx2048m -splash:./data/amase_splash.png -classpath ./dist/*:./lib/*  avtas.app.Application --config config/daidalus --scenario "../../../../../../examples/04_Example_DistributedCooperation_large/Scenario_DistributedCooperation.xml"; 

sleep 5s
# change back to original directory
cd $SAVE_DIR

#location of the UxAS binary (executable)
BIN="../../../obj/cpp/uxas"

#set the UAV ID
UAV=1000
#define the runinnig directory
RUN_DIR=UAV_${UAV}
#remove old data
rm -Rf ${RUN_DIR} 
#add new data directory
mkdir -p ${RUN_DIR}
# change to the data directory
cd ${RUN_DIR}
# run UxAS is a separate terminal. Note: requires "gnome-terminal"
/usr/bin/gnome-terminal -e $BIN" -cfgPath ../cfgDistributedCooperation_$UAV.xml"
# change back to the original directory
cd $SAVE_DIR

#set the UAV ID
UAV=2000
#define the runinnig directory
RUN_DIR=UAV_${UAV}
#remove old data
rm -Rf ${RUN_DIR} 
#add new data directory
mkdir -p ${RUN_DIR}
# change to the data directory
cd ${RUN_DIR}
# run UxAS is a separate terminal. Note: requires "gnome-terminal"
/usr/bin/gnome-terminal -e $BIN" -cfgPath ../cfgDistributedCooperation_$UAV.xml"
# change back to the original directory
cd $SAVE_DIR

#set the UAV ID
UAV=4208
#define the runinnig directory
RUN_DIR=UAV_${UAV}
#remove old data
rm -Rf ${RUN_DIR} 
#add new data directory
mkdir -p ${RUN_DIR}
# change to the data directory
cd ${RUN_DIR}
# run UxAS is a separate terminal. Note: requires "gnome-terminal"
/usr/bin/gnome-terminal -e $BIN" -cfgPath ../cfgDistributedCooperation_$UAV.xml"
# change back to the original directory
cd $SAVE_DIR
