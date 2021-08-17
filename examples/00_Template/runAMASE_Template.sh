here=$PWD;

cd $AMASE_SOURCE_DIR/OpenAMASE;

java -Xmx2048m -splash:./data/amase_splash.png -classpath ./dist/*:./lib/*  avtas.app.Application --config config/amase --scenario "$here/Scenario_Template.xml";
cd "$here";

