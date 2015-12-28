#!/bin/bash 
# This script trains a model and validates it.

inifile=$1
ROOT=`dirname $0`/..
echo "Started on: "`hostname`
echo "Using ini-File: "$inifile
echo "ROOT="$ROOT
echo "PATH="$PATH
echo "JAVA_HOME="$JAVA_HOME

export MYCLASSPATH="$ROOT/target/proj.bionlp-1.0-SNAPSHOT-jar-with-dependencies.jar:$ROOT/3rdparty/parser/stanford-parser/stanford-parser-2010-02-26/stanford-parser.jar:$ROOT/3rdparty/factorie/factorie-0.8.1-SNAPSHOT.jar:$ROOT/3rdparty/scalala/scalala-0.2.2-iesl.jar:$ROOT/3rdparty/fastutil/fastutil-5.1.5.jar:$ROOT/3rdparty/jfreechart/jfreechart-1.0.9.jar:$ROOT/3rdparty/xmlgraphics"

# only needed to run, not to compile:
export MYCLASSPATH=$MYCLASSPATH:$ROOT/3rdparty/jfreechart/jfreechart-1.0.9.jar
export MYCLASSPATH=$MYCLASSPATH:$ROOT/3rdparty/xmlgraphics/xmlgraphics-commons-1.3.1.jar
export MYCLASSPATH=$MYCLASSPATH:$ROOT/3rdparty/jcommon/jcommon-1.0.12.jar

export VMOPTS="-Xmx8g -XX:MaxPermSize=500m -XX:+UseParNewGC -XX:+UseConcMarkSweepGC -server"

cd $ROOT

$JAVA_HOME/bin/java $VMOPTS -cp $MYCLASSPATH cc.refectorie.bionlp.Workflow2 $inifile

