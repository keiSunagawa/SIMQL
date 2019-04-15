#!/bin/bash

cd ../ && sbt "project playground" fastOptJS
cp playground/target/scala-2.12/playground-fastopt.js ../src/scala
