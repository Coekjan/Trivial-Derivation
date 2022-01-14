#!/bin/bash
if [ ! -d "./project" ]
then
  mkdir ./project
fi
echo 'addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.8")' > ./project/assembly.sbt
sbt clean assembly
sbt package
mv './target/scala-2.13/expr-derivation-assembly-0.1.0-SNAPSHOT.jar' './Trivial-Derivation.jar'