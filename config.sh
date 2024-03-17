#!/bin/bash

set -e
set -x

if [ -z "$GRAALVM_HOME" ]; then
    echo "Please set GRAALVM_HOME"
    exit 1
fi

if [ ! -e "${GRAALVM_HOME}/bin/native-image" ]; then
    echo "Your ${GRAALVM_HOME} does not have native-image, aborting."
    exit 1
fi

export JAVA_HOME=$GRAALVM_HOME
export PATH=$GRAALVM_HOME/bin:$PATH

which java
java -version

clojure -T:build compile

# not currently needed
java -agentlib:native-image-agent=config-output-dir=config -cp "$(clojure -Spath):target/classes" pod.tddpirate.membrane

clojure -T:build fix-config
