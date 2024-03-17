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

clojure -T:build:native-image compile

# Options which do not exist in liberica's native-image
#    -H:+UnlockExperimentalVMOptions \

native-image \
    -cp "$(clojure -Spath):target/classes" \
    -H:ConfigurationFileDirectories=config \
    -H:Name=tddpirate_membrane \
    -Djava.awt.headless=false \
    -H:+ReportExceptionStackTraces \
    -J-Dclojure.spec.skip-macros=true \
    -J-Dclojure.compiler.direct-linking=true \
    -J-Dtech.v3.datatype.graal-native=true \
    --features=clj_easy.graal_build_time.InitClojureClasses \
    --verbose \
    --no-fallback \
    pod.tddpirate.membrane
