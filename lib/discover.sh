#! /bin/sh

set -e 
set -u

if [ "$(uname)" == "Darwin" ]; then
  JAVAC_HOME="$(dirname $(readlink $(which javac)))/java_home"
  FOUND_OS_FOLDER="darwin"
else
  JAVAC_HOME="$(dirname $(dirname $(readlink -f $(which javac))))"
  FOUND_OS_FOLDER="linux"
fi
JAVA_HOME=${JAVA_HOME:=$JAVAC_HOME}
OS_FOLDER=${OS_FOLDER:=$FOUND_OS_FOLDER}

echo "( \"-I\" \"$JAVA_HOME/include\" \"-I\" \"$JAVA_HOME/include/$OS_FOLDER\" )" > c_flags.sexp
