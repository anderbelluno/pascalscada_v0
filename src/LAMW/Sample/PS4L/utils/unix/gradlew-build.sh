export PATH=/lamw_manager/LAMW/sdk/platform-tools:$PATH
export GRADLE_HOME=/lamw_manager/LAMW/gradle-8.5
export PATH=$PATH:$GRADLE_HOME/bin
. ~/.bashrc
gradlew build
