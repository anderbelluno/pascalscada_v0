export JAVA_HOME=${/usr/libexec/java_home}
export PATH=${JAVA_HOME}/bin:$PATH
cd D:\files\pascalscada_v0\src\LAMW\Sample\PS4L\utils\unix\
jarsigner -verify -verbose -certs /files/pascalscada_v0/src/LAMW/Sample/PS4L/build/outputs/apk/release/PS4L-release.apk
