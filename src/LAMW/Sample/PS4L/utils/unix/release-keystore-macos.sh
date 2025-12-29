export JAVA_HOME=${/usr/libexec/java_home}
export PATH=${JAVA_HOME}/bin:$PATH
cd D:\files\pascalscada_v0\src\LAMW\Sample\PS4L\utils\unix\
keytool -genkey -v -keystore ps4l-release.keystore -alias ps4l.keyalias -keyalg RSA -keysize 2048 -validity 10000 < /files/pascalscada_v0/src/LAMW/Sample/PS4L/keytool_input.txt
