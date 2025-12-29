export JAVA_HOME=/lamw_manager/LAMW/jdk/zulu-default
cd /files/pascalscada_v0/src/LAMW/Sample/PS4L
LC_ALL=C keytool -genkey -v -keystore ps4l-release.keystore -alias ps4l.keyalias -keyalg RSA -keysize 2048 -validity 10000 < /files/pascalscada_v0/src/LAMW/Sample/PS4L/keytool_input.txt
