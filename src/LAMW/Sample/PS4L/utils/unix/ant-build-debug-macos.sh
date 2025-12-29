export PATH=/lamw_manager/LAMW/apache-ant-1.10.5/bin:$PATH
export JAVA_HOME=${/usr/libexec/java_home}
export PATH=${JAVA_HOME}/bin:$PATH
cd D:\files\pascalscada_v0\src\LAMW\Sample\PS4L\utils\unix\
ant -Dtouchtest.enabled=true debug
