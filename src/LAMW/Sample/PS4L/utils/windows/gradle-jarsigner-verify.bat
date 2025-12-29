set JAVA_HOME=C:\lamw_manager\LAMW\jdk\zulu-default
path %JAVA_HOME%\bin;%path%
cd D:\files\pascalscada_v0\src\LAMW\Sample\PS4L\utils\windows\
jarsigner -verify -verbose -certs D:\files\pascalscada_v0\src\LAMW\Sample\PS4L\build\outputs\apk\release\PS4L-release.apk
