set Path=%PATH%;C:\lamw_manager\LAMW\apache-ant-1.10.5\bin
set JAVA_HOME=C:\lamw_manager\LAMW\jdk\zulu-default
cd D:\files\pascalscada_v0\src\LAMW\Sample\PS4L\utils\windows\
call ant clean -Dtouchtest.enabled=true debug
if errorlevel 1 pause
