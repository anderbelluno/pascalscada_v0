set Path=%PATH%;C:\lamw_manager\LAMW\sdk\platform-tools;C:\lamw_manager\LAMW\sdk\build-tools\30.0.3
set GRADLE_HOME=C:\lamw_manager\LAMW\gradle-8.5
set PATH=%PATH%;%GRADLE_HOME%\bin
zipalign -v -p 4 D:\files\pascalscada_v0\src\LAMW\Sample\PS4L\build\outputs\apk\release\PS4L-arm64-v8a-release-unsigned.apk D:\files\pascalscada_v0\src\LAMW\Sample\PS4L\build\outputs\apk\release\PS4L-arm64-v8a-release-unsigned-aligned.apk
apksigner sign --ks D:\files\pascalscada_v0\src\LAMW\Sample\PS4L\ps4l-release.keystore --ks-pass pass:123456 --key-pass pass:123456 --out D:\files\pascalscada_v0\src\LAMW\Sample\PS4L\build\outputs\apk\release\PS4L-release.apk D:\files\pascalscada_v0\src\LAMW\Sample\PS4L\build\outputs\apk\release\PS4L-arm64-v8a-release-unsigned-aligned.apk
