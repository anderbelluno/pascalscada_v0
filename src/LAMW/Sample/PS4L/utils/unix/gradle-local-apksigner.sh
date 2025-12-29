export PATH=/lamw_manager/LAMW/sdk/platform-tools:$PATH
export PATH=/lamw_manager/LAMW/sdk/build-tools/30.0.3:$PATH
export GRADLE_HOME=/lamw_manager/LAMW/gradle-8.5
export PATH=$PATH:$GRADLE_HOME/bin
zipalign -v -p 4 /files/pascalscada_v0/src/LAMW/Sample/PS4L/build/outputs/apk/release/PS4L-arm64-v8a-release-unsigned.apk /files/pascalscada_v0/src/LAMW/Sample/PS4L/build/outputs/apk/release/PS4L-arm64-v8a-release-unsigned-aligned.apk
apksigner sign --ks /files/pascalscada_v0/src/LAMW/Sample/PS4L/ps4l-release.keystore --ks-pass pass:123456 --key-pass pass:123456 --out /files/pascalscada_v0/src/LAMW/Sample/PS4L/build/outputs/apk/release/PS4L-release.apk /files/pascalscada_v0/src/LAMW/Sample/PS4L/build/outputs/apk/release/PS4L-arm64-v8a-release-unsigned-aligned.apk
