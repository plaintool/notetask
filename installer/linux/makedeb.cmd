SET "SOURCE_DIR=%~dp0"
FOR /F "usebackq delims=" %%i IN ("%SOURCE_DIR%..\..\VERSION") DO SET "VERSION=%%i"

tar -czf control.tar.gz -C CONTROL .

tar -czf data.tar.gz -C DATA .

ar r notetask-%VERSION%.deb debian-binary control.tar.gz data.tar.gz

del control.tar.gz
del data.tar.gz