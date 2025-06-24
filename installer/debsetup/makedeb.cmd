tar -czf control.tar.gz -C CONTROL .

tar -czf data.tar.gz -C DATA .

ar r notetask.deb debian-binary control.tar.gz data.tar.gz

del control.tar.gz
del data.tar.gz