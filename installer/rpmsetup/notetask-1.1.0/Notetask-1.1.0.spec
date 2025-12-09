Buildroot: /home/tas/notetask-1.0
Name: Notetask
Version: 1.0
Release: 2
Summary: A simple application for creating and organizing task lists
License: see /usr/share/doc/notetask/copyright
Distribution: Fedora
Group: Converted/base

%define _rpmdir ../
%define _rpmfilename %%{NAME}-%%{VERSION}-%%{RELEASE}.%%{ARCH}.rpm
%define _unpackaged_files_terminate_build 0

%description
(Converted from a deb package by alien version 8.95.)

%files
%dir /usr/share/applications
%dir /usr/share/icons/hicolor/64x64/mimetypes
%dir /usr/share/icons/hicolor/64x64/apps
%dir /usr/share/icons/hicolor/128x128/mimetypes
%dir /usr/share/icons/hicolor/128x128/apps
%dir /usr/share/mime/packages
%dir /usr/share/pixmaps

/usr/bin/notetask
/usr/share/applications/x-notetask.desktop
/usr/share/pixmaps/notetask.png
/usr/share/mime/packages/x-notetask.xml
/usr/share/icons/hicolor/64x64/mimetypes/taskdoc.png
/usr/share/icons/hicolor/64x64/apps/x-notetask.png
/usr/share/icons/hicolor/128x128/mimetypes/taskdoc.png
/usr/share/icons/hicolor/128x128/apps/x-notetask.png

