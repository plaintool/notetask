Buildroot: ~/rpmbuild/BUILDROOT
Name: notetask
Version: 1.1.1
Release: 2
Summary: A simple application for creating and organizing task lists
License: see /usr/share/doc/notetask/copyright
Distribution: Fedora
Group: Converted/base
Requires: gtk2

%define _build_name_fmt %%{name}-%%{version}.rpm
%define _unpackaged_files_terminate_build 0

%description
Notetask

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}/usr/bin
mkdir -p %{buildroot}/usr/share/applications
mkdir -p %{buildroot}/usr/share/pixmaps
mkdir -p %{buildroot}/usr/share/mime/packages
mkdir -p %{buildroot}/usr/share/icons/hicolor/64x64/mimetypes
mkdir -p %{buildroot}/usr/share/icons/hicolor/64x64/apps
mkdir -p %{buildroot}/usr/share/icons/hicolor/128x128/mimetypes
mkdir -p %{buildroot}/usr/share/icons/hicolor/128x128/apps

cp -p ~/notetask-1.1.1/usr/bin/notetask %{buildroot}/usr/bin/
cp -p ~/notetask-1.1.1/usr/share/applications/x-notetask.desktop %{buildroot}/usr/share/applications/
cp -p ~/notetask-1.1.1/usr/share/pixmaps/notetask.png %{buildroot}/usr/share/pixmaps/
cp -p ~/notetask-1.1.1/usr/share/mime/packages/x-notetask.xml %{buildroot}/usr/share/mime/packages/
cp -p ~/notetask-1.1.1/usr/share/icons/hicolor/64x64/mimetypes/taskdoc.png %{buildroot}/usr/share/icons/hicolor/64x64/mimetypes/
cp -p ~/notetask-1.1.1/usr/share/icons/hicolor/64x64/apps/x-notetask.png %{buildroot}/usr/share/icons/hicolor/64x64/apps/
cp -p ~/notetask-1.1.1/usr/share/icons/hicolor/128x128/mimetypes/taskdoc.png %{buildroot}/usr/share/icons/hicolor/128x128/mimetypes/
cp -p ~/notetask-1.1.1/usr/share/icons/hicolor/128x128/apps/x-notetask.png %{buildroot}/usr/share/icons/hicolor/128x128/apps/

chmod +x %{buildroot}/usr/bin/notetask

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
