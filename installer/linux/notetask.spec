Name:           notetask
Version:        %{?version}
Release:        2%{?dist}
Summary:        Notetask
License:        GPLv3
BuildArch:      x86_64
Requires:       gtk2

%define _build_name_fmt %%{name}-%%{version}.rpm

%description
A simple application for creating and organizing task lists

%prep

%build

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}
cp -a "%{staging_dir}/." "%{buildroot}/"

%files -f %{_sourcedir}/notetask.files
