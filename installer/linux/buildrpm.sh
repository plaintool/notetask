#!/bin/bash

# Install rpm-build
sudo dnf install rpm-build

# Run rpmbuild with fixed paths
rpmbuild -bb --buildroot ~/rpmbuild/BUILDROOT ~/notetask.spec
