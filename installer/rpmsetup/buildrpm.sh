#!/bin/bash
# Run rpmbuild with fixed paths
rpmbuild -bb --buildroot ~/rpmbuild/BUILDROOT ~/notetask.spec
