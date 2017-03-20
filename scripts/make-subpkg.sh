#!/usr/bin/env bash

SUBPKG="${1}"
VERSION="${2}"

TARNAME="${SUBPKG}-${VERSION}.tar.gz"

pushd "${SUBPKG}" && version="${VERSION}" make pre_release && popd
tar -cvzf "${TARNAME}" "${SUBPKG}"
md5 "${TARNAME}"
