#!/usr/bin/env bash

# This script allows to run refmt in a way that is compatible with both CI and development.
# CI works with opam and expects refmt to be available in $PATH
# while development works with esy and you want to always run refmt from src/refmt

set -e

# $CI is set by CircleCI
# TF_BUILD is set by Azure Pipelines
if [[ -n $CI || -n $TF_BUILD ]]; then
  refmt "$@"
else
  esy x refmt "$@"
fi
