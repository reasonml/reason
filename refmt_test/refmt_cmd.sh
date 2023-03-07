#!/usr/bin/env bash
set -euf

REFMT="$( readlink -f ./../src/refmt/refmt_impl.exe )"
$REFMT "$@"
