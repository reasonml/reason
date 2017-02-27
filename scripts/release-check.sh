#!/usr/bin/env bash

die () { echo "${1}"; exit 1; }

# Check that the current HEAD is on origin/master.
HEAD=`git rev-parse --verify HEAD`
MASTERURL="https://api.github.com/repos/facebook/reason/git/refs/heads/master"
HEADERR="Current HEAD is not on upstream master. This is a requirement before releasing.
If you are sure it is, try switching branches to master and pulling changes."
HEADOK="Current HEAD is on upstream master."
echo "Checking HEAD against upstream master..."
curl --silent "${MASTERURL}" | grep "sha" | grep "${HEAD}" > /dev/null && echo "${HEADOK}" || die "${HEADERR}"

# Confirm that the user actually means to release.
RANDSTR=`head -c2 </dev/urandom | xxd -plain -u` # -u is uppercase
echo "Do you want to release? This *WILL* publish to GitHub AND npm!!!"
read -p "If so, please type '${RANDSTR}' (no quotes). Otherwise, type anything else: " inp
if [ "$inp" = "${RANDSTR}" ]; then
    echo "Preparing to release..."
    exit 0
else
    die "Not releasing."
fi
