#!/usr/bin/env bash

die () { echo "${1}"; exit 1; }

USERNAME_MSG="You have not set your deploy username. Please set it with 'git \
config deploy.username YOUR_GITHUB_USERNAME_HERE'."
TOKEN_MSG="You have not set your deploy token. Please create one with at \
least public repo read permissions at https://github.com/settings/tokens and \
set it with 'git config deploy.token YOUR_TOKEN_HERE'."
[[ -z "$(git config deploy.username)" ]] && die "$USERNAME_MSG"
[[ -z "$(git config deploy.token)" ]] && die "$TOKEN_MSG"

USERNAME="$(git config deploy.username)"
TOKEN="$(git config deploy.token)"

# Check that the current HEAD is on origin/master.
HEAD=`git rev-parse --verify HEAD`
MASTER=`git rev-parse --verify master`
if [ "${MASTER}" != "${HEAD}" ]; then
    echo "** WARNING: You are not on master! If this is a mistake, please abort \
the release. **"
fi

read -p "STOP! Have you made sure to release the sub packages? (y/n) " yn
if [ "${yn}" != "y" ]; then
    die "Not releasing."
fi

MASTERURL="https://api.github.com/repos/facebook/reason/git/refs/heads/master"
HEADERR="Current HEAD is not on upstream master. This is a requirement before releasing.
If you are sure it is, try switching branches to master and pulling changes."
HEADOK="Current HEAD is on upstream master."
echo "Checking HEAD against upstream master..."
curl --silent --user "${USERNAME}:${TOKEN}" "${MASTERURL}" | grep "sha" | grep "${HEAD}" > /dev/null && echo "${HEADOK}" || die "${HEADERR}"

# Confirm that the user actually means to release.
RANDSTR=`head -c2 </dev/urandom | xxd -plain -u` # -u is uppercase
echo "Do you want to release? This *WILL* publish to GitHub AND npm!!!"
read -p "If so, please type '${RANDSTR}' (no quotes). Otherwise, type anything else: " inp
if [ "${inp}" = "${RANDSTR}" ]; then
    echo "Preparing to release..."
    exit 0
else
    die "Not releasing."
fi
