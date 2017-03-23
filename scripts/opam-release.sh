#!/usr/bin/env bash

set -o pipefail

USERNAME="$(git config deploy.username)"
TOKEN="$(git config deploy.token)"

# Make a new release on GitHub, get the ID

RELEASE_ID=$(\
curl -H 'Accept: application/vnd.github.v3+json' \
    --user "${USERNAME}:${TOKEN}" \
    -X POST --data "{\"tag_name\": \"${version}\"}" \
    https://api.github.com/repos/facebook/reason/releases \
    | python -c 'import sys, json; print json.load(sys.stdin)["id"]' \
)




# Add reason-parser asset

SUBPKG=reason-parser
TARNAME="${SUBPKG}-${version}.tar.gz"

# pre_release and make tarball of subpkg
pushd "${SUBPKG}" && version="${version}" make pre_release && popd
tar -cvzf "${TARNAME}" "${SUBPKG}"

ASSET_DOWNLOAD_URL=$(\
curl -H 'Accept: application/vnd.github.v3+json' \
    -H 'Content-Type: application/gzip' \
    --user "${USERNAME}:${TOKEN}" \
    -X POST \
    -F "${TARNAME}=@${TARNAME}" \
    "https://uploads.github.com/repos/facebook/reason/releases/${RELEASE_ID}/assets?name=${TARNAME}" \
    | python -c 'import sys, json; print json.load(sys.stdin)["browser_download_url"]' \
)




echo "In order to publish ${SUBPKG}, execute the following two commands: \
    1) opam-publish prepare ${ASSET_DOWNLOAD_URL} \
    2) opam-publish submit ${SUBPKG}.${version} \
The former will prepare a directory in your local folder and the latter will \
submit a pull request to the opam repository."

echo

echo "In order to publish reason proper, execute the following two commands: \
    1) opam-publish prepare https://github.com/facebook/reason/archive/${version}.tar.gz \
    2) opam-publish submit reason.${version} \
The former will prepare a directory in your local folder and the latter will \
submit a pull request to the opam repository."
