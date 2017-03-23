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

pushd _build &&
tar -cvzf "${TARNAME}" "${SUBPKG}" &&
ASSET_DOWNLOAD_URL=$(\
curl -H 'Accept: application/vnd.github.v3+json' \
    -H 'Content-Type: application/gzip' \
    --user "${USERNAME}:${TOKEN}" \
    -X POST \
    --data-binary "@${TARNAME}" \
    "https://uploads.github.com/repos/facebook/reason/releases/${RELEASE_ID}/assets?name=${TARNAME}" \
    | python -c 'import sys, json; print json.load(sys.stdin)["browser_download_url"]' \
) && popd




echo "The build artifacts are now in _build. To continue, please cd there."
echo

echo "In order to publish ${SUBPKG}, execute the following two commands:"
echo "    1) opam-publish prepare ${ASSET_DOWNLOAD_URL}"
echo "    2) opam-publish submit ${SUBPKG}.${version}"
echo "The former will prepare a directory in your local folder and the latter"
echo "will submit a pull request to the opam repository."

echo

echo "In order to publish reason proper, execute the following two commands:"
echo "    1) opam-publish prepare https://github.com/facebook/reason/archive/${version}.tar.gz"
echo "    2) opam-publish submit reason.${version}"
echo "The former will prepare a directory in your local folder and the latter"
echo "will submit a pull request to the opam repository."
