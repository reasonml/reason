#!/usr/bin/env bash

set -e
set -o pipefail

USERNAME="$(git config deploy.username)"
TOKEN="$(git config deploy.token)"

# Make a new release on GitHub, get the ID

RELEASE_ID=$(\
curl --silent -H 'Accept: application/vnd.github.v3+json' \
    --user "${USERNAME}:${TOKEN}" \
    -X POST --data "{\"tag_name\": \"${version}\"}" \
    https://api.github.com/repos/facebook/reason/releases \
    | python -c 'import sys, json; print json.load(sys.stdin)["id"]' \
)

echo

echo "The build artifacts are now in _build. To continue, please cd there."
echo

echo "In order to publish reason, execute the following two commands:"
echo "    1) opam-publish prepare https://github.com/facebook/reason/archive/${version}.tar.gz"
echo "    2) opam-publish submit reason.${version}"
echo "The former will prepare a directory in your local folder and the latter"
echo "will submit a pull request to the opam repository."

echo

echo "In order to publish to npm, execute the following commands:"
echo "    npm publish --access public"
echo "This will publish a npm package with the latest version."
