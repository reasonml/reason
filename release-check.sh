#!/bin/bash

RANDSTR=`head -c2 </dev/urandom|xxd -p -u`
echo "Do you want to release? This *WILL* publish to GitHub AND npm!!!"
read -p "If so, please type '${RANDSTR}' (no quotes): " inp
if [ "$inp" = "$RANDSTR" ]; then
    echo "Preparing to release..."
    exit 0
else
    echo "Not releasing."
    exit 1
fi
