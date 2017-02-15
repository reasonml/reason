#!/bin/bash

RANDSTR=`head -c2 </dev/urandom|xxd -p -u`
read -p "Do you want to release? If so, please type '${RANDSTR}' (no quotes): " inp
if [ "$inp" = "$RANDSTR" ]; then
    echo "Preparing to release..."
    exit 0
else
    echo "Not releasing."
    exit 1
fi
