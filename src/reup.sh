#!/usr/bin/env bash
# Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

function usage { echo "$(basename $0) [-h] [-w <print-width>] -d <path> -f <x.y.z> -- upgrade syntax from version specified by -f to the latest version installed in the system

where:
    -h: show this text
    -w <print-width=90>: change print-width of upgraded files
    -d <path>: source code directory to be upgraded
    -b <path>: path for backup directory
    -f <x.y.z>: current version of the source file to be upgraded
    " 1>&2; exit 1;
}

REASON_GIT="git@github.com:facebook/Reason.git"
MERLINEXTEND_GIT="git@github.com:def-lkb/merlin-extend.git"

OPAM_BIN=$(opam config var bin)

PRINTWIDTH=90

function install_refmt {
    VERSION=$1
    if [[ -f $OPAM_BIN/refmt-$VERSION ]];
    then
        echo "refmt-$VERSION already exists at $OPAM_BIN/refmt-$VERSION, skipping installation"
        return 0
    fi
    read -p "refmt-$VERSION is needed but not found at $OPAM_BIN/refmt-$VERSION, do you want me to install it? [Y/n]:" -n 1 -r
    if [[ ! $REPLY =~ ^[Yy]$ ]]
    then
        exit 1
    fi
    BUILD_DIR=$(mktemp -d -t reason_upgrade.XXXXXXXX)
    OPAM_DIR="$BUILD_DIR/opam"
    REASON_DIR="$BUILD_DIR/reason"
    MERLINEXTEND_DIR="$BUILD_DIR/merlin-extend"
    echo "Initing opam at $OPAM_DIR"
    opam init --quiet -n --root $OPAM_DIR
    if [[ $? -ne 0 ]]; then
        echo "Couldn't init opam at $OPAM_DIR, exiting" 1>&2
        exit 1;
    fi
    git clone $REASON_GIT --branch $VERSION --depth 1 $REASON_DIR;
    if [[ $? -ne 0 ]]; then
        echo "Couldn't clone reason from $REASON_GIT to $REASON_DIR, exiting" 1>&2
        exit 1;
    fi

    git clone $MERLINEXTEND_GIT --depth 1 $MERLINEXTEND_DIR;
    if [[ $? -ne 0 ]]; then
        echo "Couldn't clone merlin-extend from $MERLINEXTEND_GIT to $MERLINEXTEND_DIR, exiting" 1>&2
        exit 1;
    fi
    opam pin add $MERLINEXTEND_DIR --root $OPAM_DIR -y
    if [[ $? -ne 0 ]]; then
        echo "Couldn't opam pin $MERLINEXTEND_DIR into $OPAM_DIR" 1>&2
        exit 1;
    fi

    opam pin add $REASON_DIR --root $OPAM_DIR -y
    if [[ $? -ne 0 ]]; then
        echo "Couldn't opam pin $REASON_DIR into $OPAM_DIR" 1>&2
        exit 1;
    fi

    echo "Installing refmt-$VERSION to OPAM_BIN/refmt-$VERSION"

    OPAM_BIN_SANDBOX=$(opam config var bin --root $OPAM_DIR)

    cp $OPAM_BIN_SANDBOX/refmt $OPAM_BIN/refmt-$VERSION

    echo "Removing sandbox $OPAM_BIN_SANDBOX"

    rm -rf $OPAM_BIN_SANDBOX
}

while getopts ':hf:d:w:b:' option; do
    case "$option" in
        h) usage
           ;;
        d) DIR=$OPTARG
           ;;
        b) BACKUP_DIR=$OPTARG
           ;;
        f) FROM=$OPTARG
           ;;
        w) PRINTWIDTH=$OPTARG
           ;;
    esac
done
shift $((OPTIND - 1))

if [[ -z $DIR ]];
then
    echo "No -d provided" 1>&2
    usage
fi

DIR=`cd "$DIR"; pwd`

if [[ -z $FROM ]];
then
    echo "No -f <from_version> provided" 1>&2
    usage
fi

if [[ ! $FROM =~ ^([0-9]\.[0-9]\.[0-9])$ ]];
then
    echo "version provided by -f should be in the form of x.y.z" 1>&2
    usage
fi

install_refmt $FROM

if [[ -z $BACKUP_DIR ]];
then
    echo "No -b <backup_dir> specified, default to use $DIR.backup"
    BACKUP_DIR=$DIR.backup
fi

if [[ -d $BACKUP_DIR ]];
then
    echo "Fail to backup: $BACKUP_DIR already exists, exiting" 1>&2
    usage
fi

echo "Backing up at $BACKUP_DIR"
cp -af $DIR $BACKUP_DIR

find $DIR -type f -name "*.re" | while read file; do
    set -x
    $OPAM_BIN/refmt-$VERSION -print binary_reason $file | $OPAM_BIN/refmt -print-width $PRINTWIDTH -use-stdin true -is-interface-pp false -parse binary_reason -print re > $file.new
    mv -f $file.new $file
    set +x
done

set -x
find $DIR -type f -name "*.rei" | while read file; do
    $OPAM_BIN/refmt-$VERSION -is-interface-pp true -print binary_reason $file | $OPAM_BIN/refmt -is-interface-pp true -print-width $PRINTWIDTH -use-stdin true -parse binary_reason -print re > $file.new
    mv -f $file.new $file
done
set +x

echo "Done. Original files are backed up at $BACKUP_DIR"
