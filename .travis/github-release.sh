#!/bin/bash

# Copyright (c) 2014 Terry Burton
# https://github.com/terryburton/travis-github-release

# Edit: uHOOCCOOHu @github 2016.6

set -e

RELEASEOS=$1 && shift
REPO=$1 && shift
RELEASEFILES=$@
RELEASE=$TRAVIS_TAG

if [ -z "$RELEASEOS" ]; then
  echo "Not release."
  exit 0
fi

if ! TAG=`git describe --exact-match --tags 2>/dev/null`; then
  echo "This commit is not a tag so not creating a release"
  exit 0
fi

if [ "$TRAVIS" = "true" ] && [ -z "$TRAVIS_TAG" ]; then
  echo "This build is not for the tag so not creating a release"
  exit 0
fi

if [[ -z "$RELEASEFILES" ]]; then
  echo "Error: No release files provided"
  exit 1
fi

SCRIPTDIR=`dirname $0`
[ -e "$SCRIPTDIR/GITHUBTOKEN" ] && . "$SCRIPTDIR/GITHUBTOKEN"
if [[ -z "$GITHUBTOKEN" ]]; then
  echo "Error: GITHUBTOKEN is not set"
  exit 1
fi

echo "Creating GitHub release for $RELEASE"

echo -n "Create draft release... "
JSON=$(cat <<EOF
{
  "tag_name":         "$TAG",
  "target_commitish": "master",
  "name":             "$TAG",
  "draft":            true,
  "prerelease":       false
}
EOF
)
RESULT=`curl -s -w "\n%{http_code}\n"     \
  -H "Authorization: token $GITHUBTOKEN"  \
  -d "$JSON"                              \
  "https://api.github.com/repos/$REPO/releases"`
if [ "`echo "$RESULT" | tail -1`" != "201" ]; then
  echo FAILED
  echo "$RESULT"
  exit 1
fi
RELEASEID=`echo "$RESULT" | sed -ne 's/^  "id": \(.*\),$/\1/p'`
if [[ -z "$RELEASEID" ]]; then
  echo FAILED
  echo "$RESULT"
  exit 1
fi
echo DONE

for FILE in $RELEASEFILES; do
  if [ ! -f $FILE ]; then
    echo "Warning: $FILE not a file"
    continue
  fi
  FILESIZE=`wc -c <"$FILE"`
  FILENAME=`basename $FILE`
  echo -n "Uploading $FILENAME... "
  RESULT=`curl -s -w "\n%{http_code}\n"                   \
    -H "Authorization: token $GITHUBTOKEN"                \
    -H "Accept: application/vnd.github.manifold-preview"  \
    -H "Content-Type: application/zip"                    \
    --data-binary "@$FILE"                                \
    "https://uploads.github.com/repos/$REPO/releases/$RELEASEID/assets?name=$FILENAME-$RELEASEOS&size=$FILESIZE"`
  if [ "`echo "$RESULT" | tail -1`" != "201" ]; then
    echo FAILED
    echo "$RESULT"
    exit 1
  fi
  echo DONE
done
