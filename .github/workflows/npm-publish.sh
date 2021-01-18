#!/bin/bash -e

[ -f "$1"/package.json ] || { echo "Invalid package directory: $1"; exit 1; }
cd "$1"

git diff --quiet || { echo "There are unstaged local changes"; exit 1; }
git diff --cached --quiet || { echo "There are staged local changes"; exit 1; }

VERSION=`awk -F'"' '/"version": ".+"/{ print $4; exit; }' package.json`

# if version is ended with -latest, replace it with an unique latest-TIME-GIT_REV
if echo "$VERSION" | fgrep -q -- "-latest";then
    TIME=`date -u +%Y%m%dT%H%M%SZ`
    GIT_REV=`git rev-parse --short HEAD`

    PUBLISHING_VERSION=${VERSION%-latest*}-latest-${TIME}-${GIT_REV}

    jq -r ". |= . + {
        \"version\" : \"$PUBLISHING_VERSION\"
    }" package.json > package.json.new #&& \
        mv package.json.new package.json
else
    PUBLISHING_VERSION=VERSION
fi

echo "Publishing pacakge, version $PUBLISHING_VERSION"

# publish to the private github registry
cat > .npmrc <<EOF
@superfluid-finance:registry=https://npm.pkg.github.com
//npm.pkg.github.com/:_authToken=${GITHUB_TOKEN}
always-auth=true
EOF
npm publish --access public --dry-run

# publish to the npmjs registry
rm -f .npmrc
npm publish --access public --dry-run

# revert the changes to the package.json if any
git checkout package.json
