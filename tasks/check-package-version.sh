#!/usr/bin/env bash

# This script checks if the package version has changed since the last release.
# Usage: check-package-version.sh [package_name] [do_publish_var_name] [new_version_var_name]
#
# It will print these to standard output:
# ${do_publish_var_name}=0|1
# ${new_version_var_name}=x.y.z # only when ${do_publish_var_name}=1

package_name=$1
do_publish_var_name=$2
new_version_var_name=$3
if ! published_version=$(npm show @superfluid-finance/"$package_name"@latest version); then
    echo "Querying current published version failed." >&2
    exit 1
fi
new_version=$(jq -r .version packages/"$package_name"/package.json)

if [ "$published_version" != "$new_version" ]; then
    echo "$package_name changed: $published_version -> $new_version" >&2
    echo "$do_publish_var_name=1"
    echo "$new_version_var_name=$new_version"
else
    echo "$package_name unchanged: $published_version" >&2
    echo "$do_publish_var_name=0"
fi
