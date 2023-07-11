#!/bin/bash

# This script checks if the package version has changed since the last release.
# If the version has changed, it will set the $package_name=1 
package_name=$1
published_version=$(npm show @superfluid-finance/"$package_name"@latest version)
new_version=$(jq -r .version packages/"$package_name"/package.json)

if [ "$published_version" != "$new_version" ]; then
    echo "$package_name changed: $published_version -> $new_version"
    echo "$package_name=1" >> "$GITHUB_ENV"
else
    echo "$package_name unchanged: $published_version"
fi

echo "$package_name=$new_version" >> "$GITHUB_OUTPUT"