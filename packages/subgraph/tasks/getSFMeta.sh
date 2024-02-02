# This files is a bash script that is used to generate the src/meta.ignore.ts file.
# This is used to fill out the SFMeta entity (see schema.graphql)
# The different fields: `${SOME_FIELD}` must be set in the enivronment when running this script.
#!/usr/bin/env bash
BRANCH="$(git branch --show-current)"

cat > src/meta.ignore.ts << EOF
export let commitHash = "${COMMIT_HASH}";
export let configuration = "${CONFIGURATION}";
export let branch = "${BRANCH}";
export let packageVersion = "${PACKAGE_VERSION}";
EOF