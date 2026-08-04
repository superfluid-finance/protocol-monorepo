# Yarn

This project uses Yarn Berry, vendored under `.yarn/releases/`.
`.yarn/releases/yarn.cjs` is a stable symlink to the pinned release; `.yarnrc.yml`
`yarnPath`, `flake.nix`, and nested projects all reference that name.
`packageManager` in the root `package.json` records the same version.

To upgrade:

```bash
yarn set version stable
# yarn set version rewrites yarnPath to the versioned file; retarget the symlink
# and restore the stable yarnPath:
ln -sfn yarn-X.Y.Z.cjs .yarn/releases/yarn.cjs
# set yarnPath back to .yarn/releases/yarn.cjs in .yarnrc.yml if needed
```
