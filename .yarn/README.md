# Yarn

This project uses Yarn Berry, vendored under `.yarn/releases/` and selected via
`yarnPath` in `.yarnrc.yml` (also reflected in the root `packageManager` field).

To upgrade:

```bash
yarn set version stable
```

Then update the `yarn-repo` path in `flake.nix` to the new release file.
