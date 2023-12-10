# Documentation Deployment
The documentation for each version is stored in its on branch - such as `1.0`, `2.0` etc. The current development branch
is stored in the `main` branch.

To deploy the development version:

```shell
nix develop
git checkout main
mike deploy --update-aliases <version> development
git push origin gh-pages:gh-pages
```

To deploy the current stable version:

```shell
nix develop
git checkout <version>
mike deploy --update-aliases <version> stable
git push origin gh-pages:gh-pages
```

To deploy an old version:

```shell
nix develop
git checkout <version>
mike deploy <version>
git push origin gh-pages:gh-pages
```
