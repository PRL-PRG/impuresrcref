# srcrefimpute

`srcrefimpute` imputes transparent `srcref` metadata for injected brace calls (`{`) in R function ASTs.

## What it does

Given code like:

```r
if (x && y) f() else g()
```

`impute_srcrefs()` rewrites control-flow positions to:

```r
if ({x} && {y}) {f()} else {g()}
```

and assigns srcrefs to injected `{` calls using parse-data-derived source spans.

## Exported API

- `impute_srcrefs(fn)`
  - Impute missing control-flow braces and transparent srcrefs for one function.
- `source_impute_srcrefs(file, envir = parent.frame(), ...)`
  - Source an R file and patch all changed/new functions in the target environment.
- `check_package_parse_data(package, include_internal = TRUE)`
  - Check whether installed package functions carry parse data.
- `impute_package_srcrefs(package, include_internal = TRUE, ...)`
  - Patch package functions if parse data is available

## Important: package installs and parse data

For installed packages, parse data is often missing unless the package was installed from source with package source options enabled.

Recommended installation pattern:

```r
local({
  options(keep.source.pkgs = TRUE, keep.parse.data.pkgs = TRUE)
  install.packages("MASS", type = "source")
})
```

## Usage

### Patch functions

```r
options(keep.source = TRUE)
f <- eval(parse(text = "function(x, y) if (x && y) f() else g()", keep.source = TRUE)[[1]])
g <- impute_srcrefs(f)
g
```

### Source a file and patch all loaded functions

```r
env <- new.env(parent = baseenv())
res <- source_impute_srcrefs("path/to/file.R", envir = env)
res$functions
```

### Check and patch an installed package

```r
check_package_parse_data("MASS")
res <- impute_package_srcrefs("MASS")
res$patched_count
```

If parse data is missing, `impute_package_srcrefs()` prints an install command and returns without patching.

### Functions without srcref metadata

`impute_srcrefs()` requires function srcref metadata by default.
For functions created without srcrefs, opt into deparse-based fallback explicitly:

```r
options(srcrefimpute.allow_deparse_fallback = TRUE)
```

## Tests

Snapshot tests compare generated output against committed `.out` golden files.
Run tests in compare mode:

```r
Rscript tests/test-srcref-imputation.R
```

Refresh snapshots:

```r
UPDATE_SNAPSHOTS=1 Rscript tests/test-srcref-imputation.R
```

By default, mismatches fail. With `UPDATE_SNAPSHOTS=1`, the snapshot file is rewritten.
Legacy `--update` is still accepted for compatibility.
