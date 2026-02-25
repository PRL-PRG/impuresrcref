# imputesrcref

`imputesrcref` imputes transparent `srcref` metadata for injected brace calls (`{`) in R function ASTs.

## srcref primer

In R, a [`srcref`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/srcfile.html) is source-location metadata (line/column ranges) attached to
parsed objects when source retention is enabled (for example with
`options(keep.source = TRUE)`).
This package solves a finer-grained mapping problem for control-flow
expressions written without braces (for example `if (x) f() else g()`) and
for unbraced function-call arguments. It adds transparent wrappers and imputes
srcrefs from parse data so individual components can be mapped to source
precisely.
It uses the parser token tables [`getParseData`](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/getParseData.html) for it.

## What it does

Given code like:

```r
if (x && y) f() else g()
```

`impute_srcrefs()` rewrites target positions to:

```r
if ({x} && {y}) {f()} else {g()}
```

and:

```r
g({x+1}, {f({y+1})})
```

and assigns srcrefs to injected `{` calls using parse-data-derived source spans.

## Exported API

- `impute_srcrefs(fn)`
  - Impute missing control-flow/function-call braces and transparent srcrefs for one function.
- `source_impute_srcrefs(file, envir = parent.frame(), ...)`
  - Source an R file and patch all changed/new functions in the target environment.
- `impute_package_srcrefs(package, include_internal = TRUE, ...)`
  - Patch package namespace functions if parse data is available

## Important: package installs and parse data

For installed packages, parse data is often missing unless the package was
installed from source with source/parse retention enabled.

Recommended installation pattern:

```r
    install.packages("<package>", INSTALL_opts=c("--with-keep.source", "--with-keep.parse.data"))
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

### Patch an installed package

```r
res <- impute_package_srcrefs("stringr", include_internal = TRUE)
res$patched_count
head(res$failed[!is.na(res$failed)])
```

Returned fields are:

- `package`
- `fn_names`
- `failed` (`NA` means successfully patched)
- `patched_count`
- `install_command` (reinstall hint when nothing could be patched)

### Functions without srcref metadata

`impute_srcrefs()` requires function srcref metadata by default.
For functions created without srcrefs, opt into deparse-based fallback explicitly:

```r
options(imputesrcref.allow_deparse_fallback = TRUE)
```

## Tests

Snapshot tests compare generated output against committed `.out` golden files.
They include regression coverage for:

- package-style `srcref` line mappings (`sr[7:8]` vs `sr[1:3]`)
- zero-formals functions (`function()`)
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

## Acknowledgements

This package was inspired in part by [covr](https://covr.r-lib.org/)'s parse-data handling [approach](https://github.com/r-lib/covr/blob/f1866d296c00884d1f085ff245669de01bc864c4/R/parse_data.R).
