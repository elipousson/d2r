# Install D2 using the sh script

`d2_install()` downloads and executes a `sh` script provided for
installing D2 in [the Getting Started
documentation](https://d2lang.com/tour/install). This is not the most
secure way to install D2. You should consider using Homebrew, a
pre-built Windows installer, or another installation option listed in
[the detailed installation
instructions](https://github.com/terrastruct/d2/blob/master/docs/INSTALL.md).

## Usage

``` r
d2_install(dry_run = TRUE)
```

## Arguments

- dry_run:

  If `TRUE` (default), append `"--dry-run"` to the arguments passed to
  the `sh` command.
