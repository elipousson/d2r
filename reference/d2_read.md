# Read, write, and format D2 files

Read, write, and format D2 files

## Usage

``` r
d2_read(file)

d2_write(data, file = NULL, fileext = "d2", tidy = getOption("d2r.tidy", TRUE))

d2_fmt(file)
```

## Arguments

- file:

  File name to write to disk. If `NULL`, set to temporary file name
  using supplied file extension.

- data:

  A set of lines or a file path to a d2 file.

- fileext:

  File extension to use for writing data to disk.

- tidy:

  If `TRUE`, pass the tidy flag to the command line execution. Defaults
  to `getOption("d2r.tidy", TRUE)`.
