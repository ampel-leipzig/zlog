# zlog 1.0

## Changes 1.0.2 (2022-12-28)

- Ignore cases for the "age" and "sex" column names in
  `lookup_limits` and `.lookup_limits_df` (now case insensitive).
  Affected user visible functions: `zlog_df`, `impute_df`.

## Changes 1.0.1 (2021-05-01)

- Add URL and BugReport URL to DESCRIPTION file.

## Changes in 1.0.0 (2021-04-29)

- First CRAN release.

# zlog 0.0

## Changes in 0.0.16

- Add `impute_df` to impute missing values with there correct reference values.

## Changes in 0.0.15

- No user-visible changes.
- Move reference lookup code from `z_df`/`zlog_df` to the internal
  `.lookup_limits_df` function.

## Changes in 0.0.14

- Fix for `lookup_limits` that reordered its results depending on the "age"
  column in the "table" argument.
- Fix for `z_df` and `z_logdf` now correctly handle the ordering of the
  `matrix` returned by `lookup_limits`.

## Changes in 0.0.13

- Add `set_missing_limits`.
- Fix first author name of reference in manual pages for `zcol`, `zlog` and
  `zlog_df`.

## Changes in 0.0.12

- `z_df` and `zlog_df` now accept a reference table `limits` with more
  parameters (`params`) than present in the `x` `data.frame`.

## Changes in 0.0.11

- Add `z_df` and `zlog_df` as convenient wrapper around `z`/`zlog` and
  `lookup_limits` for larger `data.frame`s and reference tables.
- Fix ordering in `lookup_limits` if a `"param"` column is given.

## Changes in 0.0.10

- `z` gains a `log` argument (default: `FALSE`) and `zlog` is now an alias for
  `z(..., log = TRUE)`.

## Changes in 0.0.9

- Add support for multiple reference parameters in `lookup_limits`.

## Changes in 0.0.8

- Add *z* and *z(log)* equation to vignette.

## Changes in 0.0.7

- Extend `pbc` vignette example with a sex-specific reference table and use
  `lookup_limits`.

## Changes in 0.0.6

- `zcol`: slightly increase color contrast around 1 and 2
  (move darker colours one unit into the middle).

## Changes in 0.0.5

- Add `lookup_limits` to find reference limits in reference tables.

## Changes in 0.0.4

- Fix `zcol` for `NA`.
- Add `surival::pbc` example to the vignette.

## Changes in 0.0.3

- Add `iz`/`izlog` as inverse function to `z`/`zlog`;
  closes [#2](https://github.com/ampel-leipzig/zlog/issues/2).

## Changes in 0.0.2

- Vectorise `limits`, `probs` argument of `z` and `zlog`;
  closes [#1](https://github.com/ampel-leipzig/zlog/issues/1).

## Changes in 0.0.1

- Initial version.
