# Parcom Changelog

### 1.5.1 (2025-09-30)

#### Fixed

- Depending on the parser they were given, `many` and `many1` were consuming too
  much offset. I only discovered this now as I mostly avoid `many` in favour of
  `take-while` or `sep`, but in some recent code I needed `many` for its
  original purpose (collecting a list), and the bug surfaced.

### 1.5.0 (2025-09-14)

#### Added

- Combinator: `not` for succeeding when a parser fails. Useful when trying to
  test a complicated parse boundary, say in combination with `sep-end`.
- Combinator: `consume-sep` and `consume-sep1` for advancing the parser like
  `sep` but without allocating.
- datetime: The `simple-local-time` parser for lenient parsing of local times.
- email: The new `parcom/email` system.

#### Changed

- A number of parsers/combinators are now more memory efficient by avoiding
  internal lambda allocations.

### 1.4.0 (2025-08-17)

#### Added

- Combinator: `maybe` for conditionally applying a function to the result of a parser.

#### Changed

- toml: `parse` checks if you've reached the end of the file.
- xml: `parse` checks if you've reached the end of the file.
- json: Numbers now parse as `fixnum` if they can be.
- json: Number parsing is now 2x as fast and uses half as much memory.

#### Fixed

- xml: Allow a `<!DOCTYPE ...>` tag near the top of the file.

### 1.3.0 (2025-05-31)

#### Added

- The `parcom/xml` system.

#### Changed

- Improved performance of `parcom/json`.
- Improved contextual output in parse failure condition.
- `consume`: Yields the offset as a value instead of `t`.
- A few more functions have been given lambda caches.

### 1.2.0 (2025-05-17)

#### Added

- The `parcom/toml` system.
- Parser: `pure` for injecting values as parsers.
- Utility: `pmap` for transforming parser successes more directly than `fmap`.

### 1.1.0 (2025-05-09)

#### Added

- The `parcom/datetime` system.
- Parser: `any-if`.
- Parser: `sneak`.
- Combinator: `take-until`.

#### Changed

- Vastly improved performance. Runtime is ~3x faster and uses 25x less memory.
- The signalling of parsing success and failure is done through multiple return
  values, not structs. The fundamental signature of parser functions is thus now
  `offset -> (value, offset)`.
- Contextual information when parsing fails is more accurate.
- `anybut` -> `any-but`

#### Removed

- The explicit `parser`, `failure`, and `input` types. This is now managed by
  multiple return values, as described above.

### 1.0.0 (2025-04-23)

Initial release, including `parcom` and `parcom/json`.
