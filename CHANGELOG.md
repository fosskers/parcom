# Parcom Changelog

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
