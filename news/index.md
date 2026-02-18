# Changelog

## junco 0.1.4

CRAN release: 2026-02-11

#### Fixed

- “caption” paragraph style in the docx exporter is now handled by
  flextable
  ([\#182](https://github.com/johnsonandjohnson/junco/issues/182))
- Fixed
  [`tt_to_tlgrtf()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md),
  when exporting an empty listing do not lose Title and Footers
- Fixed
  [`tt_to_tlgrtf()`](https://johnsonandjohnson.github.io/junco/reference/tt_to_tlgrtf.md)
  argument `label_width_ins` which was not applying the change in the
  row label column width
  ([\#166](https://github.com/johnsonandjohnson/junco/issues/166)).

#### Changed

- Reinstate rbmi as dependency
