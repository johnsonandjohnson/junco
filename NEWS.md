# junco 0.1.4


### Fixed
- "caption" paragraph style in the docx exporter is now handled by flextable (#182)
- Fixed `tt_to_tlgrtf()`, when exporting an empty listing do not lose Title and Footers
- Fixed `tt_to_tlgrtf()` argument `label_width_ins` which was not applying the change in the row label column width (#166).

### Changed
- Reinstate rbmi as dependency


