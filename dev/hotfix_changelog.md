# Junco Hotfix Changelog

This log tracks surgical hotfixes applied to legacy `junco` versions (e.g., v0.1.1). Because legacy containers cannot be rebuilt, these fixes are injected globally.


---

**DO NOT REMOVE**

 [Template for Hotfix]
 
## [Hotfix Title or Ticket #] - YYYY-MM-DD
**Author:** [Your Name/Handle]  
**Target Legacy Version(s):** [e.g., v0.1.1, v0.1.2]  
**Mainline PR:** [Link to the original fix merged into `dev`]  

### The Bug
* **Impact:** [Briefly describe what broke in production and why PM approved the hotfix]
* **Root Cause:** [1-2 sentences on the technical reason it failed]

### Surgical Changes
* **Primary Function(s) Fixed:**
    * `[e.g., a_freq_j()]`

### Testing & CI Notes
* **Snapshot Changes:** [None / Expected due to math correction / Link to legacy PR updating expected snapshots]
* **Runner Quirks:** [None / e.g., "Skipped `test-rbmi.R` due to GitHub Actions"]

**INSERT BELOW**

---

## Hotfixes #165, #178, and #221 - 2026-04-20
**Author:** Technology Solutions  
**Target Legacy Version(s):** v0.1.3  

### The Bug
* **Impact:** Users were unable to export optional CSVs alongside RTFs, empty listings were losing their titles during export, nested row splits were failing when using `countsource = altdf`, and label widths were not being respected across paginated tables. 

### Surgical Changes
* **Primary Function(s) Fixed:**
    * `tt_to_tlgrtf()`: 
        * Added `export_csv` and `output_csv_directory` arguments.
        * Added internal helper `get_output_csv_filename()`.
        * Added `label_width_ins = label_width_ins` to all recursive pagination calls so formatting isn't lost on multi-page outputs.
        * Fixed logic to retain `main_title` and `main_footer` when exporting an empty listing.
    * `a_freq_j()`, `s_freq_j()`, `h_a_freq_dataprep()`: 
        * Plumbed the `countsource` argument through the call stack to prevent failures when `countsource = "altdf"` in nested splits.

### Testing & CI Notes
* **Snapshot Changes:** Expected test modifications required to pass CI with the new patches.
    * `test-tt_to_tlgrtf.R`: Removed the test asserting that the CSV exists (line 202) to accommodate the new optional CSV logic.
    * `test-docx_exporter_functions.R`: Removed lines 386-387 (`string_to_look_for` and `add_title_style_caption(doc, string_to_look_for)`). **Reason:** Caption styling is handled natively by `flextable` as of v0.9.11, making explicit manual style insertion obsolete and error-prone.
