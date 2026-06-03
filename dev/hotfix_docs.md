# Junco Hotfix Standard Operating Procedure (SOP)

This document outlines the protocol for developing, testing, and deploying hotfixes for legacy versions of the `junco` package (e.g., `v0.1.1`, `v0.1.2`). 

Because we cannot rebuild and redeploy new package versions onto old containes, we use a "surgical injection" method to override specific functions in the user's environment.

## Phase 1: The Mainline Fix
**1. PM Approval:** A critical bug is identified in production. The Project Manager (PM) formally decides that a hotfix is necessary.

**2. Mainline PR & Regression Testing:** The developer fixes the bug in the current `dev` branch via a standard Pull Request. 
> **What is a Regression Test?** > When you fix a bug, you must add a specific unit test that proves the bug is fixed.
> This is called a regression test because it ensures the codebase never "regresses" back to having this specific bug in the future.

## Phase 2: The Namespace Trap & Dependency Mapping
**3. Percolate the Hotfix:** Once merged into the current working branch, the fix must be ported backwards to older legacy versions/containers.

**4. Isolate Surgical Changes & Find Child Dependencies:** To hotfix an R package, we source a `.R` file into the Global Environment. However, R uses a **locked package namespace**. 

If we fix function `a()`, and the package has an internal function `b()` that calls `a()`, the internal `b()` will ignore our hotfix and continue using the broken `a()` trapped inside the locked package. 
Therefore, **we must include both the fixed function AND any child functions that depend on it in our hotfix file.**

To identify these dependencies, use `pkgnet` or the manual environment search script below 
> Make sure you have the relevant package version installed (i.e. v0.1.1, v0.1.2, etc.)

```r
# Visual Report
library(pkgnet)
CreatePackageReport(pkg_name = "junco", report_path = "report.html")

# Programmatic Search
pkg <- "junco"
ns <- asNamespace(pkg)
funs <- ls(ns)

# Put the name of the function you are fixing here:
target <- "tt_to_tlgrtf" 

# Search the package memory for functions calling your target
deps <- sapply(funs, function(fn) {
  f <- get(fn, envir = ns)
  if (is.function(f)) {
    code <- deparse(body(f))
    any(grepl(paste0("\\b", target, "\\b"), code))
  } else {
    FALSE
  }
})

# Get names of all dependent functions that MUST be included in your hotfix file
names(deps[deps])
```

## Phase 3: Branching & Injection
**5. Create the Branch & File:** In the `junco` GitHub repository, create a new branch strictly following this naming convention:
`feature/hotfix-<your-hotfix-name>`

Save your hotfix files in the `dev/` folder. **File names must exactly match the target legacy version:**
* `dev/junco_hotfix_v0-1-1.R`
* `dev/junco_hotfix_v0-1-2.R`

## Phase 4: CI Validation
**6. Trigger the Pipeline:** Push your `feature/hotfix-*` branch to GitHub. This automatically triggers the Hotfix CI Pipeline, which will virtually inject your code into the legacy package and run the old test suite.

**7. Handle Snapshot Failures:** If the pipeline fails, check the logs. It is usually caused by snapshot mismatches in the unit tests (since your hotfix changed the math, formatting, or output). 
* Evaluate: Are these snapshot changes expected due to your fix? 
* Discuss with the team. 
* If expected, open a PR against the legacy branch (e.g., `v0.1.1`) strictly to accept and update the expected test snapshots.

## Phase 5: Documentation
**8. Update the Changelog:** Document the bug, the affected versions, and the functions modified inside `dev/hotfix_changelog.md`. 

## Phase 6: Update
**9. Update the Template:** Go to the Bitbucket `jjcs_templates` repository, do a PR on the `main` branch, and paste in your new hotfix under the header fo the old version and uppate the date i.e. :


```
###############################################################################
## Original Reporting Effort: Standards
## Program Name:              junco_hotfix.r
## R version:                 4.5.2
## Short Description:         junco package function hotfix changes
## Author:                    Technology Solutions
## Date:                      TODAY'S DATE XXXXXXXXX
###############################################################################



rver <- getRversion()

if(rver != "4.5.2"){
  stop("This hotfix should only be run on the 2026q2_r452_1_0_0 container !")
}

library(junco)
#HERE YOUR COPY PASTE#
```




---
