You're completely right — the SOP needs to show the exact template the end-user will execute so developers know what they are building towards.

I will update Phase 5 of the SOP to explicitly show the new `jjcs_templates` script structure using `hotpatchR::apply_hotfix_file()`. This makes it crystal clear how the deployment actually happens.

***

# Junco Hotfix Standard Operating Procedure (SOP)

This document outlines the protocol for developing, testing, and deploying hotfixes for legacy versions of the `junco` package (e.g., `v0.1.1`, `v0.1.2`). 

Because we cannot rebuild and redeploy new package versions onto old containers, we use **native memory injection** via the `hotpatchR` package to patch the locked namespace directly.

## Phase 1: The Mainline Fix
**1. PM Approval:** A critical bug is identified in production. The Project Manager (PM) formally decides that a hotfix is necessary.

**2. Mainline PR & Regression Testing:** The developer fixes the bug in the current `dev` branch via a standard Pull Request. 
> **What is a Regression Test?** When you fix a bug, you must add a specific unit test that proves the bug is fixed. This ensures the codebase never "regresses" back to having this specific bug in the future.

---

## Phase 2: The `hotpatchR` Injection File
**3. Isolate Surgical Changes:** We no longer need to map or copy child dependencies. Because `hotpatchR` rewrites the package memory directly, **you only need to write the exact function that is broken.** Internal dependencies will automatically inherit the fix.

**4. Create the Hotfix File:** In the `junco` GitHub repository, create a new from `dev` branch strictly following this naming convention: `feature/hotfix-<your-hotfix-name>`

Save your hotfix file in the `dev/` folder. **File names must exactly match the target legacy version** (e.g., `dev/junco_hotfix_v0-1-1.R`).

Structure the file exactly like this:

```r
# dev/junco_hotfix_v0-1-1.R

# target package
pkg <- "junco"

# 2. Write ONLY the surgical fix
fixed_target_function <- function(...) {
  # ... fixed code ...
}

# 3. Export to the patch_list
patch_list <- list(
  target_function = fixed_target_function
)
```

---

## Phase 3: CI Validation
**5. Trigger the Pipeline:** Push your `feature/hotfix-*` branch to GitHub. This automatically triggers the Hotfix CI Pipeline, which will use `hotpatchR` to virtually inject your code into the legacy package and run the old test suite.

**6. Handle Snapshot Failures:** If the pipeline fails, it is usually caused by snapshot mismatches in the unit tests (since your hotfix changed the math, formatting, or output). 
* Evaluate: Are these snapshot changes expected due to your fix? 
* Discuss with the team. 
* If expected, open a PR against the legacy branch (e.g., `v0.1.1`) strictly to accept and update the expected test snapshots.

---

## Phase 4: Documentation
**7. Update the Changelog:** Document the bug, the affected versions, and the exact functions modified inside `dev/hotfix_changelog.md`.

---

## Phase 5: Deploy the End-User Template
**8. Update Bitbucket:** Go to the Bitbucket `jjcs_templates` repository, open a PR on the `main` branch, and update the template for the target version. 

Users will execute this exact script in their legacy containers. It downloads the file you created in Phase 2 and applies it instantly.

```r
###############################################################################
## Original Reporting Effort: Standards
## Program Name:              junco_hotfix.r
## R version:                 4.5.2
## Short Description:         junco package function hotfix changes
## Author:                    Technology Solutions
## Date:                      [TODAY'S DATE]
###############################################################################

rver <- getRversion()

if(rver != "4.5.2"){
  stop("This hotfix should only be run on the 2026q2_r452_1_0_0 container!")
}

# Inject the patch into the container's memory
apply_hotfix_file(read_path(cl, 'junco_hotfix_v0-1-3.r'))

message("✅ Junco hotfix v0-1-3 applied successfully via hotpatchR.")
```
