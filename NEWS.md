# junco 0.1.2

## New features

* None

## Bug fixes

* None

## Other changes

* Moved 'rbmi' package from 'Imports' to 'Suggests' to reduce dependencies
* Added conditional checks in all functions and tests that use 'rbmi'
* Package can now be installed and used without 'rbmi' being available
* Functions that depend on 'rbmi' will provide informative error messages if 'rbmi' is not installed
