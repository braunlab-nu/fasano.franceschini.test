# fasano.franceschini.test 2.2.0
Submission of new version to CRAN.

# Test environments
* macOS 12.6.5 (64-bit) R 4.2.2 (local)
* Windows, R-oldrelease (win-builder)
* Windows, R-release (win-builder)
* Windows, R-devel (win-builder)
* Debian Linux, R-devel, GCC ASAN/UBSAN (R-hub)
* Fedora Linux, R-devel, clang, gfortran (R-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (R-hub)

# R CMD Check results
These are the various notes outputted during checks.

* checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.

**GNU make is required by the RcppParallel package.**

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found

**This seems to be an issue with R-hub as opposed to our package.**

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
  
**There is no folder or file in the check directory named 'NULL'.**

* checking for detritus in the temp directory ... NOTE
  'lastMiKTeXException'
  
**This seems to be an issue with MiKTeX as opposed to our package.**
