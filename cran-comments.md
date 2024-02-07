# fasano.franceschini.test 2.2.2
Submission of new version to CRAN.

# Test environments
* macOS 14.1, R 4.3.2 (local)
* macOS 13.1.1, R 4.3.0 (mac-builder)
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

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''

* checking for detritus in the temp directory ... NOTE
  'lastMiKTeXException'
  
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found

* checking for non-standard things in the check directory ... NOTE

 **These all seem to be an issues with R-hub as opposed to our package.**
