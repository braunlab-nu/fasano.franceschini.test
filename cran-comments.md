# fasano.franceschini.test 2.0.1
Submission of new version to CRAN.

There are no errors or warnings.
There are a few notes that come up in the various checks. These are explained below.
* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

**GNU make is required by the RcppParallel package.**

* Found the following (possibly) invalid DOIs:
  DOI: 10.1093/mnras/225.1.155
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

**I have checked this DOI and it is correct.**

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

**This seems to be an issue with MiKTeX, and can be ignored according to https://github.com/r-hub/rhub/issues/503.**

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found

**This appears to be an issue with the testing environment and not the package.**

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Connor Puritz <connorpuritz2025@u.northwestern.edu>'

Possibly mis-spelled words in DESCRIPTION:
  Fasano (3:8, 17:42)
  Franceschini (3:15, 17:53)
  
**These words are not misspelled.**

# Test environments
* macOS 11.6.8 (64-bit) R 4.2.1 (local)
* Windows, R-oldrelease (win-builder)
* Windows, R-release (win-builder)
* Windows, R-devel (win-builder)
* Debian Linux, R-devel, GCC ASAN/UBSAN (R-hub)
* Fedora Linux, R-devel, clang, gfortran (R-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (R-hub)

# R CMD Check results
## macOS 11.6.8 R 4.2.1 (local)
0 errors | 0 warnings | 1 note
* checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.

## win-builder
### R-oldrelease
0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Connor Puritz <connorpuritz2025@u.northwestern.edu>'

Possibly mis-spelled words in DESCRIPTION:
  Fasano (3:8, 17:42)
  Franceschini (3:15, 17:53)

### R-release
0 errors | 0 warnings | 0 notes

### R-devel
0 errors | 0 warnings | 0 notes

## R-hub
### Debian Linux, R-devel, GCC ASAN/UBSAN
0 errors | 0 warnings | 0 notes

### Fedora Linux, R-devel, clang, gfortran
0 errors | 0 warnings | 2 notes

* checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found

### Ubuntu Linux 20.04.1 LTS, R-release, GCC
0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Connor Puritz <connorpuritz2025@u.northwestern.edu>’

Found the following (possibly) invalid DOIs:
  DOI: 10.1093/mnras/225.1.155
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

* checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.
