# fasano.franceschini.test 2.0.0
Submission of new version to CRAN.

There are a few notes that come up in the various checks. These are explained below.
* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Connor Puritz <connorpuritz2025@u.northwestern.edu>’
  
  New maintainer:
    Connor Puritz <connorpuritz2025@u.northwestern.edu>
  Old maintainer(s):
    Elan Ness-Cohn <elanness-cohn2017@u.northwestern.edu>

**The original maintainer has emailed CRAN to notify of the change in ownership.**

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

# Test environments
* macOS 11.6.5 (64-bit) R 4.2.0 (local)
* Windows Server 2008 (64-bit) R 4.0.5 (win-builder, r-oldrelease)
* Windows Server 2008 (64-bit) R 4.1.2 (win-builder, r-release)
* Windows Server 2022 (64-bit) R 4.2.0 (win-builder, r-devel)
* Debian Linux, R-devel, GCC ASAN/UBSAN (R-hub)
* Windows Server 2022, R-devel, 64 bit (R-hub)
* Fedora Linux, R-devel, clang, gfortran (R-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (R-hub)

# R CMD Check results
## macOS 11.6.5 R 4.2.0 (local)
0 errors | 0 warnings | 1 note
* checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.

## win-builder
### R-oldrelease
0 errors | 0 warnings | 1 note
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Connor Puritz <connorpuritz2025@u.northwestern.edu>'

New maintainer:
  Connor Puritz <connorpuritz2025@u.northwestern.edu>
Old maintainer(s):
  Elan Ness-Cohn <elanness-cohn2017@u.northwestern.edu>

GNU make is a SystemRequirements.

### R-release
0 errors | 0 warnings | 1 note
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Connor Puritz <connorpuritz2025@u.northwestern.edu>'

New maintainer:
  Connor Puritz <connorpuritz2025@u.northwestern.edu>
Old maintainer(s):
  Elan Ness-Cohn <elanness-cohn2017@u.northwestern.edu>

GNU make is a SystemRequirements.

### R-devel
0 errors | 0 warnings | 1 note
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Connor Puritz <connorpuritz2025@u.northwestern.edu>'

New maintainer:
  Connor Puritz <connorpuritz2025@u.northwestern.edu>
Old maintainer(s):
  Elan Ness-Cohn <elanness-cohn2017@u.northwestern.edu>

GNU make is a SystemRequirements.

## R-hub
### Debian Linux, R-devel, GCC ASAN/UBSAN
0 errors | 0 warnings | 0 notes

### Windows Server 2022, R-devel, 64 bit
0 errors | 0 warnings | 3 notes
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Connor Puritz <connorpuritz2025@u.northwestern.edu>'

New maintainer:
  Connor Puritz <connorpuritz2025@u.northwestern.edu>
Old maintainer(s):
  Elan Ness-Cohn <elanness-cohn2017@u.northwestern.edu>

Found the following (possibly) invalid DOIs:
  DOI: 10.1093/mnras/225.1.155
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

* checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

### Fedora Linux, R-devel, clang, gfortran
0 errors | 0 warnings | 2 notes
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Connor Puritz <connorpuritz2025@u.northwestern.edu>’

New maintainer:
  Connor Puritz <connorpuritz2025@u.northwestern.edu>
Old maintainer(s):
  Elan Ness-Cohn <elanness-cohn2017@u.northwestern.edu>

Found the following (possibly) invalid DOIs:
  DOI: 10.1093/mnras/225.1.155
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

* checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.

### Ubuntu Linux 20.04.1 LTS, R-release, GCC
0 errors | 0 warnings | 2 notes
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Connor Puritz <connorpuritz2025@u.northwestern.edu>’

New maintainer:
  Connor Puritz <connorpuritz2025@u.northwestern.edu>
Old maintainer(s):
  Elan Ness-Cohn <elanness-cohn2017@u.northwestern.edu>

Found the following (possibly) invalid DOIs:
  DOI: 10.1093/mnras/225.1.155
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

* checking for GNU extensions in Makefiles ... NOTE
GNU make is a SystemRequirements.
