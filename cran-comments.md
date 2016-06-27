# CRAN submission future.BatchJobs 0.12.1
on 2015-06-26

This fixes the following two CRAN issues of 0.12.0 submitted earlier today:

* BUG FIX: Multicore BatchJobs gave an error on Solaris.

* TESTS: Disabling all multicore BatchJobs tests, because there is a risk for long processing / sleep times on systems running many other processes at the same time, e.g. CRAN servers.

Thanks


## Notes not sent to CRAN
The package has been verified using `R CMD check --as-cran` on:

* Platform x86_64-apple-darwin13.4.0 (64-bit) [Travis CI]:
  - R 3.2.4 Revised (2016-03-16)
  - R version 3.3.1 (2016-06-21)
  
* Platform x86_64-unknown-linux-gnu (64-bit) [Travis CI]:
  - R version 3.2.5 (2016-04-14)
  - R version 3.3.0 (2016-05-03)
  - R Under development (unstable) (2016-06-25 r70832)

* Platform x86_64-pc-linux-gnu (64-bit):
  - R version 3.3.1 (2016-06-21)

* Platform i686-pc-linux-gnu (32-bit):
  - R version 3.3.1 (2016-06-21)
 
* Platform i386-w64-mingw32 (32-bit) [Appveyor CI]:
  - R Under development (unstable) (2016-06-25 r70833)

* Platform x86_64-w64-mingw32/x64 (64-bit) [Appveyor CI]:
  - R Under development (unstable) (2016-06-25 r70833)

* Platform x86_64-w64-mingw32/x64 (64-bit) [win-builder]:
  - R version 3.3.1 (2016-06-21)
  - R Under development (unstable) (2016-06-25 r70833)
