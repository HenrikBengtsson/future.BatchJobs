include .make/Makefile

future.tests/%:
	$(R_SCRIPT) -e "future.tests::check" --args --test-plan=$*

future.tests: future.tests/future.BatchJobs\:\:batchjobs_local

spelling:
	$(R_SCRIPT) -e "spelling::spell_check_package()"
	$(R_SCRIPT) -e "spelling::spell_check_files(c('NEWS', dir('vignettes', pattern='[.](md|rsp)$$', full.names=TRUE)), ignore=readLines('inst/WORDLIST', warn=FALSE))"
