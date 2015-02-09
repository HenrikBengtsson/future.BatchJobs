# R package async - Asynchroneous evaluation

Copyright Henrik Bengtsson, 2015

## Asynchroneous evaluation
_Asynchroneous evaluation_ is a method for evaluting multiple R
expressions in a parallel or distributed fashion such that the
"observed" total time for computing all values is less that if the
expressions would be evaluated synchroneously (sequentially).

For instance, the following evaluation, which is synchroneous, takes approximately 10 seconds to complete:

```r
> x <- { Sys.sleep(5); 3.14 }
> y <- { Sys.sleep(5); 2.71 }
> z <- x + y
[1] 5.85
```

whereas the follwing _asynchroneous_ evaluation only takes
approximately 5 seconds to complete:

```r
> library(async)
> x %<=% { Sys.sleep(5); 3.14 }
> y %<=% { Sys.sleep(5); 2.71 }
> z <- x + y
[1] 5.85
```


## Evaluation is done in "local" environment
Note that each asynchroneous expression is evaluated in its own unique _asynchroneous environment_, which is different from the calling environment.  The only way to transfer information from the "asynchroneous environment" to the calling environment, is via the (return) value, just as when functions are called.   In other words,

```r
v1 %<=% { x <- 3.14 }
```

is effectively equivalent to

```r
v1 %<=% local({ x <- 3.14 })
```

Both calls will assign variable 'x' value `3.14` in the asynchroneous environment.  Another way to say this is that both calls will "assign _asynchroneous variable_ 'x' value `3.14`".  Since this is the last value in the expression, it is also the value of the asynchroneous expression.  This is the value that will be assigned to variable `v1` in the calling environment.

As a matter of fact, it is _not_ possible to for an asynchroneous expression to assign variable in the calling environment, i.e. assignments such as `<-`, `<<-` and `assign()` only affects the asynchrenous environment.


### Global variables

Although the following expression is evaluated in an asynchroneous environment, which is separate from the calling one, the asynchroneous environment "inherits"(*) all "global" variables in the calling environment and its parents.  For example,
```r
a <- 2
v2 %<=% { x <- a*3.14; x }
```
will result in `v2` being assigned `6.28`.

(*) Details: Since the asynchroneous environment may be in a separate R session on a physically different machine, the "inheritance" of "global" variables is achieved by identify which variables in the asynchroneous expression are global and copy them from the calling environment to the asynchroneous environment (using serialization).  The global environments are identified using code inspection, cf. the 'codetools' package.


## Availability
This package is only available via GitHub.  Install in R as:

```s
source('http://callr.org/install#HenrikBengtsson/async')
```

## Appendix

### Configuration of backend for parallel / distributed processing
The asynchroneous evaluation done by the 'async' package uses the
'BatchJob' package as a backend for distributing the computations
either in parallel to multiple cores or distribute them to multiple
machines or on to a cluster.


### Software quality

* R CMD check status: <a
  href="https://travis-ci.org/HenrikBengtsson/async"><img
  src="https://travis-ci.org/HenrikBengtsson/async.svg?branch=master"
  alt="Build status"></a>
* Test coverage status: <a
  href='https://coveralls.io/r/HenrikBengtsson/async?branch=develop'><img
  src='https://coveralls.io/repos/HenrikBengtsson/async/badge.png?branch=develop'
  alt='Coverage Status' /></a>

