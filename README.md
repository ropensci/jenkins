# jenkins

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/ropensci/jenkins.svg?branch=master)](https://travis-ci.org/ropensci/jenkins)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ropensci/jenkins?branch=master&svg=true)](https://ci.appveyor.com/project/jeroen/jenkins)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/jenkins)](http://cran.r-project.org/package=jenkins)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/jenkins)](http://cran.r-project.org/web/packages/jenkins/index.html)

> Simple Jenkins Client for R

Managing jobs and builds on your jenkins server. Create or update projects, control builds and queues, inspect logs, and much more.

## How to use

Generate create a new PAT in your Jenkins server:

![screenshot](https://user-images.githubusercontent.com/216319/58768185-60142680-8597-11e9-9e5d-1c05798f59ec.png)

Add this in your `~/.Renviron` file like this:

```
JENKINS_PAT=3858f62230ac3c915f300c664312c63f
```

Now you connect to your Jenkins server and do stuff:

```r
# Make a connection
jk <- jenkins(server = 'http://jenkins.ropensci.org', username = 'jeroen')

# Do stuff
jk$info()
jk$build_start('magick')
jk$build_info('magick')
```

