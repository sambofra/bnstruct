bnstruct v1.0.11 (Release date: 2021-05-19 )
================
* dag.to.cpdag() now takes into account layer.struct (thanks to Erica Tavazzi)
* class(x)==y ported to inherits(x,y)
* bugfix in BP in case of disconnected single nodes

bnstruct v1.0.10 (Release date: 2021-05-06 )
================
* Removed direct dependency from Matrix package

bnstruct v1.0.9 (Release date: 2021-02- )
================

* Bugfix in BP (found by Fritz Bayer)
* Bugfix in EM, continuous case
* Speedup EM by computing log-likelihood only for observations that change

bnstruct v1.0.8 (Release date: 2020-05-22)
===============

* Bugfix in EM

bnstruct v1.0.7 (Release date: 2020-05-04)
===============

* Improved robustness in reading data and header files
* Bugfix in EM

bnstruct v1.0.6 (Release date: 2019-07-09)
===============

* Bugfix in SM

bnstruct v1.0.5 (Release date: 2019-07-02)
===============

* Bugfix in EM algorithm (found by Anne Helby Petersen)
* Correction in vignette example
* Regenerated example data, now it requires R 3.5+

bnstruct v1.0.4 (Release date: 2018-08-31)
===============

* Added qgraph option for plot()
* Parameters wm.max, initial.cpc exposed in learn.network
* Layering added to HC
* Mandatory edges can be specified to learn.network
* Modified way of imposing a maximum number of parent nodes for the different algorithms
* Bugfix in discretization (thanks to Amy Willis)

bnstruct v1.0.2 (Release date: 2016-12-13)
===============

* Second revision for paper submission

bnstruct v1.0.1 (Release date: 2016-11-10)
===============

* First revision for paper submission

bnstruct v1.0 (Release date: 2015-07-30)
===============

* First CRAN submission
