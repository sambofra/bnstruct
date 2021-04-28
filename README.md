bnstruct
========

R package for Bayesian Network Structure Learning from Data with Missing Values

Introduction
-----------

Bayesian Networks are a powerful tool for
probabilistic inference among a set of variables, modeled using a
directed acyclic graph. However, one often does not have the network,
but only a set of observations, and wants to reconstruct the network
that generated the data. The `bnstruct` package provides
objects and methods for learning the structure and parameters of the
network in various situations, such as in presence of missing data, for
which it is possible to perform *imputation* (guessing the missing
values, by looking at the data). The package also contains methods for
learning using the Bootstrap technique. Finally,
`bnstruct`, has a set of additional tools to use Bayesian
Networks, such as methods to perform inference and interventions.

In particular, the absence of some observations in the dataset is a very
common situation in real-life applications such as biology or medicine,
but very few software around is devoted to address these problems.
`bnstruct` is developed mainly with the purpose of filling
this void.

Installation
-----------
The latest stable version of `bnstruct` is available
[on CRAN](https://CRAN.R-project.org/package=bnstruct)
and can be installed with
```{r eval=FALSE}
install.packages("bnstruct")
```
from within an R session.

The latest development version of `bnstruct` can be found on GitHub
[here](https://github.com/sambofra/bnstruct).

In order to install the package, it suffices to launch
`R CMD INSTALL path/to/bnstruct`
from a terminal, or `make install` from within the package source folder.

Being hosted on GitHub, it is also possible to use the `install_github`
tool from an R session:

```{r eval=FALSE}
library("devtools")
install_github("sambofra/bnstruct")
```

For Windows platforms, a binary executable of the latest stable version is available
[on CRAN](https://CRAN.R-project.org/package=bnstruct).

`bnstruct` requires R `>= 3.5`, and depends on
`bitops`, `igraph`, `graph` and
`methods`. Package `Rgraphviz` is requested in
order to plot graphs, but is not mandatory.

Reference
---------
If you `bnstruct` in your work, please cite it as:
```
Alberto Franzin, Francesco Sambo, Barbara di Camillo. "bnstruct: an R package for Bayesian Network structure learning in the presence of missing data." Bioinformatics, 2017; 33 (8): 1250-1252; Oxford University Press.
```
These information and a BibTeX entry can be found with
```{r eval=FALSE}
citation("bnstruct")
```
