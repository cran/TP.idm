\encoding{UTF-8}
\name{TP.idm-package}
\alias{TP.idm-package}
\alias{TP.idm}
\docType{package}
\title{
\packageTitle{TP.idm}
}
\description{
\packageDescription{TP.idm}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{TP.idm}
\packageIndices{TP.idm}

This package incorporates the function \code{TPidm} which can be used to compute a nonparametric transition probability matrix for the illness-death model. For a complete list of functions use \code{library(help=TP.idm)}.
}
\author{
\packageAuthor{TP.idm}

Maintainer: \packageMaintainer{TP.idm}
}
\references{
Andersen P. K., Borgan O., Gill R. D. and Keiding N. (1993). Statistical models based on counting processes. Springer Series in Statistics. New York, NY: Springer.

Datta S. and Satten G. A. (2001). Validity of the Aalen-Johansen estimators of stage occupation probabilities and Nelson-Aalen estimators of integrated transition hazards for non-Markov models. \emph{Statistics and Probability Letters} \bold{55(4)}, 403--411.

de Uña-Álvarez J. and Meira-Machado L. (2015). Nonparametric estimation of transition probabilities in a non-Markov illness-death model: a comparative study. \emph{Biometrics} \bold{71}, 364--375.

Meira-Machado L. F., de Uña-Álvarez J. and Cadarso-Suárez C. (2006). Nonparametric estimation of transition probabilities in a non-Markov illness-death model. \emph{Lifetime Data Annal} \bold{12(3)}, 325--344.
}
\keyword{ package }
%%\seealso{
%%~~ Optional links to other man pages, e.g. ~~
%%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%%}
\examples{
data(colonTP)

# create a TPidm object with s = 0, t = "last" (default),
# CI = TRUE and method = "AJ":
tp_s0_aj_ci <- TPidm(colonTP, s = 0, method = "AJ", CI = TRUE)
summary(tp_s0_aj_ci) # summarizing the results
plot(tp_s0_aj_ci) # plotting all occupation probabilities
}
