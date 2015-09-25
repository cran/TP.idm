\name{plot.TPidm}
\alias{plot.TPidm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot method for a TPidm object
}
\description{
Plot method for an object of class \sQuote{TPidm}. It draws the estimated occupation probabilities (if s = 0) or transition probabilities (if s > 0) and confidence intervals (only available if \code{CI} is \code{TRUE}) in a basic scatterplot or in a multipanel layout.
}
\usage{
\method{plot}{TPidm}(x, chosen.tr = "ALL", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \sQuote{TPidm}.
}
  \item{chosen.tr}{
Character vector of the form \sQuote{c(\dQuote{from to}, \dQuote{from to})} specifying which transitions should be plotted. Default, all the transition probabilities are plotted.
}
  \item{\dots}{
Further arguments to plot.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
%%\references{
%% ~put references to the literature/web site here ~
%%}
%%\author{
%%  ~~who you are~~
%%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
data(colonTP)

# create a TPidm object with s = 0, t = "last" (default),
# CI = FALSE (default) and method = "NM" (default):
tp_s0_nm <- TPidm(colonTP, s = 0)

# plot all possible transitions without CI:
plot(tp_s0_nm)

# or 
# create a TPidm object with s = 0, t = "last" (default),
# CI = TRUE and method = "AJ":
tp_s0_aj_ci <- TPidm(colonTP, s = 0, CI = TRUE, method = "AJ")

# plot transitions "1 2" and "1 3" with CI:
plot(tp_s0_aj_ci, chosen.tr = c("1 2", "1 3"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
