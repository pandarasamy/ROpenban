\name{knn.test}
\alias{knn.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
KNN - test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
knn.test(data, model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, model) 
{
    library(rjson)
    library(class)
    json_obj = fromJSON(data)
    test = data.frame(json_obj)
    len = length(test)
    test = test[, 1:len - 1]
    len = length(model)
    train = model[, 1:len - 1]
    gt = model[, len]
    result = knn(train, test, gt, k = 3, prob = TRUE)
    return(data.frame(result))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
