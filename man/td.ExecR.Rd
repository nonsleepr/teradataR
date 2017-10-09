\name{td.ExecR}
\alias{td.ExecR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ExecR Wrapper Function
}
\description{
This function is a wrapper function around the table operator ExecR.  It generates a string that can be passed into the function tdQuery to make a query that uses ExecR.
}
\usage{
td.ExecR(selectPhrase=string, ons=list(), returns=NULL, contract=NULL, operator=string, optional_operators=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selectPhrase}{
a character that represents a select phrase
}
  \item{ons}{
a list or character representation of on clauses 
}
  \item{returns}{
a file or character representation of a returns clause  
}
  \item{contract}{
a file or character representation of a contract clause  
}
  \item{operator}{
a file or character representation of an operator clause 
}
  \item{optional_operators}{
a character representation of any other needed operators 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A character representation of a query that will use the ExecR table operator when passed into the tdQuery function.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Erin Cole
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{on}}
}
\examples{
on1 <- on(target="select *", from="tab1", hash="c1", local_order="c2")
query <- td.ExecR(selectPhrase="select distinct * from TD_SYSGPL.ExecR", ons=on1, contract="~/Documents/contract.txt", operator="~/Documents/operator.txt")
res <- tdQuery(query)
print(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
