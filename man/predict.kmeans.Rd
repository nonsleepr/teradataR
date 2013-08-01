\name{predict.kmeans}
\alias{predict.kmeans}
\title{
Kmeans Model Prediction
}
\description{
predict is a generic function for predictions from the results of various model fitting functions.  This function is invoked when predict is used on a class of type 'kmeans'.
}
\usage{
\method{predict}{kmeans}(object, newdata=NULL, oTable="", oDatabase="", \dots)
}
\arguments{
  \item{object}{
    object of type 'kmeans'.
}
  \item{newdata}{
    td data frame or data frame to predict.
}
  \item{oTable}{
    Output table name if a td data frame is being predicted.
}
  \item{oDatabase}{
    Output database name if a td data frame is being predicted.
}
  \item{\dots}{
    Generic parameter list.
}
}
\details{
When using this function with a td data frame you must at least include the 'oTable' parameter for the table name that will be
written in the Teradata database.  An optional 'oDatabase' parameter is included to allow creating the table in other databases
within the Teradata database.  The prediction will build a table from the td data frame including all the column names and
adding a new column called 'clusterID' which will have the predictions from applying the 'kmeans' object to your Teradata
table pointed to in your td data frame.  This function is also able to score a regular R data frame object.
}
\value{
A td data frame will be returned if your 'newdata' object is of type td data frame.  It will point to your newly created table.
A vector of cluster scores will be returned if you are scoring a regular data frame.
}
\examples{

\dontrun{
tdf <- td.data.frame("customer")
tkm <- td.kmeans(tdf[1:5], centers=3) #use columns 1-5 to cluster on
newTbl <- predict(tkm, tdf, "newcustomerwithcluster")
}

}
