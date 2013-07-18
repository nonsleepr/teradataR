\name{td.kmeans}
\alias{td.kmeans}
\title{
K-Means Clustering
}
\description{
Perform k-means clustering on data in Teradata
}
\usage{
td.kmeans(x, centers, iter.max=10, nstart=1)
}
\arguments{
  \item{x}{
    td data frame
}
  \item{centers}{
    The number of clusters to produce. 
}
  \item{iter.max}{
    The maximum number of iterations allowed.
}
  \item{nstart}{
    How many random sets should be chosen from which to select the initial centers. 
}
}
\details{
The data given by x(data in Teradata) is clustered by the k-means method, which aims 
partition the points into k groups such that the sum of squares from points to the
assigned cluster centers is minimized.  At the minimum, all cluster centers are at the
mean of their Voronoi sets(the set of data points which are nearest to the cluster center).
}
\value{
An object of class "kmeans" which is a list with components:

{cluster}{For a td data frame this is NULL due to no data rows being in memory.}
{centers}{A matrix of cluster centers.}
{withinss}{The within-cluster sum of squares for each cluster.}
{size}{The number of points in each cluster.}
There is a print method for this class.
}
\examples{

\dontrun{
tdf <- td.data.frame("customer")
tkm <- td.kmeans(tdf, 3)
tkm   #print out kmeans class results
}

}
