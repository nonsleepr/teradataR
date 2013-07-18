\name{td.join}
\alias{td.join}
\title{
Join Tables in Teradata
}
\description{
Join tables in Teradata based on their td data frames and index values.
}
\usage{
td.join(tdf1, tdf2, otable = "newJoinTable", odatabase = "", index1 = "", index2 = "", joinType = "inner")
}
\arguments{
  \item{tdf1}{
    first td data frame of table one.
}
  \item{tdf2}{
    second td data fame of table two.
}
  \item{otable}{
    output table name of result.
}
  \item{odatabase}{
    output database name of result.
}
  \item{index1}{
    index columns to use for the join from table one.
}
  \item{index2}{
    index columns to use for the join from table two.
}
  \item{joinType}{
    join type.  One of ("inner", "left outer", "right outer", "full outer").
}
}
\details{
Allow joining of columns from two Teradata tables.  The columns in the td data frames
are used as the new column names along with the index from the first table.  If
indices are missing the primary indices will be searched and used.
}
\value{
td data frame pointing to the new output table from the join.
}
\examples{

\dontrun{
td.join(tdf, tdf2, "myNewTable",index1=c("pidx1","pidx2"), index2=c("pidx1","pidx2"))
}

}
