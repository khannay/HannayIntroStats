#' Function to plot kmeans clusters in the US States
#'
#' This function is useful to visualize clusters of states. It uses the kmeans function to form clusters of the states and then provides a visualization
#' of the data. State names should be saved as the row.names of the passed in data frame. Depends on the map function from the maps package.
#'
#' @param numClusters defaults to 2, tells the function how many clusters you want to form on your data set
#' @param mydf this is the data frame to perform the kmeans clustering on. By default all columns of the data will be used.
#' this data frame must have the state names as the row names. It cannot currently handle abbreviations of the states names.
#' @param drops defaults to NULL (none) use this if you want to drop one of more columns from the clustering data set
#' @param scale defaults to TRUE. This will scale each column to have a mean of zero and standard deviation of one. This prevents the clustering algrorithm from
#' overweighting one column over another and can allow for easier interpretations of the results.
#' @return The kmeans cluster object. It will also print the centers of the clusters to the command line.
#' @examples
#' data(crime_cluster) #load the data crime_cluster
#' StatePlot(2, crime_cluster) #make the plot with two clusters
#' @export




StatePlot <- function(numClusters=2, mydf, drops=NULL, scale=TRUE) {

  mydf$cluster<-NULL
  mydf<-mydf[ , !(names(mydf) %in% drops)]

  if (scale==TRUE) {
    mydf<-data.frame(scale(mydf)) #if asked for re-scale the variables
  }

  #Do the clustering using kmeans
  cl_obj<-kmeans(mydf, numClusters, nstart=25)
  mydf$cluster<-cl_obj$cluster

  #Get the map names for the states
  namevec <- map(database = "state", col = "blue",fill=T, namesonly=TRUE, plot = FALSE);
  row.names(mydf)<-tolower(row.names(mydf))
  namevec2<-gsub(":.*", "", namevec) #remove the colons from the names
  mycolors<-mydf[namevec2,]$cluster

  #Make the actual map
  color.list=c("darkgreen", "yellow", "red", "blue", "cyan", "lightblue", "coral")
  map(database = "state",col =color.list[mycolors],fill=TRUE)
  report_clusters<-data.frame(cl_obj$centers)
  report_clusters$colors<-color.list[1:numClusters]
  print(report_clusters)
  return(cl_obj)
}

