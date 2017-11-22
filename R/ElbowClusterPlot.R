#' Function to generate a Cluster Elbow Plot.
#'
#' Makes an Elbow plot, which are used to choose the number of clusters to use when applying kmeans clustering on a data set. The y axis shows the sum of
#' within cluster deviations from the center of that cluster (distortion). This will decrease as we increase the number of clusters naturally. Making an elbow
#' plot allows us to look for the "elbow" of this curve, which is one criterion for choosing the number of clusters to look for in a data set.
#'
#' @param mydf this is the data frame we are clustering, all columns should be numeric (or be dropped if they aren't)
#' @param maxClusters this defaults to 10, gives the maximum number of clusters you want to include in the plot.
#' @param drops defaults to NULL (none) use this if you want to drop one of more columns from the clustering data set.
#' @param scale defaults to TRUE. This will scale each column to have a mean of zero and standard deviation of one. This prevents the clustering algrorithm from
#' overweighting one column over another and can allow for easier interpretations of the results.
#' @return The kmeans cluster object. It will also print the centers of the clusters to the command line.
#' @examples
#' data(crime_cluster) #load the data crime_cluster
#' ElbowClusterPlot(crime_cluster) #make the plot with two clusters
#' @export




ElbowClusterPlot <- function(mydf, maxClusters=10, drops=NULL, scale=TRUE) {

  mydf$cluster<-NULL
  mydf<-mydf[ , !(names(mydf) %in% drops)]

  if (scale==TRUE) {
    mydf<-data.frame(scale(mydf)) #if asked for re-scale the variables
  }
  clusterValues<-seq(1,maxClusters)
  withinss.measure<-rep(1,length(clusterValues))

  for (i in 1:maxClusters) {
    cl_obj=kmeans(mydf, centers=clusterValues[i], nstart=25)
    withinss.measure[i]<-cl_obj$tot.withinss
  }
  plot(clusterValues, withinss.measure, type='b', xlab='Number of Clusters', ylab='Distortion', col='red', main='Elbow Plot for Cluster Analysis')
}
