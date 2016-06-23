#' Extracts suspicious tips of imbalance in a given tree
#'
#' \code{tiplength} returns suspicious tips in a given tree
#'
#' @param tree a phylogenetic tree (as a \code{phylo} object)
#' @param repslications of bootstrapping
#' @return a vector of tips name that causes the imbalance
#' @author Yu Luo (\email{yu.luo3@mail.mcgill.ca})
#' @export
imbalanceTips<-function(tree,reps=100){
  treemetrics<-imbalanceMetrics(tree)
  thrg<-threthold(tree,reps=100)
  if(length(which(as.vector(unlist(treemetrics[1:12]))<thrg[2,]
                  |as.vector(unlist(treemetrics[1:12])>thrg[1,])))>1)
  {
    ImbalanceTips<-NULL
    for(i in 3:(tree$Nnode-1)){
      thr<-threthold(timeprune(tree)$trees[[i]],reps=100)
      if(!any(unlist(imbalanceMetrics(timeprune(tree)$trees[[i]]))[1:12]<thr[2,]|
              unlist(imbalanceMetrics(timeprune(tree)$trees[[i]]))[1:12]>thr[1,]))
      {
        ImbalanceTips<-c(ImbalanceTips, names(timeprune(tree)$trees)[i])
      }
    }
    return(ImbalanceTips)}
  else (return("This tree is balanced"))}
