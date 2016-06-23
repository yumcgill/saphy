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
  if(length(which(as.vector(unlist(treemetrics[1:12]))<threthold(tree,reps=100)[2,]
                  |as.vector(unlist(treemetrics[1:12])>threthold(tree,reps=100)[1,])))>1)
  {
    ImbalanceTips<-NULL
    for(i in 1:tree$Nnode){
      if(!any(unlist(imbalanceMetrics(timeprune(tree)$trees[[i]]))[1:12]<threthold(timeprune(tree)$trees[[i]],reps=100)[2,]|
              unlist(imbalanceMetrics(timeprune(tree)$trees[[i]]))[1:12]>threthold(timeprune(tree)$trees[[i]],reps=100)[1,]))
      {
        ImbalanceTips<-c(ImbalanceTips,timeprune(tree)$trees[[i]]$tip.label[1])
      }
    }
    return(ImbalanceTips)}
  else (return("This tree is balanced"))}
