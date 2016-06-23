#' Extracts suspicious tips of imbalance in a given tree
#'
#' \code{tiplength} returns suspicious tips in a given tree,
#'                 the observed value and 95% threshold for each sequence
#
#' @param tree a phylogenetic tree (as a \code{phylo} object)
#' @param repslications of bootstrapping
#' @return a vector of tips name that causes the imbalance
#' @author Yu Luo (\email{yu.luo3@mail.mcgill.ca})
#' @export


imbalanceTips<-function(tree,reps=100){
  treemetrics<-imbalanceMetrics(tree)
  thrg<-threthold(tree,reps=100)
  if(length(which(as.vector(unlist(treemetrics[1:12]))<thrg[2,]
                  |as.vector(unlist(treemetrics[1:12]))>thrg[1,]))>3)
  {
    ImbalanceTips<-NULL
    seqtrees<-timeprune(tree)$trees[names(timeprune(tree)$trees)[3:tree$Nnode]]
    thr<-lapply(seqtrees,function(x){threthold(x,reps)})
    obs<-lapply(seqtrees,function(x){unlist(imbalanceMetrics(x)[1:12])})
    for(i in 3:(tree$Nnode)){
      if(length(which(as.vector(unlist(imbalanceMetrics(timeprune(tree)$trees[[i]]))[1:12])<thr[[i]][2,|
                                as.vector(unlist(imbalanceMetrics(timeprune(tree)$trees[[i]])))[1:12]>thr[[i]][1,]))>3)
      {
        ImbalanceTips<-c(ImbalanceTips, names(timeprune(tree)$trees)[i])

      }
    }
    return(list(ImbalanceTips,thr,obs))
   }
  else (return(list("This tree is balanced",thr,obs)))}
