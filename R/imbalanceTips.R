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
  imbTips<-list()
  treemetrics<-imbalanceMetrics(tree)
  thrg<-threthold(tree,reps=100)
  seqtrees<-timeprune(tree)$trees[names(timeprune(tree)$trees)[3:tree$Nnode]]
  imbTips$thr<-lapply(seqtrees,function(x){threthold(x,reps)})
  imbTips$obs<-lapply(seqtrees,function(x){unlist(imbalanceMetrics(x)[1:12])})
  if(length(which(as.vector(unlist(treemetrics[1:12]))<thrg[2,]
                  |as.vector(unlist(treemetrics[1:12]))>thrg[1,]))>3)
  {
    ImbalanceTips<-NULL

    for(i in 3:(tree$Nnode)){
      if(length(which(obs[[i]]<thr[[i]][2,]|obs[[i]]>thr[[i]][1,]))>3)
      {
        imbTips$ImbalanceTips<-c(ImbalanceTips, names(timeprune(tree)$trees)[i])

      }
    }
   }
  else {imbTips$ImbalanceTips<-"This tree is balanced"}
    return(imbTips)
  }
