get_stats = function(object, var, rr = 3){
  s = summary(object)
  if(isS4(s)){
    bp = s@coef3[var,c(1, 4)] # get beta and p-value  
  } else{
    bp = s$coefficients[var,c("Estimate","Pr(>|t|)")] # get beta and p-value
  }
  bp = matrix(bp, ncol = 2)
  # pvalue to stars
  pvalues = bp[,2]
  stars = ifelse(bp[,2] < 0.001, "\\*\\*\\*", 
                 ifelse(bp[,2] < 0.01, "\\*\\*",
                        ifelse(bp[,2] < 0.05, "\\*", "")))
  
  bp[,2] = stars
  
  l = paste(signif(as.numeric(bp[,1]), rr), bp[,2], sep = "")  
  l = paste(l, collapse = "; ")
  l
}

model_output = function(objects, var, model_label){
  
  statistics = sapply(objects, get_stats, var)
  aic_values = sapply(objects, function(x) round(AIC(x), 2))
  n = nrow(objects[[1]]$model)
  
  out = c(model_label, N = n, statistics, aic_values)
  
  names(out) = c("Model", "N", "Bivariate", "Language family", "Division", "BV:AIC", "LF:AIC", "DV:AIC")
  
  out
}

musical_conversion = function(x, line_range){
  original_values = sapply(1:max(line_range), function(z) 2^z)
  coded_values = 1:max(line_range)
  
  matched_df = data.frame(coded_values = coded_values, 
                          original_values = original_values)
  
  x_df = data.frame(original_values = x)
  
  paired = dplyr::left_join(x_df, matched_df)
  
  (paired$coded_values - 1) / (max(line_range) - 1)
}

#' Convert glottolog tip labels
#'
#' @param X vector of labels.
#' @param nodeLabelsBy How should the node labels be returned?  Glottolog trees have nodes labelled with glottoids, language names and sometimes iso codes. Defaults to "glottoid" for returning glottoids.
#' @return vector of node labels
#' @keywords Glottolog
#' @examples
#' phy$tip.label = editGlottologTipLabels(phy$tip.label,"glottoid")
#' 
editGlottologTipLabels = function(X, nodeLabelsBy="glottoid"){
  item = 2 # glottoid by default
  if(is.numeric(nodeLabelsBy)){
    item = nodeLabelsBy
  } else{
    if(nodeLabelsBy=="name"){
      item = 1
    }
    if(nodeLabelsBy=="iso"){
      item = 3
    }
  }
  X = gsub("\\]","[",X)
  return(
    sapply(strsplit(X,"\\["),function(z){
      if(length(z)>=item){
        return(z[item])
      } else{
        return("")
      }
    }))
  
}

bind.tip<-function(tree,tip.label,edge.length=NULL,where=NULL){
  if(is.null(where)) where<-length(tree$tip)+1
  tip<-list(edge=matrix(c(2,1),1,2),
            tip.label=tip.label,
            edge.length=edge.length,
            Nnode=1)
  class(tip)<-"phylo"
  obj<-bind.tree(tree,tip,where=where)
  return(obj)
}


setNodeToTip = function(nodeName,tree, edge.length=0.001){
  
  bind.tip(tree,nodeName, edge.length,which(tree$node.label==nodeName)[1] + length(tree$tip.label))
  
}

nodesToTips = function(phy, nodesToFix = phy$node.label[phy$node.label!='']){
  for(nx in nodesToFix){
    phy = setNodeToTip(nx,phy)
  }
  return(phy)
}

downloadTree <- function(glottoid){
  url = paste("http://glottolog.org/resource/languoid/id/",glottoid,'.newick.txt',sep='')
  tx = readLines(url,warn = FALSE)
  return(tx[1])
}


#' Download glottolog trees from the web and convert to phylo objects
#'
#' @param glottoid The glottoid of the family to get (will be downloaded from the web) or local filename.
#' @param nodeLabelsBy How should the node labels be returned?  Glottolog trees have nodes labelled with glottoids, language names and sometimes iso codes. Defaults to "glottoid" for returning glottoids.
#' @param langNodesToTips Some languages in glottolog are not tips on the tree, but nodes with children (e.g. for languages with dialects).  If langNodesToTips is TRUE, the returned tree has extra tips for all nodes with non-blank labels.
#' @return phylo tree
#' @keywords Glottolog
#' @export
#' @examples
#' phy <- getGlottologTree("atla1278","glottoid")

getGlottologTree <- function(glottoid, nodeLabelsBy="glottoid", langNodesToTips=F){
  if(grepl("^[a-z][a-z][a-z][a-z][0-9][0-9][0-9][0-9]$",glottoid)){
    tx = downloadTree(glottoid)
  } else{
    tx = readLines(glottoid,warn = FALSE)[1]
  }
  
  # Add ending semicolon so that read.newick works
  if(substr(tx,nchar(tx),nchar(tx))!=";"){
    tx = paste(tx,";",sep='')
  }
  phy<-phytools::read.newick(text=tx)
  phy$tip.label = editGlottologTipLabels(phy$tip.label,nodeLabelsBy)
  phy$node.label = editGlottologTipLabels(phy$node.label,nodeLabelsBy)
  
  if(langNodesToTips){
    phy = nodesToTips(phy)
  }
  
  return(phy)
}
