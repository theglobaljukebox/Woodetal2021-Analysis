# Tree built from Glottolog and following:
# Roberts, S. G., Winters, J., & Chen, K. (2015). Future Tense and Economic Decisions: Controlling for Cultural Evolution. PLOS ONE, 10(7), e0132145. https://doi.org/10.1371/journal.pone.0132145

suppressPackageStartupMessages({
  library(dplyr)
  library(ape)
  library(phytools)
  library(phangorn)
  library(readxl)
  library(stringr)
  library(geiger)
  library(assertthat)
})

source('correlations/helper.R')

## -- Parameters -- ##
years = 6000 # how old the LF trees should be

# -- Data -- #
data = read.csv(file = "data/cantometrics_ethnographicatlas.csv")
glottolog = read.csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/csv/glottolog.csv")


## Manual adjustments
## Currently: "Bulgarian: bulg1262; Macedonian: mace1250"
## Pick bulg1262
data$GlottoID[data$soc_id == 12739] = "bulg1262"
# Currently: "Mal: mall1246; Prai: phai1238"
# Pick mall1246
data$GlottoID[data$soc_id == 20109] = "mall1246"
# Currently: "Moksha: moks1248; Erzya: erzy1239"
# Pick moks1248
data$GlottoID[data$soc_id == 21704] = "moks1248"
# Currently: panj1259 (which is not a glottocode)
# Preferred name is: Punjabi
# Glottolog has panj1256 (for eastern Punjabi) & west2386 (for Western Punjabi)
# Choose panj1256, since this was likely a typo in glottocode recording. 
data$GlottoID[data$soc_id == 24014] = "panj1256"

languages = left_join(data, glottolog, by = c("GlottoID" = "id"))

#### Family changes
# Family was jodi1234; part of the Jodi-Saliban LF.
# The Glottocode piar1243 is shown to be in the Saliban family now
# Change code to sali1297
languages$FamilyLevGlottocode[languages$GlottoID == "piar1243"] = "sali1297"

languages = languages[!is.na(languages$GlottoID),]

assertthat::assert_that(sum(is.na(languages$GlottoID)) == 0)

unq_languages = unique(languages$GlottoID)

families = unique(languages$FamilyLevGlottocode)
families = families[families != ""]
families = str_remove(families, "ISOLATE_")
dir.create('correlations/glottolog_trees')
isolate = c()
for(f in families){
  print(f)
  tre = getGlottologTree(f)
  
  all_keeps = c(unq_languages[unq_languages %in% tre$node.label],
                unq_languages[unq_languages %in% tre$tip.label])
  
  save = FALSE
  # make nodes that I have into tips
  if(any(tre$node.label %in% unq_languages)){
    nodes = tre$node.label[tre$node.label %in% unq_languages]
    for(nod in nodes){
      print(nod)
      n_node = length(tre$tip.label) + which(tre$node.label == nod)
      tre = add.tips(tre, tips = nod, where = n_node)
      save = TRUE
    }
  }
  
  # remove tips I don't use
  if(any(tre$tip.label %in% unq_languages)){
    keep = tre$tip.label[tre$tip.label %in% unq_languages]
    tre = drop.tip(tre, tip = setdiff(tre$tip.label, keep))
    save = TRUE
  }
  
  # if we have only one tip, save as an isolate a patch onto the supertree
  if(length(all_keeps) == 1){
    isolate = c(isolate, all_keeps)
  }
  
  if(save){
    if(length(tre$tip.label) == 1){
      tre$edge.length = years
    } else{
      tre = compute.brlen(tre, method = 'Grafen', power = 1) # Grafen is default, with power = 1
      tre = rescale(tre, "depth", years)  
    }
  }
  ape::write.tree(tre, paste0('correlations/glottolog_trees/', f, '.nex'))  
}

## - Paste trees together -- ## 
tree_list = list.files('correlations/glottolog_trees/', full.names = TRUE)

# build a tree from isolates
out = "("
## add families
for(t in tree_list){
  language_tree = readChar(t, file.info(t)$size)
  language_tree = str_replace(language_tree, ";\n", ":54000,")
  out = paste0(out, language_tree)
}

out = str_sub(out, end=-2)

out = paste0(out, ");")

## Write tree
fileConn = file("data/super_tree.nwk")
  writeLines(out, fileConn)
close(fileConn)

# Check tree is valid
tree = read.tree('data/super_tree.nwk')

## Delete temporary tree files
system('rm -r correlations/glottolog_trees')
