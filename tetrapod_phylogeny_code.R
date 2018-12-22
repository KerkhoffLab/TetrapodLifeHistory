## Stitch together a full tree from all taxa
require(phytools)
require(geiger)

tree_list <- list(amphib=amphibiantree, birds=birdtree1, squam=squamatetree, mamm=mammaltree_best)
tree_list <- list(amphib=amphibiantree, birds=birdtree1)

tip.labels <- c("fish", "amphib", "squam", "birds", "mamm")
edge <- matrix(c(9, 4,
                 9, 3,
                 8, 5,
                 8, 9,
                 7, 8,
                 7, 2,
                 6, 7,
                 6, 1), byrow=TRUE, ncol=2)
edge.length <- c(274.9, 274.9, 324.5, 324.5-274.9, 382.9-324.5, 382.9, 454.6-382.9 , 454.6)
Nnode <- 4
ordertree <- list(edge=edge, Nnode=Nnode, tip.label=tip.labels, edge.length=edge.length)
class(ordertree) <- 'phylo'
ordertree <- reorder(ordertree, "postorder")
plot(ordertree)
ordertree <- drop.tip(phy = ordertree, tip = "fish")
ordertree <- drop.tip(phy = ordertree, tip = c("squam", "mamm"))
plot(ordertree)

otax <- data.frame("Class"= ordertree$tip.label, "Superclass"=c(rep("Tetrapoda",4)))
otax <- data.frame("Class"= ordertree$tip.label, "Superclass"=c(rep("Tetrapoda",2)))
rownames(otax) <- ordertree$tip.label
classtree <- nodelabel.phylo(ordertree, otax, ncores=1)

class(tree_list) <- "multiPhylo"
plot(classtree)
abline(v = max(nodeHeights(tree_list[[1]])), lty = 2)
abline(v = max(nodeHeights(tree_list[[2]])), lty = 2)
abline(v = max(nodeHeights(tree_list[[3]])), lty = 2)
abline(v = max(nodeHeights(tree_list[[4]])), lty = 2)
# abline(v=sapply(tree_list, function(x) max(nodeHeights(x))),lty=2)

res <- glomogram.phylo(classtree, tree_list)

plot(res)