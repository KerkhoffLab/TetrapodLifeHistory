## Stitch together a full tree from all taxa
require(phytools)
require(geiger)
# require(aRbor)
amphib <- readRDS("../output/data/amphibians.rds")
birds <- readRDS("../output/data/mckenchie.rds")
squam <- readRDS("../output/data/squamates.rds")
mamm <- readRDS("../output/data/newmammals.rds")
fish <- readRDS("../output/data/fish.rds")

tree_list <- list(amphib=ult_pruned_amphibiantree, birds=pruned_birdtree1, squam=ult_pruned_squamatetree, mamm=pruned_mammaltree_di)


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
vec <- c("amphib", "squam", "birds", "mamm")
ordertree <- drop.tip(phy = ordertree, tip = "fish")

otax <- data.frame("Class"= ordertree$tip.label, "Superclass"=c(rep("Tetrapoda",4)))
rownames(otax) <- ordertree$tip.label
classtree <- nodelabel.phylo(ordertree, otax, ncores=1)

class(tree_list) <- "multiPhylo"
plot(classtree)
abline(v = max(nodeHeights(tree_list[[1]])), lty = 2)
abline(v = max(nodeHeights(tree_list[[2]])), lty = 2)
abline(v = max(nodeHeights(tree_list[[3]])), lty = 2)
abline(v = max(nodeHeights(tree_list[[4]])), lty = 2)
abline(v=sapply(tree_list, function(x) max(nodeHeights(x))),lty=2)

res <- glomogram.phylo(classtree, tree_list)

tdsall <- lapply(tds, function(x) select(x, mean.mass, q10smr))
tdsall <- lapply(tdsall, function(x) mutate(x, lnMass=log(mean.mass), lnBMR=log(q10smr)))
dats <- lapply(tdsall, function(x) x$dat)
rn <- unname(unlist(sapply(tdsall, function(x) rownames(x$dat))))
resdat <- do.call(rbind, dats)
rownames(resdat) <- rn
td <- make.treedata(res, resdat)
plot(td$dat$lnMass, td$dat$lnBMR)

saveRDS(td, "../output/data/tetrapods.rds")

td <- reorder(td, "postorder")
tmp <- identifyBranches(td$phy, 30, plot.simmap=TRUE)


glomogram.phylo(classtree, trees[4])

plot(res)