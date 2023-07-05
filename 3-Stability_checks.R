# -------------------------------------------------------------------------
# --------------------- Stability Checks ----------------------------------
# -------------------------------------------------------------------------
exnet <- function(data){
  mgm <- mgm(data = as.matrix(data), 
                type = c(rep("g", 8), rep("c", 3)), 
                level = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 7, 2), 
                lambdaSel = "EBIC", 
                lambdaGam = 0.5)

w <-mgm[["pairwise"]][["wadj"]]
w <- w[1:7, 1:7]
return(w)
}

library(bootnet)
set.seed(1)
networkt <- estimateNetwork(finalAll,
                           fun = exnet,
                           labels = labels)

plot(networkt, layout = "spring", labels= labels)

save(networkt, file="network_bridge 18-5-2022.Rdata")

## ---------------------------------------------------------
## Estimate edge weight CIs using non-parametric bootstrap
## ---------------------------------------------------------
library(bootnet)
set.seed(1)
nonParBoott <- bootnet(networkt, nBoots=1000, ncores=8, type = "nonparametric", 
                      statistics="all", communities= groups, labels= labels)

CIs_bridge <- summary(nonParBoott)

save(nonParBoott, file="nonParBoot_bridge 11-5-2022.Rdata")

## Plot edge weight CIs 
library(ggpubr)
#png(file="Edge weight CIs 11-5-2022.png", width=4, height=5,res=600, units="in", bg="white")
edgeweights <- annotate_figure(ggarrange(
  plot(nonParBoott, order="sample", labels=F, legend=F, meanColor=NA) + 
    xlab("Edge Weight") +  
    theme(plot.title=element_text(hjust=0.5, size=10, face="bold"),
          strip.text=element_blank(), # remove "edge" header
          axis.text.x=element_text(size=9)), 
    ncol=1, nrow=1, common.legend=F))
#dev.off()

ggsave(filename= "Edge weight CIs.png", plot= edgeweights, dpi= 600, units="in", 
       width=3, height=4)
ggsave(filename= "sup H6.1.pdf", plot= edgeweights,units="in", 
       width=6, height=8, device="pdf")

## ---------------------------------------------------------
## Estimate centrality stability using case-drop bootstrap
## ---------------------------------------------------------
set.seed(1)
caseBoott <- bootnet(networkt, type="case", nBoots=1000, ncores=8, 
                    statistics="all", communities = groups, labels = labels)

save(caseBoott, file="caseBoot_bridge 11-5-2022.Rdata") 

## Compute CS-coefficients
CS_coefficientB <- corStability(caseBoott)

CS <- plot(caseBoott, statistics=c("expectedInfluence"), legend=T)

## Plot centrality stability 
ggsave(filename= "Centrality stability.png", plot= CS, dpi= 600, units="in", 
       width=3, height=4)
ggsave(filename= "sup H6.4.pdf", plot= CS,units="in", 
       width=6, height=8, device="pdf")
# ---------------------------------------------------------
## Difference tests
## ---------------------------------------------------------
## note: Gray = nodes or edges that do NOT significantly differ from each other 
## Black = nodes or edges that significantly differ from one another (does not control for multiple testing)

## Edge weights difference test
edgeDiffTest <- 
  plot(nonParBoott, "edge", plot="difference", onlyNonZero=T, order="sample") + 
    theme(plot.title=element_text(hjust=0.5, size=10, face="bold"),
        strip.text=element_blank(), # remove "edge" header
        axis.text.x=element_text(size=5),
        axis.text.y=element_text(size=5))

ggsave(filename= "Edge weight differences.png", plot= edgeDiffTest, dpi= 600, units="in", 
       width=7, height=5)
ggsave(filename= "sup H6.2.pdf", plot= edgeDiffTest,units="in", 
       width=7, height=5, device="pdf")

## Centrality difference tests
centralitydiff_EI <- annotate_figure(ggarrange(
  plot(nonParBoott, "expectedInfluence", plot="difference", order="sample")+ 
       theme(plot.title=element_text(hjust=0.5,face="bold",size=10)),
    ncol=1, nrow=1, common.legend=F))

## Plot centrality difference tests 
ggsave(filename= "Centrality differences EI.png", plot= centralitydiff_EI, dpi= 600, units="in", 
       width=7, height=5)
ggsave(filename= "sup H6.3.pdf", plot= centralitydiff_EI,units="in", 
       width=7, height=5, device="pdf")
#dev.off()


############################################################
## Summary of network and strongest edges
############################################################
cor.test(centrality(w)$InExpectedInfluence, 
         as.numeric(sapply(finalAll[,1:k], function(x) sd(x, na.rm=TRUE))))

cor.test(centrality(w)$InExpectedInfluence, 
         as.numeric(sapply(finalAll[,1:k], function(x) mean(x, na.rm=TRUE))))

cor.test(centrality(w)$InExpectedInfluence, 
         as.numeric(sapply(finalAll[,1:k], function(x) var(x, na.rm=TRUE))))

cor.test(centrality(w)$InExpectedInfluence, 
         as.numeric(sapply(finalAll[,1:k], function(x) sum(x<1, na.rm=TRUE))))

sum(w!=0)/2 # number of nonzero edges (14)
sum(w<0)/2 # number of negative edges (0)
sum(w>0)/2 # number of positive edges (14)
(sum(w>0)/2) / (sum(w!=0)/2) # percentage of edges that were positive (100%)

sum(apply(w[1:4, which(groups=="Symptoms")], 2, function(x) sum(x!=0))) #cross-domain edges (6)
sum(apply(w[5:7, which(groups=="Symptoms")], 2, function(x) sum(x!=0)))/2 #within-domain edges (3)
sum(apply(w[1:4, which(groups=="Existential factors")], 2, function(x) sum(x!=0)))/2 #within-domain edges (5)

## Identify the strongest edges (note: row=IV, col=DV)
res <- order(w, decreasing = T)[seq_len(49)] #CHANGE seq_len value!!
pos <- arrayInd(res, dim(w), useNames = TRUE)

posWithLabs <- data.frame(nodeOut=labels[pos[,1]], 
                          nodeIn=labels[pos[,2]],
                          value=w[res])

## ---------------------------------------------------------------------------------------
## Significant differences between specific edges
## ---------------------------------------------------------------------------------------

## For loop to calculate number of significant differences in centrality between a specified node and all others
DiffCentMat <- NULL
k <- 7
for (i in 1:(k-1)) {
  diffTest <- differenceTest(nonParBoott, "Aut", 
                             colnames(w)[-which(colnames(w)=="Aut")][i], 
                             "expectedInfluence", verbose=F)
  DiffCentMat <- rbind(DiffCentMat, diffTest)
}
DiffCentMat
sum(DiffCentMat$significant==TRUE) #4

## Edges
sum(subset(edgeDiffTest$data, id1=="Rec--Anx")$fill=="sig") #9
