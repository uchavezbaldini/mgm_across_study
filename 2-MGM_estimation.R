# -------------------------------------------------------------------------
# ------------------- Network Estimation ----------------------------------
# -------------------------------------------------------------------------

## ---------------------------------------------------------
## Load packages
## ---------------------------------------------------------
library(mgm)
library(qgraph)
library(xlsx)

## ---------------------------------------------------------
## Get Data for Network
## ---------------------------------------------------------
longlabels <- c("Autonomy",
                "Recognition of psychiatric disorder",
                "Social",
                "Identity",
                "Anxiety symptoms",
                "Depression symptoms",
                "Psychotic symptoms")

labels <- c("Aut","Rec","Soc","Id","Anx","Dep","Psy")

labels_all <- c("Aut","Rec","Soc","Id","Anx","Dep","Psy",
                "Age", "Gen", "Diag", "Med")

groups <- c(rep("Existential factors",4), rep("Symptoms",3))
groups_all <- c(rep("Existential factors",4), rep("Symptoms",3), rep("Covariates",4))


colnames(finalAll) <- labels_all

data_all <- list("data" = NULL, 
                  "type" = NULL, 
                  "level" = NULL,
                  "names" = NULL,
                  "labels" = NULL,
                  "grouplabels"= NULL)

data_all$data <- as.matrix(finalAll)
data_all$type <- c(rep("g", 8), rep("c", 3))
data_all$level <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 7, 2)
data_all$labels <- labels_all
data_all$grouplabels <- groups

save(data_all, file="data_all 11-5-2022.Rdata")

## ---------------------------------------------------------
## Network Estimation
## ---------------------------------------------------------
library(mgm)
# Fit model: Whole sample
mgmAll <- mgm(data = data_all$data, 
              type = data_all$type, 
              level = data_all$level, 
              lambdaSel = "EBIC", 
              lambdaGam = 0.5)

w <-mgmAll[["pairwise"]][["wadj"]]
colnames(w) <- labels_all
rownames(w) <- labels_all

library(xlsx)
write.xlsx(round(getWmat(w), digits = 3), file = "NetworkMatrix_All.xlsx")

w <- w[1:7, 1:7]

e <- mgmAll$pairwise$edgecolor_cb
e <- e[1:7, 1:7]

#Plot network paths
library(qgraph)

shapes <- c(rep("circle",4), rep("diamond",3))

tiff("Network 27-2-2023.tiff", width= 20, height=14, units= "cm", res= 500, compression = "lzw")

png(file="Network.png",width=6,height=6,res=600,units="in",bg="white")

pdf(file="H6.1.pdf",width=6,height=6)

qgraph(w, layout = "circle", labels = labels, groups=groups, theme="colorblind",
       edge.color = e, nodeNames= longlabels, legend.mode= "style1", 
       layoutOffset= c(-0.1,0), layoutScale= c(1,1), legend.cex= 0.5, 
       GLratio= 1.7)

ebicnetwAll <- qgraph(w, layout = "circle", labels = labels, groups=groups, theme="colorblind",
               edge.color = e, legend= FALSE)

dev.off()

save(ebicnetwAll, file="ebicnetwAll 11-5-2022.Rdata")

## ---------------------------------------------------------
## Centrality plots
## ---------------------------------------------------------
library(qgraph)
library(ggpubr)

centralityfig <-   annotate_figure(
  centralityPlot(w, labels=labels, orderBy = "ExpectedInfluence", include =c("ExpectedInfluence")) + 
    xlab("Standardized Centrality Coefficients") + ylab("Nodes") + 
    theme(axis.title.x=element_text(size=7), axis.title.y=element_text(size=7)))

ggsave(filename= "Centrality plot 27-2-2023.tiff", plot= centralityfig, dpi= 500, units="cm", 
       width=6, height=8,compression = "lzw")

ggsave(filename= "H6.2.pdf", plot= centralityfig,units="in", 
       width=6, height=8, device="pdf")
