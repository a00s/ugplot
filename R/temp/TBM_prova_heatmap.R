library(heatmap3)
# library(ggplot)

# gene expression colors
my_palette <- colorRampPalette(c("blue","white","red"))(n = 50)

# methylation colors
my_palette <- colorRampPalette(c("blue","white","yellow"))(n = 50)


rand.mat <- matrix(data = rep(1, 9), ncol = 3, nrow=3)
colnames(rand.mat) <- c("lol1", "lo", "pol")
rownames(rand.mat) <- c("gen1", "ge", "LDCR1")

patient.list <- c("lo", "lol1", "peppppppp")
selected.genes <- c("ge", "LDCR1")

# take only selected patients
take.col <- pmatch(patient.list, colnames(rand.mat))
take.col <- take.col[which(is.na(take.col)==F)]
rand.mat <- rand.mat[, take.col]

# take only selected genes
take.row <- pmatch(selected.genes, rownames(rand.mat))
take.row <- take.row[which(is.na(take.row)==F)]
rand.mat <- rand.mat[take.row, ]


# take colors from file

# col1 <- read.table(....)[,1]
col1 <- c(rep("blue", 40), rep("red", 40))
col2 <- c(rep("white", 40), rep("black", 40))





# Rowside colors inside hearmap 3 to change gene rows color

MakeHeatmap <- function(table.name = "dataset_finto.txt",
                        table.read=T,
                        plot.name = "plot1.png", log.transform = T,
                        selected.genes = c("K", "ABC", "BRCAX", "ALP1"), method.corr = "pearson",# spearman
                        method.dist = "average", color1 =  c(rep("blue", 40), rep("red", 40)), color2=c(rep("white", 40), rep("black", 40)),
                        class.type1 = c(rep("ttl", 80)),
                        class.type2 = c(rep("llol", 80)),
                        width.plot = 7, height.plot = 5,
                        patient.list = c("TCGA.YZ.A980.01A.11R.A405.07", "TCGA.YZ.A983.01A.11R.A405.07", "TCGA.YZ.A985.01A.11R.A405.07", "TCGA.WC.A88A.01A.11R.A405.07"),
                        col.clust = "both", margin.plot = c(0,10), filter.matrix = F, thr.gene = 15, thr.pat = 13){
  my_palette <- colorRampPalette(c("blue","white","red"))(n = 50) # colorRampPalette(c("blue","black","red"))(n = 50)
  if(table.read){
    dataset <- read.delim(table.name, header=TRUE, sep="\t",row.names = 1)
  }else{
    dataset <- table.name
  }

  if(log.transform){
    dataset <- log(dataset+1)
  }

  # transpose matrix to have patients in rows
  dataset <- t(dataset)

  if(length(patient.list)>0){# if you'll plot only a list of patients
    color1 <- color1[pmatch(patient.list, rownames(dataset))]
    color2 <- color2[pmatch(patient.list, rownames(dataset))]
    class.type1 <- class.type1[pmatch(patient.list, rownames(dataset))]
    class.type2 <- class.type2[pmatch(patient.list, rownames(dataset))]
    dataset <- dataset[pmatch(patient.list, rownames(dataset)), ]
  }


  # create class1 vector
  # create color labels
  col1 <- color1
  labels.class <- class.type1

  if(F){
    # assign to each element its group color label
    for(i in (1:length(labels.class))){
      col1[which(col1==labels.class[i])] <- class.labels[i]
    }
  }

  # cat("check labels order: \n", paste(labels.class, "->", class.labels, "\n"))
  # take only selected genes
  take.col <- pmatch(selected.genes, colnames(dataset))
  if(any(is.na(take.col))){
    cat("error! gene IDs not found: \n", selected.genes[which(is.na(take.col))], "\n")
    cat("check gene IDs or if they are present in multiple rows of the gene expression column\n (different ensembl ID, but same gene ID)\n")
  }
  take.col <- take.col[which(is.na(take.col)==F)]
  dataset <- dataset[, take.col]
  print(ncol(dataset))
  print(nrow(dataset))

  dataset <- t(dataset)
  # change dataset colnames (sample IDS) for plot
  dat.names <- gsub("_", ".", colnames(dataset))
  colnames(dataset) <- paste(gsub("\\.", "", gsub("[0-9]", "", dat.names)), gsub("[A-Z]", "", dat.names))

  print(dataset[c(1,2), ])

  if(filter.matrix){
    scale.mat <- t(scale(t(dataset)))
    scale.mat <- round(scale.mat, digits=0)
    # remove samples with high number of 0
    zer.gene <- sapply(1:ncol(scale.mat), function(x){length(which(scale.mat[x, ]==0))})
    zer.samp <- sapply(1:ncol(scale.mat), function(x){length(which(scale.mat[,x]==0))})

    dataset <- dataset[which(zer.gene < thr.gene), which(zer.samp < thr.pat)]
    col1 <- col1[which(zer.samp < thr.pat)]
  }

  # add S color columns
  col2 <- color2
  if(method.corr != ""){ # you don't use euclidean
    #Spearman HCL
    hr <- hclust(as.dist(1-cor(t(dataset), method = method.corr)), method = method.dist)
    hc <- hclust(as.dist(1-cor(dataset, method = method.corr )), method = method.dist)
    png(plot.name, width = width.plot, height = height.plot, units = 'in', res = 300)
    if(is.na(col.clust)){
      heatmap.3(dataset, dendrogram = ifelse(is.na(col.clust), 'row', 'both'), Rowv=as.dendrogram(hr),
                Colv = NA,
                col = my_palette, keysize = 1.0,
                symkey = FALSE, density.info ='none', trace = 'none',scale="row",
                labRow=NA, ColSideColors = col1, color_branches(dend1, k=4), breaks=seq(-1,1,length.out = length(my_palette)))
    } else {
      heatmap3(dataset, dendrogram = ifelse(is.na(col.clust), 'row', 'both'), Rowv=as.dendrogram(hr),
               Colv = as.dendrogram(hc),
               col = my_palette, keysize = 1.0,
               symkey = FALSE, density.info ='none', trace = 'none',scale="row", RowSideColors = rep("blue", nrow(dataset)),#labRow = NA,
               ColSideColors = cbind(col2,col1), breaks=seq(-4,4,length.out = (length(my_palette)+1)), ColSideLabs = c("rrrrr", "treatment"),
               margin=margin.plot)
    }
    #ggsave(plot.name, device="png")


    #legend(x="topright",
    #       legend = c(unique(class1), "NO333", "S333310"),
    #       col = c(unique(col1), unique(col2)),
    #       lty= 1,
    #       lwd = 4,
    #       cex=.6,
    #)
    dev.off()
  } else {
    myclust=function(dataset) hclust(dataset, method = method.dist)
    mydist = function(dataset) dist(dataset, method = "euclidean")
    png(plot.name, width = width.plot, height = height.plot, units = 'in', res = 300)
    heatmap.3(dataset, hclustfun=myclust, distfun=mydist, dendrogram = ifelse(is.na(col.clust), 'row', 'both'), col = my_palette,
              keysize = 1.0, symkey = FALSE, density.info ='none', trace = 'none', # colsep = 3, sepcolor = 'white', # separate elements from colsep =3
              sepwidth = 0.1,scale="row",ColSideColors=col1,labCol=NA, breaks=seq(-2,2,0.08))
    dev.off()
  }

}

MakeHeatmap(table.name = "/media/x/DATA/faculdade/ugplot2/ugplot/R/temp/dataset_finto.txt",log.transform = F,
            selected.genes = c("K", "ABC", "BRCAX", "ALP1"), plot.name = "results/plot_deseq2wwX.png",
            color1 =  c(rep("blue", 40), rep("red", 40)), color2=c(rep("white", 40), rep("black", 40)),
            method.corr = "spearman", height.plot = 14,
            width.plot = 12, filter.matrix = F, method.dist = "average", thr.pat = 0,
            patient.list = c("TCGA.YZ.A980.01A.11R.A405.07", "TCGA.YZ.A983.01A.11R.A405.07", "TCGA.YZ.A985.01A.11R.A405.07", "TCGA.WC.A88A.01A.11R.A405.07"))

heatmap3(mtcars)


# Example data for heatmap
mat <- matrix(rnorm(25), nrow = 5, ncol = 5)
rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D", "E")

# Heatmap using base R
heatmap(mat)
dev.off()

mat <- matrix(rnorm(25), nrow = 5, ncol = 5)
rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D", "E")

# Display the heatmap
png("heatmap.png")
dev.off()
heatmap(matrix(rnorm(25), nrow = 5, ncol = 5))
