#To plot the variation of PC's to select the PC's for
pca <- prcomp(t(mat)) #Can be assay(DESeqobj) for summarizedExperiment object or exprs(DESeqobj) for ExpressionSet
sd <- pca$sdev
var <- sd^2
var.percent <- var/sum(var) * 100
barplot(var.percent,...) #Add all the parameters necessary to make plot looks good

#For correlation scatterplot matrix
#Courtesy: rafalab, snippet from HarvardX 
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)  {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(mat, asp=1, col=rgb(0,0,0,.3), lower.panel=panel.cor)

#Multi-dimenstional plotting
#Matrix can be expression of most variable genes 
mds <- cmdscale(t(mat))
plot(mds[,1],mds[,2],...)
