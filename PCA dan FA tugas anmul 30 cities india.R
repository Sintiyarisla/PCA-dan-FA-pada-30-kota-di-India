#Principal Component Analysis and Factor Analysis 
library(psych)

install.packages("psych", dependencies=TRUE)


#import data
data <- read.csv("C:\\Users\\admin\\Downloads\\ICT_Subdimension_Dataset new.csv")
print(data)



#Pre-processing data
sum(is.na(data))
p <- ncol(data)

# Menghapus kolom "City" dan "Year" atau yang kualitatif
data <- data[, !(colnames(data) %in% c("City", "Year"))]
print(data)

# Menghitung Statistika Deskriptif
summary_stats <- summary(data)
print(summary_stats)

# Visualisasi 
options(repr.plot.width = 16, repr.plot.height = 4)
par(mfrow = c(1, 4), mar = c(4, 4, 2, 1))
num_plots <- 0
for (col in colnames(data)) {
  if (is.numeric(data[[col]])) {
    hist(data[[col]],
         main = paste("Histogram of", col),
         xlab = col,
         col = "lightblue",
         border = "black")
    num_plots <- num_plots + 1
  }
}
par(mfrow = c(1, 1))

#uji kelayakan pca#
#Check KMO
r <- cor(data)
KMO(r)#kmo > 0.6 data cocok untuk pca 


#Bartlett Test
# p - value < 0.05 data ga cocok untuk pca 
bartlett.test(data)


#---------Principal Component Analysis-----------

## pca manual----

scale_data = scale(data) #standarisasi data scaling
r = cov(scale_data)
##Menghitung eigenvalue dan eigenvector
pc <- eigen(r)
pc$values



#proporsi varians untuk menentukan jumlah kompunen utama yang harus dipilih
sumvar <- sum(pc$values)
propvar <- sapply(pc$values, function(x) x/sumvar) * 100

# Buat data frame tanpa %>%
cumvar <- data.frame(value = pc$values, propvar = propvar)
cumvar$cum <- cumsum(propvar)  # Tambahkan kolom kumulatif

# Atur nama kolom dan baris
rownames(cumvar) <- paste0("PC", seq_len(nrow(cumvar)))
print(cumvar)



library(factoextra)


pca_result <- prcomp(scale_data, center = TRUE, scale. = TRUE)
fviz_eig(pca_result,
         addlabels = TRUE,
         barfill = "skyblue",
         barcolor = "darkblue",
         linecolor = "red")

#hasilPCA
pc$vectors
scores <- as.matrix(scale_data) %*% pc$vectors
head(scores) 

#contribution
contrib_v_PC1 <- fviz_contrib(pca_result, choice = "var", axes = 1, top = 10) + ggtitle("PC1")
contrib_v_PC2 <- fviz_contrib(pca_result, choice = "var", axes = 2, top = 10) + ggtitle("PC2")
plot(contrib_v_PC1)
plot(contrib_v_PC2)



# correlation circle
contrib_circle <- fviz_pca_var(pca_result, col.var = "contrib",
                               gradient.cols = c("#00AFBB", "#E7B800"),
                               repel = TRUE) +
  ggtitle("Kontribusi Variabel")
plot(contrib_circle)

#---------Factor Analysis-----------
varcov = cov(scale_data)
pc = eigen(varcov)
pc$values
pc$vectors

sp = sum(pc$values[1:2])

y = scale_data[,1]%*%t(pc$vectors[1,1])
L1 = sqrt(pc$values[1])*pc$vectors[,1]
L2 = sqrt(pc$values[2])*pc$vectors[,2]


L = cbind(L1,L2)



fa <- fa(r = scale_data, 
         covar = TRUE,
         nfactors = 2, 
         rotate = "none") 
#summary(fa) 

loadings <- fa$loadings



library(RColorBrewer)  # Pastikan library ini sudah di-load

# Loadings dari analisis faktor/PCA
loadings <- load  


x_range <- range(loadings[,1]) + c(-0.2, 0.2)  
y_range <- range(loadings[,2]) + c(-0.2, 0.2)


num_colors <- min(nrow(loadings), 12) 
cols <- brewer.pal(num_colors, "Paired")


plot(loadings[,1:2], type="n", xlab="MR1", ylab="MR2", main="Factor Loadings",
     xlim=x_range, ylim=y_range)
points(loadings[,1:2], col=cols, pch=19, cex=1.5)  
legend("topleft", legend=rownames(loadings), col=cols, pch=19, 
       cex=0.9, bty="n", y.intersp=1)  

png("diagram_faktor.png", width=1200, height=800)
fa.diagram(load)
dev.off()



