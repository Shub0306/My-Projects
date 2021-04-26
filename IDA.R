library(pca3d)
library(MASS)
library(ggplot2)

//PCA Procedure

attach(geo)                                        //Import data of IDA-SE dataset and attach it.
pca<-prcomp(geo[,-1], scale.= TRUE)                //Converts dataset into matrix form
gr<-factor(geo[, 1])                               //Assign group from dataset
summary(pca)
pca3d(pca, group = gr, legend = "topright")       //3D output
pca2d(pca, group = gr, legend = "topright")       //2D output
pca3d(pca, group=gr, show.ellipses=TRUE, ellipse.ci=0.75, show.plane=FALSE)
pca2d(pca, group=gr, biplot=TRUE, biplot.vars=3)

//Eigenvalue and Eigenvector

cov(pca$x)
Eigenvalue<-eigen(cov(pca$x))$value         //Prints Eigenvalue
Eigenvector<-eigen(cov(pca$x))$vector       //Prints Eigenvector
Eigenvalues[1:3]
print(round(Eigenvalues/sum(Eigenvalues) * 100, digits = 2))
round(cumsum(Eigenvalues)/sum(Eigenvalues) * 100, digits = 2)
plot<-(pca$rotation[, 1])
plot<-(pca$rotation[, 2])

//LDA Procedure

pca.lda<-transform(data.frame(pca$x), gr = as.factor(geo[, 1]))    //Transform matrix into data.frame
LDA<-lda(gr ~., data = pca.lda)                                    //Calculate LDA
plot(LDA)                                                          //Plot LDA result of dataset
LDA
pca.predict<-predict(pca.lda)                                      //Make predictions of dataset
ldahist(pca.predict[, 1], g = gr)
ldahist(pca.predict[, 2], g = gr)
