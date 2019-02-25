#install.packages("jpeg")
library(jpeg)
#install.packages("imager")
library(imager)
#devtools::install_github("thomasp85/patchwork")
library(patchwork)

#On importe l'image via jpeg et imager.
URL1<-"tableau1.jpg"
tableau1jpeg <- readJPEG(URL1)
tableau1im <- load.image(URL1)


#On détermine le nombre de découpes que l'on souhaite faire dans l'image. Ici 10.
nombredecoupes<-10
tranches<-nombredecoupes-1
largeur<-round(dim(tableau1im)[1]/nombredecoupes,0)
longueur<-round(dim(tableau1im)[2]/nombredecoupes,0)

Couleurs<-NULL

#On crée un dataframe avec une ligne pour chaque combinaison de largeur/hauteur
Couleurs <- data.frame(matrix(NA, nrow = tranches*tranches, ncol = 2))
colnames(Couleurs)<-c("Largeur", "Longueur")
Couleurs$Largeur[1]<-largeur
Couleurs$Longueur[1:tranches]<-longueur
for (i in 2:tranches){
  Couleurs$Largeur[i]<-Couleurs$Largeur[i-1]+largeur
}
Couleurs$Largeur[(tranches+1):(tranches*tranches)]<-rep(Couleurs$Largeur[1:tranches],tranches-1)
for (i in (tranches+1):(tranches*tranches)){
  Couleurs$Longueur[i]<-Couleurs$Longueur[i-tranches]+longueur
}

#On extrait pour chaque ligne le niveau de rouge, de vert et de bleu.
for (i in 1:nrow(Couleurs)){
  Couleurs$Rouge[i]<-tableau1im[Couleurs$Largeur[1],Couleurs$Longueur[1],1]
  Couleurs$Vert[i]<-tableau1im[Couleurs$Largeur[i],Couleurs$Longueur[i],2]
  Couleurs$Bleu[i]<-tableau1im[Couleurs$Largeur[i],Couleurs$Longueur[i],3]
} 
#On transforme ces niveaux en un code hexadécimal.
Couleurs$Hexa<-rgb(Couleurs$Rouge,Couleurs$Vert,Couleurs$Bleu)

#On sort l'image via ggplot.
df1 <- as.data.frame(tableau1im,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
IMTableau1 <- ggplot(df1,aes(x,y))+geom_raster(aes(fill=rgb.val))+
  scale_fill_identity()+scale_y_reverse()+
  coord_fixed(ratio=1)+theme_void()

#Si on veut, on sort le jpeg de l'image.
jpeg(filename = "Tableau1normal.jpg", width=dim(tableau1im)[1], height = dim(tableau1im)[2], quality=100, units = "px",type="cairo")
IMTableau1
dev.off()

#On crée le ggplot avec juste les points de couleur.
Points<-Couleurs%>%
  ggplot()+
  geom_point(aes(Largeur,Longueur, colour=Hexa),size=5)+
  scale_color_identity()+
  scale_y_reverse()+
  coord_fixed(ratio=1)+
  #scale_fill_identity()+
  theme_void()

#Si on veut, on le sort en jpeg
jpeg(filename = "Tableau1points.jpg", width=dim(tableau1im)[1], height = dim(tableau1im)[2], quality=100, units = "px",type="cairo")
Points
dev.off()


#On crée le ggplot avec les couleurs de chaque point.
TexteCouleurs<-Couleurs%>%
  ggplot()+
  #geom_point(aes(Longueur,Largeur, colour=Hexa),size=5)+
  geom_label(aes(Largeur,Longueur, label=Hexa, fill=Hexa), colour="white")+
  #scale_color_identity()+
  scale_fill_identity()+
  scale_y_reverse()+
  theme_void()

#Si on veut, on le sort en jpeg.
jpeg(filename = "Tableau1couleur.jpg", width=dim(tableau1im)[1], height = dim(tableau1im)[2], quality=100, units = "px",type="cairo")
TexteCouleurs
dev.off()


#Image plus large que haute : On sort le tout dans un seul jpeg.
jpeg(filename = "Extracts.jpg", width=dim(tableau1im)[1], height = (3*dim(tableau1im)[2]), quality=100, units = "px",type="cairo")
IMTableau1+Points+TexteCouleurs+plot_layout(ncol=1)
dev.off()  

#Image plus haute que large : On sort le tout dans un seul jpeg.
jpeg(filename = "Extracts.jpg", width=(3*dim(tableau1im)[1]), height = dim(tableau1im)[2], quality=100, units = "px",type="cairo")
IMTableau1+Points+TexteCouleurs+plot_layout(ncol=3)
dev.off()  
