graphics.off()
library(ggplot2)
library(lme4)
library(lmerTest)
library(gridExtra)
library(readxl)

#Load data
chen_original<-data.frame(read_excel(
	"U:\\Pudue Files\\Manuscripts\\01 IN REVISION\\000 Comment on CHen\\FIGURES\\data_Chen_et_al_Funct_Ecol.xlsx", 
	na="NA"))

#Chen et al. (2015) added 1g of nutrient when there was one plant (i.e. chen$neighbour = 0), 
#and 2g of nutrient when there were 2 plants, (i.e. chen$neighbour = 1)
#Thus, g/L can be calulated as: 
N.conc = (chen_original$neighbour+1)/chen_original$rooting_volume

#Add N concentration to new dataframe
chen = cbind(chen_original, N.conc) 

#Subset dataframe to only include factorial concentrations. 
chen = chen[which(round(N.conc,2)==0.6 | round(N.conc,2)==0.91 | round(N.conc,2)==1.79),]


###############
panel.a<-ggplot(data = chen, aes(x=as.factor(round(N.conc,2)), y=log(pod_mass+shoot_mass+root_mass), fill=as.factor(neighbour)))  + 
	geom_boxplot()+ 
  	scale_fill_manual(values=c("#F8766D", "#00BFC4"))  +
	labs(x="", y="ln Total mass (g)") +
   	theme_bw() +
  	theme(axis.text.x=element_text(size=11), axis.title.x=element_text(size=14),
       	axis.text.y=element_text(size=11), axis.title.y=element_text(size=14),
	plot.title=element_text(hjust=0)) +
  	guides(fill=FALSE) + ggtitle('(a)') 

panel.b<-ggplot(data = chen, aes(x=as.factor(round(N.conc,2)), y=log(shoot_mass+root_mass), fill=as.factor(neighbour)))  + 
	geom_boxplot() + 
  	scale_fill_manual(values=c("#F8766D", "#00BFC4"))  +
	labs(x="", y="ln Vegetative mass (g)") +
   	theme_bw() +
  	theme(axis.text.x=element_text(size=11), axis.title.x=element_text(size=14),
       	axis.text.y=element_text(size=11), axis.title.y=element_text(size=14),
	plot.title=element_text(hjust=0)) +
  	guides(fill=FALSE) + ggtitle('(b)') 

panel.c<-ggplot(data = chen, aes(x=as.factor(round(N.conc,2)), y=log(root_mass), fill=as.factor(neighbour)))  + 
	geom_boxplot() +   	scale_fill_manual(values=c("#F8766D", "#00BFC4"))  + 
	labs(x="", y="ln Root mass (g)") +
   	theme_bw() +
  	theme(axis.text.x=element_text(size=11), axis.title.x=element_text(size=14),
       	axis.text.y=element_text(size=11), axis.title.y=element_text(size=14),
	plot.title=element_text(hjust=0)) +
  	guides(fill=FALSE) + ggtitle('(c)') 

panel.d<-ggplot(data = chen, aes(x=as.factor(round(N.conc,2)), y=log(shoot_mass), fill=as.factor(neighbour))) + 
	geom_boxplot() +   	scale_fill_manual(values=c("#F8766D", "#00BFC4"))  +
	labs(x="", y="ln Shoot mass (g)") +
   	theme_bw() +
  	theme(axis.text.x=element_text(size=11), axis.title.x=element_text(size=14),
       	axis.text.y=element_text(size=11), axis.title.y=element_text(size=14),
	plot.title=element_text(hjust=0)) +
  	guides(fill=FALSE) + ggtitle('(d)') 
	 
panel.e<-ggplot(data = chen, aes(x=as.factor(round(N.conc,2)), y=log(pod_mass), fill=as.factor(neighbour)))  + 
	geom_boxplot() +   	scale_fill_manual(values=c("#F8766D", "#00BFC4"))  +
	labs(x="", y="ln Pod mass (g)") +
   	theme_bw() +
  	theme(axis.text.x=element_text(size=11), axis.title.x=element_text(size=14),
       	axis.text.y=element_text(size=11), axis.title.y=element_text(size=14),
	plot.title=element_text(hjust=0)) +
  	guides(fill=FALSE) + ggtitle('(e)') 

panel.f<-ggplot(data = chen, aes(x=as.factor(round(N.conc,2)), y=log(root_mass/(pod_mass+shoot_mass+root_mass)), fill=as.factor(neighbour)))  + 
	geom_boxplot() +   	scale_fill_manual(values=c("#F8766D", "#00BFC4"))  +
	labs(x="Nutrient concentration (g/L)", y="ln Root mass fraction") +
   	theme_bw() +
  	theme(axis.text.x=element_text(size=11), axis.title.x=element_text(size=14),
       	axis.text.y=element_text(size=11), axis.title.y=element_text(size=14),
	plot.title=element_text(hjust=0)) +
  	guides(fill=FALSE) + ggtitle('(f)') 

panel.g<-ggplot(data = chen, aes(x=as.factor(round(N.conc,2)), y=log(shoot_mass/(pod_mass+shoot_mass+root_mass)), fill=as.factor(neighbour))) + 
	geom_boxplot() +   	scale_fill_manual(values=c("#F8766D", "#00BFC4"))  +
	labs(x="Nutrient concentration (g/L)", y="ln Shoot mass fraction") +
   	theme_bw() +
  	theme(axis.text.x=element_text(size=11), axis.title.x=element_text(size=14),
       	axis.text.y=element_text(size=11), axis.title.y=element_text(size=14),
	plot.title=element_text(hjust=0)) +
  	guides(fill=FALSE) + ggtitle('(g)') 
	 
panel.h<-ggplot(data = chen, aes(x=as.factor(round(N.conc,2)), y=log(pod_mass/(pod_mass+shoot_mass+root_mass)), fill=as.factor(neighbour)))  + 
	geom_boxplot() +  	scale_fill_manual(values=c("#F8766D", "#00BFC4"))  +
	labs(x="Nutrient concentration (g/L)", y="ln Pod mass fraction") +
   	theme_bw() +
  	theme(axis.text.x=element_text(size=11), axis.title.x=element_text(size=14),
       	axis.text.y=element_text(size=11), axis.title.y=element_text(size=14),
	plot.title=element_text(hjust=0)) +
  	guides(fill=FALSE) + ggtitle('(h)') 

panel.blank<-ggplot(data = chen, aes(x=as.factor(round(N.conc,3)), y=log(pod_mass/(pod_mass+shoot_mass+root_mass)), fill=as.factor(neighbour)))  +
 	geom_boxplot() +
  	#scale_fill_discrete(name="", labels=c("Alone", "With neighbours")) +
	scale_fill_manual(values=c("#F8766D", "#00BFC4"),name="", labels=c("Alone", "With neighbours")) +	ylim(-10000,-1) +  
  	theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank(),
	legend.title = element_text(size=16),
	legend.text = element_text(size = 14),
	legend.position=c(.5, .5)) 
#######

#tiff(paste0(dir,"Fig3.tiff"), 
#	width = 30, height = 30, units = "cm", res = 300)
grid.arrange(panel.a, panel.b, panel.blank, panel.c, panel.d, panel.e, panel.f, panel.g, panel.h, ncol=3)
#dev.off()
#######

total.glmm<-lmer(log(pod_mass+shoot_mass+root_mass)~N..g.N..V.*neighbour + (1 | block), data=chen)
veg.glmm<-lmer(log(shoot_mass+root_mass)~N..g.N..V.*neighbour + (1 | block), data=chen)
root.glmm<-lmer(log(root_mass)~N..g.N..V.*neighbour + (1 | block), data=chen)
shoot.glmm<-lmer(log(shoot_mass)~N..g.N..V.*neighbour + (1 | block), data=chen)
pod.glmm<-lmer(log(pod_mass)~N..g.N..V.*neighbour + (1 | block), data=chen)
froot.glmm<-lmer(log(root_mass/(pod_mass+shoot_mass+root_mass))~N..g.N..V.*neighbour + (1 | block), data=chen)
fshoot.glmm<-lmer(log(shoot_mass/(pod_mass+shoot_mass+root_mass))~N..g.N..V.*neighbour + (1 | block), data=chen)
fpod.glmm<-lmer(log(pod_mass/(pod_mass+shoot_mass+root_mass))~N..g.N..V.*neighbour + (1 | block), data=chen)

if(require(pbkrtest))
anova(total.glmm, ddf = "Kenward-Roger")
anova(veg.glmm, ddf = "Kenward-Roger")
anova(root.glmm, ddf = "Kenward-Roger")
anova(shoot.glmm, ddf = "Kenward-Roger")
anova(pod.glmm, ddf = "Kenward-Roger")
anova(froot.glmm, ddf = "Kenward-Roger")
anova(fshoot.glmm, ddf = "Kenward-Roger")
anova(fpod.glmm, ddf = "Kenward-Roger")

total.glmm<-lmer(log(pod_mass+shoot_mass+root_mass)~log(rooting_volume)*neighbour + (1 | block), data=chen)
anova(total.glmm, ddf = "Kenward-Roger")

total.glmm<-lmer(log(pod_mass+shoot_mass+root_mass)~log(N..g.N..V.)*neighbour + (1 | block), data=chen)
anova(total.glmm, ddf = "Kenward-Roger")




