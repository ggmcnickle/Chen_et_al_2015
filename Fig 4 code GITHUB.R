library(reshape)
library(ggplot2)
library(gridExtra)
library(readxl)

#Step 1: Download the data, and unzip the zip file. http://dx.doi.org/10.5061/dryad.dr491

dir = "" #Step 2: specify the directory you stored the file
chen=data.frame(read_excel(
	paste0(dir,"data_Chen_et_al_Funct_Ecol.xlsx"), 
	na="NA"))

IDF.means = with(chen, tapply(root_mass, list(neighbour, rooting_volume), mean, na.rm=TRUE))
IDF.sd = with(chen, tapply(root_mass, list(neighbour, rooting_volume), sd, na.rm=TRUE))
IDF.95CI = 2.776*IDF.sd/sqrt(5)

##################
## V = 1.1
##################


#BRC raw

raw = chen[(chen$neighbour==0 & chen$rooting_volume==1.1),]
MEAN = mean(na.omit(raw$root_mass))
STDEV = sd(na.omit(raw$root_mass))
CI = 2.776*STDEV

BRC = data.frame(cbind(c(raw$root_mass, rep(0, length(raw$root_mass))), c(rep(0, length(raw$root_mass)), raw$root_mass)))
BRC = cbind(BRC, rep("Alone, V", dim(BRC)[1]))
colnames(BRC)[3] = "Competition"
BRC1 = BRC

LOWER = data.frame(matrix(c(MEAN-CI, 0, 0, MEAN-CI), ncol=2))
UPPER = data.frame(matrix(c(MEAN+CI, 0, 0, MEAN+CI), ncol=2))
LOWER = cbind(LOWER, rep("Alone, V", 2))
UPPER = cbind(UPPER, rep("Alone, V", 2))
colnames(LOWER)[3] = "Competition"
colnames(UPPER)[3] = "Competition"
LOWER1 = LOWER
UPPER1 = UPPER

#with neigbhours, V
a=2
b=2
Neighb = data.frame(cbind(IDF.means[a,b], IDF.means[a,b]))
#Alone, v/2
c = 1
d = 1
Alone = data.frame(cbind(IDF.means[c,d], IDF.means[c,d]))
L95CI1 = c(IDF.means[a,b] - IDF.95CI[a,b], IDF.means[c,d] - IDF.95CI[c,d])
U95CI1 = c(IDF.means[a,b] + IDF.95CI[a,b], IDF.means[c,d] + IDF.95CI[c,d])

halfV = cbind(rbind(Neighb, Alone), c("Neighb, V", "Alone, V/2"))
colnames(halfV)[3] = "Competition"
halfV1= halfV

v1.1= ggplot(halfV1, aes(X1, X2, col=Competition)) + geom_point(shape=c(19), size=4) + 
	scale_color_manual(values=c("#000000", "#F8766D", "#00BFC4"))  +
	geom_errorbar(aes(ymin=L95CI1, ymax=U95CI1)) +
	geom_errorbarh(aes(xmin=L95CI1, xmax=U95CI1)) +
	geom_point(data=BRC1, aes(X1, X2), shape=17)+ 
	geom_smooth(data=BRC1, aes(X1, X2), method="lm", se=F) +
	geom_point(data=LOWER1, aes(X1, X2), size=0) +
	geom_smooth(data=LOWER1, aes(X1, X2), method="lm", se=F) +
	geom_point(data=UPPER1, aes(X1, X2), size=0) +
	geom_smooth(data=UPPER1, aes(X1, X2), method="lm", se=F) +
	xlim(0, 2) + ylim(0, 2) +
	xlab("Root production, player 1") +
	ylab("Root production, player 2") +  
	annotate("text", x = 1.5, y = 2, label = "V = 1.1 L") +
	ggtitle("(a)") +
   	theme_bw() + theme(legend.position="none")
	
	
	
############################


##################
## V = 2.2
##################


#BRC raw

raw = chen[(chen$neighbour==0 & chen$rooting_volume==3.36),]
MEAN = mean(na.omit(raw$root_mass))
STDEV = sd(na.omit(raw$root_mass))
CI = 2.776*STDEV

BRC = data.frame(cbind(c(raw$root_mass, rep(0, length(raw$root_mass))), c(rep(0, length(raw$root_mass)), raw$root_mass)))
BRC = cbind(BRC, rep("Alone, V", dim(BRC)[1]))
colnames(BRC)[3] = "Competition"
BRC2 = BRC

LOWER = data.frame(matrix(c(MEAN-CI, 0, 0, MEAN-CI), ncol=2))
UPPER = data.frame(matrix(c(MEAN+CI, 0, 0, MEAN+CI), ncol=2))
LOWER = cbind(LOWER, rep("Alone, V", 2))
UPPER = cbind(UPPER, rep("Alone, V", 2))
colnames(LOWER)[3] = "Competition"
colnames(UPPER)[3] = "Competition"
LOWER2 = LOWER
UPPER2 = UPPER


#with neigbhours, V
a=2
b=5
Neighb = data.frame(cbind(IDF.means[a,b], IDF.means[a,b]))
#Alone, v/2
c = 1
d = 2
Alone = data.frame(cbind(IDF.means[c,d], IDF.means[c,d]))
L95CI2 = c(IDF.means[a,b] - IDF.95CI[a,b], IDF.means[c,d] - IDF.95CI[c,d])
U95CI2 = c(IDF.means[a,b] + IDF.95CI[a,b], IDF.means[c,d] + IDF.95CI[c,d])

halfV = cbind(rbind(Neighb, Alone), c("Sharers, V", "Owners, V/2"))
colnames(halfV)[3] = "Competition"
halfV2= halfV

v2.2= ggplot(halfV2, aes(X1, X2, col=Competition)) + geom_point(shape=c(19), size=4) + 
	geom_errorbar(aes(ymin=L95CI2, ymax=U95CI2)) +
	geom_errorbarh(aes(xmin=L95CI2, xmax=U95CI2)) +
	geom_point(data=BRC2, aes(X1, X2), shape=17)+ 
	geom_smooth(data=BRC2, aes(X1, X2), method="lm", se=F) +
	geom_point(data=LOWER2, aes(X1, X2), size=0) +
	geom_smooth(data=LOWER2, aes(X1, X2), method="lm", se=F) +
	geom_point(data=UPPER2, aes(X1, X2), size=0) +
	geom_smooth(data=UPPER2, aes(X1, X2), method="lm", se=F) +
	xlim(0, 2) + ylim(0, 2) +
	xlab("Root production, player 1") +
	ylab("Root production, player 2") +  
	annotate("text", x = 1.5, y = 2, label = "V = 2.2 L") +
	ggtitle("(b)")+
   	theme_bw() +
	scale_color_manual(values=c("#000000", "#F8766D", "#00BFC4")) + 
	theme(legend.position=c(.75,.75), legend.title=element_blank())
	
	
############################



##################
## V = 3.36
##################


#BRC raw

raw = chen[(chen$neighbour==0 & chen$rooting_volume==3.36),]
MEAN = mean(na.omit(raw$root_mass))
STDEV = sd(na.omit(raw$root_mass))
CI = 2.776*STDEV

BRC = data.frame(cbind(c(raw$root_mass, rep(0, length(raw$root_mass))), c(rep(0, length(raw$root_mass)), raw$root_mass)))
BRC = cbind(BRC, rep("Alone, V", dim(BRC)[1]))
colnames(BRC)[3] = "Competition"
BRC3 = BRC

LOWER = data.frame(matrix(c(MEAN-CI, 0, 0, MEAN-CI), ncol=2))
UPPER = data.frame(matrix(c(MEAN+CI, 0, 0, MEAN+CI), ncol=2))
LOWER = cbind(LOWER, rep("Alone, V", 2))
UPPER = cbind(UPPER, rep("Alone, V", 2))
colnames(LOWER)[3] = "Competition"
colnames(UPPER)[3] = "Competition"
LOWER3 = LOWER
UPPER3 = UPPER


#with neigbhours, V
a=2
b=6
Neighb = data.frame(cbind(IDF.means[a,b], IDF.means[a,b]))
#Alone, v/2
c = 1
d = 4
Alone = data.frame(cbind(IDF.means[c,d], IDF.means[c,d]))
L95CI3 = c(IDF.means[a,b] - IDF.95CI[a,b], IDF.means[c,d] - IDF.95CI[c,d])
U95CI3 = c(IDF.means[a,b] + IDF.95CI[a,b], IDF.means[c,d] + IDF.95CI[c,d])

halfV = cbind(rbind(Neighb, Alone), c("Neighb, V", "Alone, V/2"))
colnames(halfV)[3] = "Competition"
halfV3= halfV

v3.36= ggplot(halfV3, aes(X1, X2, col=Competition)) + geom_point(shape=c(19), size=4) + 
	scale_color_manual(values=c("#000000","#F8766D",  "#00BFC4")) +
	geom_errorbar(aes(ymin=L95CI3, ymax=U95CI3)) +
	geom_errorbarh(aes(xmin=L95CI3, xmax=U95CI3)) +
	geom_point(data=BRC3, aes(X1, X2), shape=17)+ 
	geom_smooth(data=BRC3, aes(X1, X2), method="lm", se=F) +
	geom_point(data=LOWER3, aes(X1, X2), size=0) +
	geom_smooth(data=LOWER3, aes(X1, X2), method="lm", se=F) +
	geom_point(data=UPPER3, aes(X1, X2), size=0) +
	geom_smooth(data=UPPER3, aes(X1, X2), method="lm", se=F) +
	xlim(0, 2) + ylim(0, 2) +
	xlab("Root production, player 1") +
	ylab("Root production, player 2") +  
	annotate("text", x = 1.5, y = 2, label = "V = 3.36 L") +
	ggtitle("(c)")+
   	theme_bw() + theme(legend.position="none")
	
	
############################

tiff(paste0(dir,"Fig2.tiff"),
	width = 10, height = 30, units = "cm", res = 600)

grid.arrange(v1.1, v2.2, v3.36, ncol=1)

dev.off()

