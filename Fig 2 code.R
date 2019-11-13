


#Step 1: Download the data, and unzip the zip file. http://dx.doi.org/10.5061/dryad.dr491

dir = "" #Step 2: specify the directory you stored the file
chen<-read.csv(paste0(dir,"data_Chen_et_al_Funct_Ecol.csv"))

tiff(paste0(dir,"Fig2.tiff"),
	width = 10, height = 10, units = "cm", res = 300)
par(mar=c(5,5,2,2))
plot(data=chen, rooting_volume~N..g.N..V., col=c("#F8766D", "#00BFC4")[as.factor(neighbour)], 
	xlim=c(0,4), ylim=c(0,4), 
	xlab="Nutrient concentration (g/L)", 
	ylab="Rooting Volume (L)", 
	pch=c(15,16)[as.factor(neighbour)])
grid (NULL,NULL, lty = 1)
#Volume comparisons
arrows(0.298, 3.36, .59, 3.36, length=0.05, lty=3)
arrows(.59, 3.36, 0.298, 3.36, length=0.05, , lty=3)
arrows(0.454, 2.2, .909, 2.2, length=0.05, , lty=3)
arrows(.909, 2.2,0.454, 2.2,  length=0.05, lty=3)
arrows(0.595, 1.68, 1.19, 1.68, length=0.05, lty=3)
arrows(1.19, 1.68, 0.595, 1.68, length=0.05, lty=3)
arrows(.9, 1.11, 1.78, 1.11, length=0.05, lty=3)
arrows(1.78, 1.11, .9, 1.11, length=0.05, lty=3)
arrows(1.82, .56, 3.57, .56, length=0.05, lty=3)
arrows(3.57, .56, 1.82, .56, length=0.05, lty=3)
#Nutrient comparisons
arrows(1.8, 1.11, 1.8, 0.56, length=0.05, lty=1)
arrows(1.8, 0.56, 1.8, 1.11, length=0.05, lty=1)
arrows(.9, 1.11, .9, 2.2, length=0.05, lty=1)
arrows(.9, 2.2, .9, 1.11,length=0.05, lty=1)
arrows(.59, 1.68, .59, 3.36, length=0.05, lty=1)
arrows(.59, 3.36, .59, 1.68, length=0.05, lty=1)
mtext("With\nneighbours", side=3, adj=0.4, line=-7, col=c("#00BFC4"))
mtext("Alone", side=3, adj=0.1, line=-10, col=c("#F8766D"))

dev.off()

#Log Scale version, 
#Not used. 
par(mar=c(5,5,2,2))
plot(log(chen$N..g.N..V.), log(chen$rooting_volume), 
	xlab="ln(Nutrient concentration, g/L)", ylab="ln(Rooting Volume, L)", pch=16)
#Volume comparisons
arrows(log(0.298), log(3.36), log(.59), log(3.36), length=0.12, col="blue", lwd=2)
arrows(log(.59), log(3.36), log(0.298), log(3.36), length=0.12, col="blue", lwd=2)
arrows(log(0.454), log(2.2), log(.909), log(2.2), length=0.12, col="blue", lwd=2)
arrows(log(.909), log(2.2), log(0.454), log(2.2),  length=0.12, col="blue", lwd=2)
arrows(log(0.595), log(1.68), log(1.19), log(1.68), length=0.12, col="blue", lwd=2)
arrows(log(1.19), log(1.68), log(0.595), log(1.68), length=0.12, col="blue", lwd=2)
arrows(log(.9), log(1.11), log(1.78), log(1.11), length=0.12, col="blue", lwd=2)
arrows(log(1.78), log(1.11), log(.9), log(1.11), length=0.12, col="blue", lwd=2)
arrows(log(1.82), log(.56), log(3.57), log(.56), length=0.12, col="blue", lwd=2)
arrows(log(3.57), log(.56), log(1.82), log(.56), length=0.12, col="blue", lwd=2)
#Nutrient comparisons
arrows(log(1.8), log(1.11), log(1.8), log(0.56), length=0.12, col="red", lty=2, lwd=2)
arrows(log(1.8), log(0.56), log(1.8), log(1.11), length=0.12, col="red", lty=2, lwd=2)
arrows(log(.9), log(1.11), log(.9), log(2.2), length=0.12, col="red", lty=2, lwd=2)
arrows(log(.9), log(2.2), log(.9), log(1.11), length=0.12, col="red", lty=2, lwd=2)
arrows(log(.59), log(1.68), log(.59), log(3.36), length=0.12, col="red", lty=2, lwd=2)
arrows(log(.59), log(3.36), log(.59), log(1.68), length=0.12, col="red", lty=2, lwd=2)

