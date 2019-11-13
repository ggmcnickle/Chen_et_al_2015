
library(rootSolve); library(ggplot2); library(gridExtra)

#Original model and concept from:
#McNickle, G.G. and Brown J.S. (2012) 
#Evolutionary stable strategies for nutrient foraging and below-ground competition in plants. 
#Evolutionary Ecology Research. 14:667–687
#http://www.evolutionary-ecology.com/abstracts/v14/2778.html 

toc = function (u) {
  	with (as.list(params),	{
	u_1 = u[1]; u_2 = u[2]
	r = a*u_1 + a*u_2

	H = R*(1-exp(-r))
	f1 = a*u_1/(r)
	f2 = a*u_2/(r)
	dH = a*R*exp(-a*u_1-a*u_2)
	df1 = (a*r - a*a*u_1)/r^2
	df2 = (a*r - a*a*u_2)/r^2

	dP1 = df1*H + f1*dH - c
	dP2 = df2*H + f2*dH - c

	return(c(dP1 = dP1, dP2 = dP2)) 
	})} 

toc.brc = function (u) {
  	with (as.list(params),	{
	u_1 = u
	r = a*u_1 + a*u_2

	H = R*(1-exp(-r))
	f1 = a*u_1/(r)
	dH = a*R*exp(-a*u_1-a*u_2)
	df1 = (a*r - a*a*u_1)/r^2

	dP1 = df1*H + f1*dH - c

	return(c(dP1 = dP1)) 
	})} 

u_2 = 0
roots1 = numeric()
roots2 = numeric()
precision = numeric()

repeat {
	u_2 = u_2 + 0.01
	params = c(a = 1, c = 2, R = 25, u_2 = u_2)
	solution = multiroot(f = toc.brc, start = (0.1), 
		maxiter=10000, positive = TRUE)
	roots1 = c(roots1, solution$root)
	roots2 = c(roots2, u_2)
	precision = c(precision, solution$estim.precis)

	if (u_2 > 10) break()
	}

params = c(a = 1, c = 2, R = 25)
ess = multiroot(f = toc, start = c(0.1, 0.1), 
	maxiter=10000, positive = TRUE)

params = c(a = 1, c = 2, R = 25, u_2 = 0)
AV = multiroot(f = toc.brc, start = c(0.1, 0.1), 
	maxiter=10000, positive = TRUE)

params = c(a = 1, c = 2, R = 25/2, u_2 = 0)
AhalfV = multiroot(f = toc.brc, start = c(0.1, 0.1), 
	maxiter=10000, positive = TRUE)

par(mfrow=c(1,2))

brc.toc = data.frame(roots1, roots2)
points.toc = data.frame(ess$root, AV$root, AhalfV$root)

plot(roots1, roots2, type="l", 
	xlim=c(0,5), ylim=c(0,5), 
	xlab="Root mass, plant 1", ylab="Root mass, plant 2")
lines(roots2, roots1, type="l", col="red")
points(ess$root[1], ess$root[2])
points(AV$root[1], 0, pch=2)
points(AhalfV$root[1], AhalfV$root[1], pch=4)

######################################################
#Original model and concept from:
#McNickle, G.G. and Brown J.S. (2014) 
#An ideal free distribution explains the root production of plants that do not engage in a tragedy of the commons game.
#Journal of Ecology. 102(4): 963–971.
#http://dx.doi.org/10.1111/1365-2745.12259

max.v = AV$root[1]

ifd = function (u) {
  	with (as.list(params),	{
	u_1 = u[1]; u_2 = u[2]
	r = a*u_1 + a*u_2

	H = R*(1-exp(-r))
	f1 = 1/2
	f2 = 1/2
	dH = a*R*exp(-a*u_1-a*u_2)

	dP1 = f1*dH - c
	dP2 = f2*dH - c

	return(c(dP1 = dP1, dP2 = dP2)) 
	})} 

ifd.brc = function (u) {
  	with (as.list(params),	{
	u_1 = u
	r = a*u_1 + a*u_2

	H = R*(1-exp(-r))
	f1 = a*u_1/(r)
	dH = a*R*exp(-a*u_1-a*u_2)
	df1 = (a*r - a*a*u_1)/r^2

	dP1 = dH - c

	return(c(dP1 = dP1)) 
	})} 


u_2 = 0
roots1 = numeric()
roots2 = numeric()
precision = numeric()

repeat {
	u_2 = u_2 + 0.1
	params = c(a = 1, c = 2, R = 25, u_2 = u_2)
	solution = multiroot(f = ifd.brc, start = (0.1), 
		maxiter=10000, positive = TRUE)
	roots1 = c(roots1, solution$root)
	roots2 = c(roots2, u_2)
	precision = c(precision, solution$estim.precis)

	if (u_2 > max.v-0.1) break()
	}

params = c(a = 2, c = 2, R = 25/2, u_2 = 0)
NV = multiroot(f = ifd.brc, start = c(0.1, 0.1), 
	maxiter=10000, positive = TRUE)

params = c(a = 1, c = 2, R = 25, u_2 = 0)
AV = multiroot(f = ifd.brc, start = c(0.1, 0.1), 
	maxiter=10000, positive = TRUE)

params = c(a = 1, c = 2, R = 25/2, u_2 = 0)
AhalfV = multiroot(f = ifd.brc, start = c(0.1, 0.1), 
	maxiter=10000, positive = TRUE)

brc.ifd = data.frame(roots1, roots2)
points.ifd = data.frame(NV$root, AV$root, AhalfV$root)


plot(roots1, roots2, type="l", 
	xlim=c(0,5), ylim=c(0,5), 
	xlab="Root mass, plant 1", ylab="Root mass, plant 2")
lines(roots2, roots1, type="l", col="red", lty=2)
points(NV$root[1], NV$root[2])
points(AV$root[1], 0, pch=2)
points(nash$root[1], nash$root[2])
points(AhalfV$root[1], AhalfV$root[1], pch=4)


A = ggplot(brc.toc, aes(x=roots2, y=roots1)) +
	geom_point(size=0.1) +
	geom_point(aes(x=roots1, y=roots2), colour="blue", size=0.1) +
	geom_point(data=points.toc, aes(x=AV.root, y=c(0,0)), shape=17, size = 4, colour="#000000")+
	geom_point(data=points.toc, aes(x=ess.root, y=ess.root), shape=19, size = 4, colour="#00BFC4")+
	geom_point(data=points.toc, aes(x=AhalfV.root, y=AhalfV.root), shape=15, size = 4, colour="#F8766D") +
	xlab("Roots produced, plant 1") +
	ylab("Roots produced, plant 2") +
	guides(fill=FALSE) + ggtitle('(a)') +
	xlim(0, 5) + ylim(0,5)+
	theme_bw()	


B = ggplot(brc.ifd, aes(x=roots2, y=roots1)) +
	geom_line(size=1) +
	geom_line(aes(x=roots1, y=roots2), colour="blue", size=1, linetype = "dashed") +
	geom_point(data=points.ifd, aes(x=AV.root, y=c(0,0)), shape=17, size = 4, colour="#000000")+
	geom_point(data=points.ifd, aes(x=NV.root, y=NV.root), shape=19, size = 4, colour="#00BFC4")+
	geom_point(data=points.ifd, aes(x=AhalfV.root, y=AhalfV.root), shape=15, size = 4, colour="#F8766D") +
	xlab("Roots produced, plant 1") +
	ylab("Roots produced, plant 2") +
	guides(fill=FALSE) + ggtitle('(b)') +
	xlim(0,5) + ylim(0,5) +
	theme_bw()	


grid.arrange(A,B, ncol=1)	
