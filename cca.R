#Operate packages
library(CCA)
library(CCP)
library(ggplot2)
#Read the data
setwd('C:\\Users\\Daniel\\Desktop')
getwd()
fish = read.csv('fish.csv',encoding = 'UTF-8')
colnames(fish) <- c('y1','y2','y3','y4','y5')
chemical = read.csv('chemical.csv',encoding = 'UTF-8')
X=as.matrix(chemical[,4:6])
Y=as.matrix(fish)

#do canonical correlation analysis 
cc1 <- cc(chemical, fish)
#Test of significance
rho <- cc1$cor
n <- dim(chemical)[1]
p <- length(chemical)
q <- length(fish)
p.asym(rho, n, p, q, tstat = "Wilks")

#Draw biplot cca(fish chem)
cca.2$CCA$biplot
cca.2$CCA$v
cca.2$CCA$u
sam.2 <- data.frame(cca.2$CCA$u[,c(1,2)], as.character(chemical$year)[-10],as.character(chemical$county)[-10]) 
colnames(sam.2) <- c("CCA1","CCA2","group","county")
spec <- cca.2$CCA$v[,c(1,2)]  
spec <- as.data.frame(spec)
env <- cca.2$CCA$biplot[,c(1,2)] 
env <- as.data.frame(env)
cca1 =round(cca.2$CCA$eig[1]/sum(cca.2$CCA$eig)*100,2) 
cca2 =round(cca.2$CCA$eig[2]/sum(cca.2$CCA$eig)*100,2) 
colnames(sam.2)
p <- ggplot(data=sam.2,aes(CCA1,CCA2)) + geom_point(aes(colour=group),size = 3)
p <- p + labs(title="CCA Plot",x=paste("CCA1 ",cca1," %"),y=paste("CCA2 ",cca2," %"))+ 
  theme(text=element_text(family="serif")) +  
  geom_segment(data = env,aes(x=0,y=0,xend = env[,1], yend = env[,2]),  size=0.5, arrow=arrow(angle=35, length=unit(0.3, "cm"))) +
  geom_text(data=env, aes(x=env[,1], y=env[,2], label=rownames(env)),
            size=5, hjust = (1 - 2 * sign(env[ ,1])) / 3,
            angle = (180/pi) * atan(env[ ,2]/env[ ,1])) + 
  geom_segment(data = spec,aes(x=0,y=0,xend = spec[,1], yend = spec[,2]), colour = "purple",  size=0.5, arrow=arrow(angle=35, length=unit(0.3, "cm"))) +
  geom_text(data=spec, aes(x=spec[,1], y=spec[,2], label=rownames(spec)),
            size=5, colour = "purple", hjust = (1 - 2 * sign(spec[ ,1])) / 3,
            angle = (180/pi) * atan(spec[ ,2]/spec[ ,1]))
p
#zoom biplot
p2 <- p + ylim(-2,2) + xlim(-1.5,2)
p2