library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(gtable)

test<-data.frame(x=runif(35))
test$y<-2*test$x+rnorm(35,mean=0,sd=0.4)

mod<-lm(test$y~test$x)

p<-ggplot() + geom_point(data=test,aes(x=x,y=y)) + theme_bw() + scale_x_continuous(name='X',limits=c(0,1.6)) + scale_y_continuous(name='Y',limits=c(-1,4.5)) + geom_abline(slope=mod$coefficients[2],intercept=mod$coefficients[1],linetype=1,size=0.75,color='grey40') + theme(plot.title = element_text(size = 10, face = "bold")) + theme(plot.title = element_text(hjust = 0.5))

p1<-p + ggtitle('Original data, no problem points') + theme(plot.margin = unit(c(0,5,0,5), "cm"))


mod2<-mod<-lm(c(test$y,3.06)~c(test$x,1.53))
p2<-p + geom_point(aes(x=1.53,y=3.06),color='#D2443F') + geom_abline(slope=mod2$coefficients[2],intercept=mod2$coefficients[1],linetype=2,size=0.75,color='#D2443F') + ggtitle('Leverage point only')


mod3<-lm(c(test$y,0.73)~c(test$x,1.53))
p3<-p + geom_point(aes(x=1.53,y=0.73),color='#9BA800') + geom_abline(slope=mod3$coefficients[2],intercept=mod3$coefficients[1],linetype=2,size=0.75,color='#9BA800') + ggtitle('Leverage point, influential observation, and outlier')

mod4<-lm(c(test$y,3.32)~c(test$x,0.56))
p4<-p + geom_point(aes(x=0.56,y=3.32),color='#CE5899') + geom_abline(slope=mod4$coefficients[2],intercept=mod4$coefficients[1],linetype=2,size=0.75,color='#CE5899') + ggtitle('Outlier only')

mod5<-lm(c(test$y,4.22)~c(test$x,0.19))
p5<-p + geom_point(aes(x=0.19,y=4.22),color='#FE8162') + geom_abline(slope=mod5$coefficients[2],intercept=mod5$coefficients[1],linetype=2,size=0.75,color='#FE8162') + ggtitle('Outlier and influential observation')

footnote<-'Black points are the original data points and solid lines represent the regression line in\n the original data. Colored points are added problem points and dotted colored lines represent\nthe regression line including the new problem point'
lay<-rbind(c(5,5,5,5),
	       c(1,1,1,1),
	       c(1,1,1,1),
	       c(2,2,2,2),
	       c(2,2,2,2),
	       c(3,3,3,3),
	       c(3,3,3,3),
	       c(4,4,4,4))
pdf(file='/Users/lent/ProblemPointsEx_KB.pdf',height=11,width=8.5)
grid.arrange(p1,arrangeGrob(p2,p3,ncol=2),arrangeGrob(p4,p5,ncol=2),grid.text(footnote,gp = gpar(fontsize = 12)),grid.text('Problem Point Examples',gp = gpar(fontsize = 20)),layout_matrix=lay,heights=unit(c(0.75,rep(1.4,7)),'in'),widths=unit(rep(2,4),'in'))
dev.off()

colorpalette<-matrix(c('#FFF271','#FFD857','#E6BF3E','#E5F972','#C1CE13','#9BA800','#33BEED','#008BBA','#006594','#FE8162','#D2443F','#AC1E19','#CE5899','#A8156D','#770052'),nrow=5,ncol=3,byrow=T)
rownames(colorpalette)<-c('yellow','green','blue','red','purple')
colnames(colorpalette)<-c('light','medium','dark')
colorpalette<-colorpalette[c('red','yellow','blue','green','purple'),]