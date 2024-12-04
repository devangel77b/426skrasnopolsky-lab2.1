library(ggplot2)
raw <- read.csv("krasnopolsky.csv",header=TRUE)

library(dplyr)
data <- mutate(
 raw,
 ameas.ms2 = 2*x.m/t.s^2
 )

apredicted.ms2 <- function(m2,m1,mc){
  9.81*m2/(m2+m1+mc)
}

tpredicted.s <- function(m2,m1,mc,d){
  (2*d/(9.81*m2/(m2+m1+mc)))^0.5
}

fig <- ggplot(data)+geom_point(aes(x=m2.kg,y=ameas.ms2))+
    ylim(0,11)+
    geom_function(fun=apredicted.ms2,args=list(m1=0.0,mc=0.5),color='blue')+
    xlab('$m_2$ (\\unit{\\kilo\\gram})')+
    ylab('$a$ (\\unit{\\meter\\per\\second\\squared})')+
    theme_bw(base_size=8)

library(svglite)
svglite('fig4.svg',width=3,height=2,pointsize=8)
print(fig)
dev.off()

fig3 <- ggplot(data)+geom_point(aes(x=m2.kg,y=t.s))+
    #ylim(0,11)+xlim(0,1.5)+
    ylim(0,0.6)+
    geom_function(fun=tpredicted.s,args=list(d=0.45,m1=0.0,mc=0.5),color='blue')+
    xlab('$m_2$ (\\unit{\\kilo\\gram})')+
    ylab('$t$ (\\unit{\\second})')+
    theme_bw(base_size=8)

svglite('fig3.svg',width=3,height=2,pointsize=8)
print(fig3)
dev.off()

library(xtable)
results <- summarize(
	mean.t = mean(t.s),
	sd.t = sd(t.s),
	mean.a = mean(ameas.ms2),
	sd.a = sd(ameas.ms2),
	group_by(data,m2.kg)
	)
print(xtable(results),include.rownames=FALSE,file='table1raw.tex')

