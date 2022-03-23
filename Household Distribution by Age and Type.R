library(haven)
library(ggplot2)

#Leitura do censo
SP_2080 <- read_sav("C:/Users/gusta/OneDrive/área de Trabalho/Geomeridium/Estado de Sao Paulo/SP_Pessoas_Tipodedomicilio_2080.sav")
SP_1991 <- read_sav("C:/Users/gusta/OneDrive/área de Trabalho/Geomeridium/Estado de Sao Paulo/SP_Pessoas_Tipodedomicilio_1991.sav")
SP_2000 <- read_sav("C:/Users/gusta/OneDrive/área de Trabalho/Geomeridium/Estado de Sao Paulo/SP_Pessoas_Tipodedomicilio_2000.sav")
SP_2010 <- read_sav("C:/Users/gusta/OneDrive/área de Trabalho/Geomeridium/Estado de Sao Paulo/SP_Pessoas_Tipodedomicilio_2010.sav")

#selecionar Variáveis necessárias
SP_2000<-SP_2000[,c("muniat","V0401","V4752","Peso","TIPO")]
SP_2010<-SP_2010[,c("muniat","V0601","V6036","PESO","TIPO")]

RCM_2010<-subset(SP_2010, muniat=""|muniat=""|muniat=""|muniat=""|muniat=""|muniat=""|muniat=""|
                   muniat=""|muniat=""|muniat=""|muniat=""|muniat=""|muniat=""|muniat=""|muniat=""|
                   muniat=""|muniat=""|muniat=""|muniat=""|muniat=""|)

# tabela TIPO de domicílio por idade
t2000<-xtabs(Peso ~ V6036+TIPO, data=SP_2010)
t2010<-xtabs(PESO ~ V6036+TIPO, data=SP_2010)
p2010<-xtabs(PESO ~ V6036, data=SP_2010)


tipos2010<-data.frame(t2010[,1],t2010[,2],t2010[,3],t2010[,4],t2010[,5],t2010[,6],t2010[,7],t2010[,8],t2010[,9])
colnames(tipos2010)<-c(1:9)

pessoas2010<-data.frame(t(p2010))
pessoas2010<-data.frame(pessoas2010[,3])

index<-1:123
plot(index,pessoas2010)
plot(tipos2010[,3], type="l")



install.packages("ggpubr")
library(ggplot2)
require(gridExtra)
library(ggpubr)
setwd("D:\\Gustavo\\Zjg1ZGMzMmRlOThiNGRjZD\\Volume{970c5b77-5f9c-4b5d-af34-3a3b0c6b725e}\\Users\\Gustavo\\Desktop\\Doutorado\\TESE\\ROTINAS\\")
data<-read.table("Data1.txt",header = TRUE, sep='')

unipe<-read.table("UNIPESSOAL.txt",header = TRUE, sep='')
csf<-read.table("CASALSEMFILHOS.txt",header = TRUE, sep='')
ccf<-read.table("CASALCOMFILHOS.txt",header = TRUE, sep='')
mono<-read.table("MONOPARENTAL.txt",header = TRUE, sep='')

unipessoal<-ggplot(unipe, aes(x=IDADE, y=POP, group=ANO, color=ANO)) +
  geom_line(size=1.3)+
  labs(title="Living alone",x="Age", y = "Population (%)")

casal_sem_filhos<-ggplot(csf, aes(x=IDADE, y=POP, group=ANO, color=ANO)) +
  geom_line(size=1.3)+
  labs(title="Couple without children",x="Age", y = "Population (%)")

casal_com_filhos<-ggplot(ccf, aes(x=IDADE, y=POP, group=ANO, color=ANO)) +
  geom_line(size=1.3)+
  labs(title="Couple with children",x="Age", y = "Population (%)")

monop<-ggplot(mono, aes(x=IDADE, y=POP, group=ANO, color=ANO)) +
  geom_line(size=1.3)+
  labs(title="Mother/Father alone",x="Age", y = "Population (%)")

grid.arrange(casal_com_filhos, casal_sem_filhos, unipessoal, monop, ncol=2)
ggarrange(casal_com_filhos, casal_sem_filhos, unipessoal, monop, ncol=2, nrow=2, common.legend = TRUE, legend= "right")
