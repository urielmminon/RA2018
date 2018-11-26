library(readstata13)
library(R2OpenBUGS)
library(R2jags)

#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

setwd("/home/rstudio/ma/RA2018")
set.seed(1000)

dat <- read.dta13("../Panel 41_45.dta")

#Filtrar las observaciones para obtener de 15 a 65 anios, Que sean personas dentro de la población Económicamente Activa
# que se mantienen en la misma población a lo largo de un año.

p4144NA <- dat[dat$n_ent_1=="1" & dat$d_edad_15_65=="1" & dat$ocupado_1=="1" & dat$ocupado_5=="1",] %>%
  subset(select=c(d_for_1, sb2_1, sb3_1, sb4_1, d_hombre_1, edu_1, eduq_1, d_jefe_1, menor6_1, ocup_med_1, ocup_grande_1, ocup_otros_1, ocup_serv_1, ocup_comer_1, ocup_manu_1, ocup_cons_1, desitae_prommov_vartrim, irs_1,d_for_5))

p4144 <- na.omit(p4144NA)

#Variables Explicativas
p4144x <- p4144 %>% subset(select=c(-d_for_5))

#Observaciones
N<-nrow(p4144x)

#Muestra para pruebas
n <- 100

Samplesn <- sample(1:N, n, replace = FALSE, prob = NULL )

#Variables
m <- dim(p4144x)[2]

#Preparar datos para JAGS / OpenBUGS
data<-list("n"=n,"y"=p4144$d_for_5[Samplesn],"x"=matrix(unlist(p4144x[Samplesn,]), ncol = 18, byrow = TRUE), "m"=m)

inits<-function(){list(beta=rep(0,(m+1)),yf1=rep(1,n))}

parameters<-c("beta","yf1","p")

#JAGS
p4144.sim<-jags(data,inits,parameters,model.file="model-logit.txt",
                n.iter=100,n.chains=1,n.burnin=10,n.thin = 1)

#BUGS
p4144.sim<-bugs(data,inits,parameters,model.file="model-logit.txt",
                n.iter=100,n.chains=1,n.burnin=10,n.thin = 1)
