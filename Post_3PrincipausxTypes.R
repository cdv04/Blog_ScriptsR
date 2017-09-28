

#############################
#  Sim data
#############################
Ag_huile <- rnorm(500,0.52,0.1)
Ag_col <- rbinom(500,1, 0.45)
Ag_esp <- rep("Aglandau",500)

Pi_huile <- rnorm(500,0.35,0.05)
Pi_col <- rbinom(500,1, 0.35)
Pi_esp <- rep("Picholine",500)

huile <- c(Ag_huile, Pi_huile)
coloration <- c(Ag_col, Pi_col)
espece <- c(Ag_esp, Pi_esp)

coloration[coloration==0] <- "faible"
coloration[coloration==1] <- "élevée"
head(coloration)

data_olive <- data.frame(espece, huile, coloration) 


#############################
# stats descri
#############################

library(dplyr)
library(knitr)
library(Rmisc)
options(digits=3) 


##### stat descri quantité d'huile
kable(data_olive %>%
    group_by(espece) %>%
    dplyr::summarise(N=n(),
                   nb_NA = sum(is.na(huile)),
                   moy = mean(huile),
                   mediane = median(huile),
                   min = min(huile),
                   max = max (huile),
                   Q1 = quantile(huile,0.25),
                   Q3 = quantile(huile,0.75),
                   sd = sd(huile),
                   var = var (huile),
                   IQR = Q3-Q1,
                   se= sd/sqrt(N),
                   ci=qt(0.975,N-1)*se,
                   binf = moy - ci,
                   bsup = moy + ci))
                   


# verif avec summarySE
summarySE(data_olive,  measurevar="huile",groupvars="espece") 
#==> OK




##### stat descri coloration




data_olive %>%
  group_by(espece,coloration) %>%
  dplyr::summarise(N=n(),
        p=N/500,
        se = sqrt((p*(1-p))/500),
        ci = qnorm(0.975)*se,
        binf = p - ci,
        bsup = p + ci)             
                                       

# verif des IC avec la fonction binom.test : ok
binom.test(232,500,0.464)$conf
binom.test(268,500,0.536)$conf
binom.test(172,500,0.344)$conf
binom.test(328,500,0.653)$conf


#### A FAIRE
# verifier les stats descri avec sumamarySE
# verifier les formules de se d'une proportion + verifier le calcul de l'IC avec formule sandrine Charles



#############################
# plot
#############################

huile_desc <- summarySE(data_olive,  measurevar="huile",groupvars="espece") 




ggplot(data_olive,aes(x=espece, y=huile))+
  geom_jitter(aes(colour=espece)) +
  facet_grid(~espece, space="free", scales="free_x")+
  stat_summary(fun.y="mean", geom="point", shape=21, size=2, fill="black")+
  geom_errorbar(data=huile_desc,aes(ymin=huile-ci, ymax=huile+ci), colour="black", size=1)+
  theme(legend.position="none")+
  ylab("teneur en huile (g)")+
  xlab("")+
  scale_y_continuous(limits=c(0,0.9))
  

  
  


