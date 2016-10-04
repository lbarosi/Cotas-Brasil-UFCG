########################
# Gráficos para análise da população da UFCG
# Luciano Barosi
# Pró-Reitoria de Ensino
# 10.Setembro.2016
suppressMessages(library("ggthemes"))
library(scales)

source("analiseCursosBASEv7.r")
#--------------------------------------------------------------------------------------
#Evolução Alunos totais por Centro - PERIODO
# alunos1        <- eqUFCGp()
# matriculados1  <- alunos1 %>% filter(PERIODO %in% periodoREUNI, Status == "Matriculado") %>% 
#   ddply( c("PERIODO","CENTRO"), summarise, Alunos = sum(N))
# #----------------------------------------------------------
# evolucaoMatriculas <-   ggplot(matriculados1, aes(PERIODO, Alunos, fill = CENTRO)) + 
#   geom_bar(stat="identity" )+
#   ggtitle("Evolução da população de alunos UFCG") +
#   labs(x = "", y = "Alunos") +
#   theme_economist_white() + 
#   theme(axis.text.x = element_text(angle = 75))+
#   theme(legend.position = "bottom") +
#   theme(legend.text = element_text(size = 6))+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))+
#   scale_fill_brewer(palette = "Paired")
# #----------------------------------------------------------
# fluxoAlunos  <- alunos1 %>% filter(PERIODO %in% periodoREUNI, Status != "Matriculado") %>% 
#   ddply( c("PERIODO","Status"), summarise, Alunos = sum(N))
# 
# evolucaoMatriculasfluxo <-   ggplot(fluxoAlunos, aes(PERIODO, Alunos, fill = Status)) + 
#   geom_bar(stat="identity" , position = "dodge")+
#   ggtitle("Fluxo de alunos UFCG") +
#   labs(x = "", y = "Alunos") +
#   theme_economist_white() + 
#   theme(axis.text.x = element_text(angle = 75))+
#   theme(legend.position = "bottom") +
#   theme(legend.text = element_text(size = 6))+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))+
#   scale_fill_brewer(palette = "Dark2")
# 
# #-----------------------------------------------------------------------------
# fluxoAlunosCentro  <- alunos1 %>% filter(PERIODO %in% periodoREUNI, Status != "Matriculado") %>% 
#   ddply( c("PERIODO","CENTRO","Status"), summarise, Alunos = sum(N))
# 
# pfluxoCentro <-   ggplot(fluxoAlunosCentro, aes(PERIODO, Alunos, fill = Status)) + 
#   geom_bar(stat="identity" , position = "dodge")+
#   facet_wrap(~CENTRO)+
#   ggtitle("Fluxo de alunos UFCG") +
#   labs(x = "", y = "Alunos") +
#   theme_economist_white() + 
#   theme(axis.text = element_text(size = 5))+
#   theme(axis.text.x = element_text(angle = 75))+
#   theme(legend.position = "bottom") +
#   theme(legend.text = element_text(size = 6))+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))+
#   scale_fill_brewer(palette = "Dark2")
# 
# 
# #-----------------------------------------------------------------------------
# alunos2         <- evasaoLobo()
# alunos2$PERIODO <- as.factor(alunos2$PERIODO)
# evasaoLobop <-   ggplot(alunos2, aes(PERIODO, Ev)) + 
#   geom_bar(stat="identity", fill = "darkorchid4")+
#   ggtitle("Evasão UFCG - Instituto Lobo") +
#   labs(x = "", y = "Evasão") +
#   theme_economist_white() + 
#   theme(axis.text.x = element_text(angle = 75))+
#   theme(legend.position = "bottom") +
#   theme(legend.text = element_text(size = 6))+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))
# #-----------------------------------------------------------------------------  
# alunoseq <- eqUFCGanual()
# alunoseq <- filter(alunoseq, Status == "Equivalente")
# 
# pevolucaoeq <-   ggplot(alunoseq, aes(ANOS, N, fill = CENTRO)) + 
#   geom_bar(stat="identity" )+
#   ggtitle("Evolução de alunos-equivalente UFCG") +
#   labs(x = "", y = "Alunos") +
#   theme_economist_white() + 
#   theme(axis.text.x = element_text(angle = 75))+
#   theme(legend.position = "bottom") +
#   theme(legend.text = element_text(size = 6))+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))+
#   scale_fill_brewer(palette = "Paired")
# 
# #Verificar Função
# #alunos3         <- evasaoOCDE()
# 
# 
# #-----------------------------------------------------------------------------
# #População SEXO
# sexoalunos <- filtraAlunosCENTRO("2016.1","SEXO")
# sexoalunos$SIGLA <- as.factor(sexoalunos$SIGLA)
# psexo <-   ggplot(sexoalunos, aes(SEXO, Alunos, fill = SIGLA)) + 
#   geom_bar(stat="identity")+
#   ggtitle("Composição da população - SEXO") +
#   labs(x = "", y = "Alunos") +
#   theme_economist_white() + 
#   theme(axis.text.x = element_text(angle = 75))+
#   theme(legend.position = "bottom") +
#   theme(legend.text = element_text(size = 6))+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
#   scale_fill_brewer(palette = "Paired")
# #-----------------------------------------------------------------------------
#População Ensino Médio
labelsEM <- c("Particular", "Público")

emalunos <- filtraAlunosCENTRO("2016.1","TIPO_ENSINO_MEDIO")
emalunos <- emalunos %>% filter(TIPO_ENSINO_MEDIO %in% c("1.0","2.0")) %>% droplevels()

emalunos$SIGLA <- as.factor(emalunos$SIGLA)
emalunos$TIPO_ENSINO_MEDIO <- as.factor(emalunos$TIPO_ENSINO_MEDIO)
levels(emalunos$TIPO_ENSINO_MEDIO) <- labelsEM

pensinomedio <-   ggplot(emalunos, aes(TIPO_ENSINO_MEDIO, Alunos, fill = SIGLA)) +
  geom_bar(stat="identity")+
  ggtitle("Composição da população - Tipo de Ensino Médio") +
  labs(x = "", y = "Alunos") +
  theme_economist_white() +
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Paired")
#-----------------------------------------------------------------------------
#População Cotas
labels <- c("Livres", "NAPPI>", "APPI>", "NAPPI<", "APPI<")

cotasalunos <- filtraAlunosCENTRO("2016.1","COTISTA")
cotasalunos$COTISTA <- ifelse(is.na(cotasalunos$COTISTA),"0",cotasalunos$COTISTA)
cotasalunos$SIGLA <- as.factor(cotasalunos$SIGLA)
cotasalunos$COTISTA <- as.factor(cotasalunos$COTISTA)
levels(cotasalunos$COTISTA) <- labels


pcotas <-   ggplot(cotasalunos[which(cotasalunos$COTISTA != "Livres"),], 
                         aes(COTISTA, Alunos, fill = SIGLA)) + 
  geom_bar(stat="identity")+
  ggtitle("Composição da população de cotistas") +
  labs(x = "", y = "Alunos") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Paired")
#----------------------------------------------------------

livressumario <- ddply(cotasalunos[which(cotasalunos$COTISTA == "Livres"),], c("SIGLA"), summarise, Alunos = sum(Alunos))
livressumario$Status <- "Livres"
cotistasumario <- ddply(cotasalunos[which(cotasalunos$COTISTA != "Livres"),], c("SIGLA"), summarise, Alunos = sum(Alunos))
cotistasumario$Status <- "Cotistas"

cotassumario <- rbind(livressumario, cotistasumario)

pcotas1 <-   ggplot(cotassumario, aes(Status, Alunos, fill = SIGLA)) + 
  geom_bar(stat="identity")+
  ggtitle("Composição da população - Cotas") +
  labs(x = "", y = "Alunos") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Paired")

#-----------------------------------------------------------------------------
#Distribuição ENEM
ativos <- alunosUFCGativos("2016.1",SIGLAS)

ativos <- filter(ativos, MEDIA_INGRESSO < 1000, MEDIA_INGRESSO > 350)
pENEM1 <-   ggplot(ativos, aes(x=MEDIA_INGRESSO)) + 
  geom_density(aes(group=SEXO, colour=SEXO, fill = SEXO), alpha = 0.3)+
  ggtitle("Densidade de distribuição das notas ENEM") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_brewer(palette = "Dark2")

#-----------------------------------------------------------------------------

ativos <- filter(ativos, MEDIA_INGRESSO < 1000, MEDIA_INGRESSO > 350)
pENEMP <-   ggplot(ativos, aes(x=ENEMP)) + 
  geom_density(aes(group=SEXO, colour=SEXO, fill = SEXO), alpha = 0.3)+
  ggtitle("Densidade de distribuição das notas ENEM (Normalizado por Curso") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_brewer(palette = "Dark2")
#----------------------------------------------------------
pENEMsexo <-   ggplot(ativos, aes(SEXO, MEDIA_INGRESSO, fill = SEXO)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("ENEM por sexo") +
  labs(x = "", y = "ENEM por sexo") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_brewer(palette = "Dark2")
#----------------------------------------------------------


ativos$COTISTA <- ifelse(is.na(ativos$COTISTA),"0",ativos$COTISTA)
ativos$COTISTA <- as.factor(ativos$COTISTA)
levels(ativos$COTISTA) <- labels

pENEMcotas <-   ggplot(ativos, aes(COTISTA, MEDIA_INGRESSO, fill = COTISTA)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("Notas Enem por Cotas") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_brewer(palette = "Dark2")

#-----------------------------------------------------------------------------
#Distribuição CRA
psexoCRA <-   ggplot(ativos, aes(x=CRA)) + 
  geom_density(aes(group=SEXO, colour=SEXO, fill = SEXO), alpha = 0.3)+
  ggtitle("Densidade de distribuição das notas CRA") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_brewer(palette = "Dark2")
#----------------------------------------------------------

psexoCRAbox <-   ggplot(ativos, aes(SEXO, CRA, fill = SEXO)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("Densidade de distribuição CRA") +
  labs(x = "", y = "ENEM por sexo") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_brewer(palette = "Dark2")
#----------------------------------------------------------

psexoCRAP <-  ggplot(ativos, aes(SEXO, CRAP, fill = SEXO)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("CRA padronizado por curso") +
  labs(x = "", y = "ENEM por sexo") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_brewer(palette = "Dark2")
#----------------------------------------------------------
psexoIEA <- ggplot(ativos, aes(SEXO, IEAP, fill = SEXO)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("IEA por sexo") +
  labs(x = "", y = "ENEM por sexo") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_brewer(palette = "Dark2")
#----------------------------------------------------------

psexoIEAP <- ggplot(ativos, aes(SEXO, IEAP, fill = SEXO)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("IEA padronizado por curso, por sexo") +
  labs(x = "", y = "ENEM por sexo") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_brewer(palette = "Dark2")
#----------------------------------------------------------

pcotistaCRA <-   ggplot(filter(ativos, Semestres>1), aes(COTISTA, CRA, fill = COTISTA)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("CRA por Cotas") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Set2")
#----------------------------------------------------------
pcotistaCRAP <-  ggplot(ativos, aes(COTISTA, CRAP, fill = COTISTA)) + 
     geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
     ggtitle("CRA padronizado por Curso") +
     labs(x = "", y = "Densidade de Probabilidade") +
     theme_economist_white() + 
     theme(axis.text.x = element_text(angle = 75))+
     theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
     guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
     scale_fill_brewer(palette = "Set2")
#----------------------------------------------------------   

pcotistaIEA <- ggplot(ativos, aes(COTISTA, IEA, fill = COTISTA)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("IEA por cotas") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Set2")
#----------------------------------------------------------

pcotistaIEAP <-   ggplot(ativos, aes(COTISTA, IEAP, fill = COTISTA)) + 
     geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
     ggtitle("IEA padronizado por curso") +
     labs(x = "", y = "Densidade de Probabilidade") +
     theme_economist_white() + 
     theme(axis.text.x = element_text(angle = 75))+
     theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
     guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
     scale_fill_brewer(palette = "Set2")

#----------------------------------------------------------

pENEMarea <-ggplot(ativos, aes(ÁREA.DO.CONHECIMENTO, MEDIA_INGRESSO, fill = ÁREA.DO.CONHECIMENTO)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("Notas Enem por ENEM") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() +
  theme(axis.text = element_text(size = 6))+
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Set1")
#----------------------------------------------------------

pENEMParea <- ggplot(ativos, aes(ÁREA.DO.CONHECIMENTO, ENEMP, fill = ÁREA.DO.CONHECIMENTO)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("Notas Enem padronizadas por curso") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() +
  theme(axis.text = element_text(size = 6))+
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Set1")
#----------------------------------------------------------


pCRAarea <- ggplot(ativos, aes(ÁREA.DO.CONHECIMENTO, CRA, fill = ÁREA.DO.CONHECIMENTO)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("CRA por área de conhecimento") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() +
  theme(axis.text = element_text(size = 6))+
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Set1")
#----------------------------------------------------------


   
   
pCRAParea <-   ggplot(ativos, aes(ÁREA.DO.CONHECIMENTO, CRAP, fill = ÁREA.DO.CONHECIMENTO)) + 
     geom_boxplot()+
     stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
     ggtitle("CRA padronizado por curso") +
     labs(x = "", y = "Densidade de Probabilidade") +
     theme_economist_white() +
     theme(axis.text = element_text(size = 6))+
     theme(axis.text.x = element_text(angle = 75))+
     theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
     guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
     scale_fill_brewer(palette = "Set1")
#----------------------------------------------------------   
#----------------------------------------------------------



pIEAarea <-   ggplot(ativos, aes(ÁREA.DO.CONHECIMENTO, IEA, fill = ÁREA.DO.CONHECIMENTO)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("IEA por Área do conhecimento") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() +
  theme(axis.text = element_text(size = 6))+
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Set1")
#----------------------------------------------------------

pIEAParea <-   ggplot(ativos, aes(ÁREA.DO.CONHECIMENTO, IEAP, fill = ÁREA.DO.CONHECIMENTO)) + 
  geom_boxplot()+
  ggtitle("IEA padronizado por curso") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() +
  theme(axis.text = element_text(size = 6))+
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Set1")
#----------------------------------------------------------

pcotistaIEAevol <- ggplot(filter(ativos, Semestres>1 & Semestres < 8), aes(COTISTA, IEA, fill = COTISTA)) + 
  geom_boxplot()+
  facet_wrap(~Semestres)+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("IEA por cotas") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Set2")    

pcotistaCRAevol <- ggplot(filter(ativos, Semestres>1 & Semestres < 8), aes(COTISTA, CRA, fill = COTISTA)) + 
  geom_boxplot()+
  facet_wrap(~Semestres)+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) + 
  ggtitle("IEA por cotas") +
  labs(x = "", y = "Densidade de Probabilidade") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  scale_fill_brewer(palette = "Set2")    

#------------------------
labels <- c("Livres", "NAPPI>", "APPI>", "NAPPI<", "APPI<")
alunosTotal <- read_excel("./DADOS/Alunos/Alunos.xlsx")
alunosTotal$PERIODO_INGRESSO <- as.numeric(alunosTotal$PERIODO_INGRESSO)
alunosTotal$COTISTA <- ifelse(is.na(alunosTotal$COTISTA),"1",cotasalunos$COTISTA)
alunosTotal <- filter(alunosTotal, PERIODO_INGRESSO > 2013)

alunosTotal$COTISTA <- as.factor(alunosTotal$COTISTA)

levels(alunosTotal$COTISTA) <- labels

alunosTotal$Situacao <- ifelse(alunosTotal$COD_EVASAO %in% EVASAO, "evasão",
                               ifelse(alunosTotal$COD_EVASAO %in% DESISTENCIA, "desistência",
                                      ifelse(alunosTotal$COD_EVASAO %in% FALSOFERA, "falsofera",
                                             "Regular")))
alunossumEv <- ddply(alunosTotal, c("COTISTA", "Situacao"), summarize, N = length(CPF))
alunossumEv$Situacao <- as.factor(alunossumEv$Situacao)

alunossumEv <- ddply(alunossumEv, .(COTISTA), 
                   transform, pos = (cumsum(N) - 0.5*N)/sum(N),
                   lab = (N)/sum(N)
)


evadCOTA <- ggplot(alunossumEv, aes(COTISTA, N, fill = Situacao)) + 
  geom_bar(position = "fill", stat = "identity")+
  ggtitle("Tipos de Evasão") +
  labs(x = "", y = "% do total") +
  geom_text_repel(data = alunossumEv, aes(label=percent(lab), y = pos),
                  size = 3)+
  scale_y_continuous(labels = percent_format())+
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  theme(axis.text = element_text(size = 6, hjust = 0))+
  theme(plot.title = element_text(size=12, face = "bold", family="Verdana"))+
  theme(legend.text = element_text(family = "Verdana"))+
  theme(axis.text = element_text(family = "Verdana"))+
  guides(fill=guide_legend(nrow=1))+
  scale_fill_brewer(palette = "Set2")

