library(reshape2)
library("ggthemes")
library("scales")
library(ggrepel)
library(cowplot)
library(extrafont)
library(scales)
library(mosaic)

library(tictoc)

tic()



source("./analiseCursosBASEv7.r")

#-------------------------------
#INDIA - Desigualdade Social
avIncome <- read_excel("./DADOS/avIncome-GR-IND.xlsx", skip = 1, sheet = 2)
avIncome <- avIncome[,-c(14:16)]
indiaIncome <- filter(avIncome, Country =="India")


indiaIncome <- melt(indiaIncome[,c(1:5)], id.vars = c("Country", "Year"))

indiaIncome$variable <- as.factor(indiaIncome$variable)
levels(indiaIncome$variable) <- c("top 1%", "top 0.5%", "top 0,1%")

desigualdadeIndia <-   ggplot(indiaIncome, aes(Year, value/1000, color = variable )) +
  geom_smooth(span=0.1 )+
  ggtitle("INDIA - PIB per capita")+
  labs(x = "", y = "1.000 Rupees") +
  scale_y_continuous(labels = dollar) +
  scale_fill_brewer(palette = "Paired")+
  theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = c(0.5,0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))
  


#------------------------------------
#Desigualdade US

avIncomeUS <- read_excel("./DADOS/avincomeUS.xlsx", skip = 1, sheet = 2)
avIncomeUS <- avIncomeUS[,-c(21:23)]
USIncome <- filter(avIncomeUS, Country =="United States")

USIncome <- melt(USIncome[,c(1:6)], id.vars = c("Country", "Year"))


ggplot(USIncome, aes(Year, value, fill = variable )) +
  geom_smooth(span = 0.1)

desigualdadeUS <-   ggplot(USIncome, aes(Year, value, color = variable )) +
  geom_smooth(span=0.1 )+
  labs(x = "", y = "U$") +
  ggtitle("Estados Unidos - PIB p/capita")+
  scale_y_continuous(labels = dollar) +
  scale_fill_brewer(palette = "Paired")+
theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = c(0.5,0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))




#----------------------------
#Preston

dataPreston <- read_excel("./DADOS/preston.xlsx")
dPreston <- select(dataPreston, -c(4))

dPrestonano <- function(ano){
  pos <- grep(ano, names(dPreston), fixed = TRUE)  
  colunas <- c(1,2,3,pos)
  dPreston <- select(dPreston, colunas)
  dPreston$`Country Name` <- as.factor(dPreston$`Country Name`)
  paises <- levels(dPreston$`Country Name`)
  
  
  dados <- lapply(paises, function(x){ 
    dcast( dPreston[which(dPreston$`Country Name` == x),], 
           `Country Name` + `Country Code` ~ `Series Name`)  })
  
  dados <- do.call(rbind, dados)
  names(dados) <- c("País", "Sigla", "PIB.p.capita","Expectativa.vida","População")
  dados$PIB.p.capita <- as.numeric(dados$PIB.p.capita)
  dados$Expectativa.vida <- as.numeric(dados$Expectativa.vida)
  dados$População <- as.numeric(dados$População)
  return(dados)
  
}

dPreston2005 <- dPrestonano(2005)

dPreston1965 <- dPrestonano(1965)


ddPreston2005 <- filter(dPreston2005, PIB.p.capita > 1000 & PIB.p.capita < 52000 & População < 300000000)

ddPreston1965 <- filter(dPreston1965, PIB.p.capita > 100 & PIB.p.capita < 52000 & População < 194303001)



grafPreston2005 <-   ggplot(ddPreston2005, aes(PIB.p.capita, Expectativa.vida, color = País )) +
  geom_point(aes(size = População, alpha = 0.3))+
  geom_text_repel(data = filter(ddPreston2005, População > 38145491), aes(label=Sigla), color = "black")+
  ggtitle("2005 - Curva de Preston")+
  labs(x = "", y = "Expectativa de Vida ao Nascer") +
  expand_limits(y=c(30,85))+
  scale_x_continuous(labels = dollar) +
  scale_fill_brewer(palette = "Paired")+
  theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = "none",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))


grafPreston2005

#----------------------------------------------------------------------
grafPreston1965 <-   ggplot(ddPreston1965, aes(PIB.p.capita, Expectativa.vida, color = País )) +
  geom_point(aes(size = População, alpha = 0.3))+
  geom_text_repel(data = filter(ddPreston1965, População > 22283380), aes(label=Sigla), color = "black")+
  ggtitle("1965 - Curva de Preston")+
  labs(x = "", y = "Expectativa de Vida ao Nascer") +
  expand_limits(y=c(30,85))+
  scale_x_continuous(labels = dollar) +
  theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = "none",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))



#-----------------------




#----------------------

brasilgdp <- read_excel("./DADOS/brasil-gdp.xlsx")
brasilgdp <- select(brasilgdp, -c(2,4))

brasildata <- melt(brasilgdp, id.vars = c("Country Name", "Series Name"))
names(brasildata) <- c("País", "Serie", "Ano", "Valor")
brasildata$Ano <- substr(brasildata$Ano,1,4)
brasildata$Valor <- as.numeric(brasildata$Valor)
brasildata$Ano <- as.numeric(as.character(brasildata$Ano))
Anos <- seq(first(brasildata$Ano),last(brasildata$Ano),by = 10)
#brasildata$Serie <- as.factor(brasildata$Serie)





brasildataPIB <-   ggplot(filter(brasildata, Serie == "GDP per capita (current US$)"), aes(Ano, Valor)) +
  geom_smooth(span=0.2 )+
  geom_point(color = "steelblue1")+
  ggtitle("BRASIL - PIB per capita")+
  labs(x = "", y = "") +
  scale_y_continuous(labels = dollar) +
  theme_economist_white() + 
  theme(aspect.ratio = 0.56)+
  theme(legend.position = "none") +
  theme(legend.text = element_text(size = 6))+
  theme(axis.text = element_text(size = 6, hjust = 0))+
  theme(plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  theme(legend.text = element_text(family = "Verdana"))+
  theme(axis.text = element_text(family = "Verdana"))+
  scale_fill_brewer(palette = "Paired")+
  scale_x_continuous(breaks = Anos)




brasildataGINI <-   ggplot(filter(brasildata, Serie == "GINI index (World Bank estimate)"), aes(Ano, Valor)) +
    geom_smooth(span=0.2 )+
    geom_point(color = "steelblue1")+
    labs(x = "", y = "") +
    ggtitle("BRASIL - Estimativa de índice GINI (%)")+
  expand_limits(y=c(40,70))+
  theme_economist_white() + 
    theme(aspect.ratio = 0.56)+
    theme(legend.position = "none") +
    theme(legend.text = element_text(size = 6))+
    theme(axis.text = element_text(size = 6, hjust = 0))+
    theme(plot.title = element_text(size=8, face = "bold", family="Verdana"))+
    theme(legend.text = element_text(family = "Verdana"))+
    theme(axis.text = element_text(family = "Verdana"))+
    guides(fill=guide_legend(nrow=2,byrow=TRUE))+
    scale_fill_brewer(palette = "Paired")+
    scale_x_continuous(breaks = Anos)
  

  
  
  
#----------------------------------

brasilP <- read_excel("./DADOS/ibge-populacaqo-1872-2010.xlsx")
brasilP <- select(brasilP, -c(1))
brasilP <- melt(brasilP, id.vars = "OPCAO")
names(brasilP) <- c("Raça", "Ano", "População")

brasilP$População <- as.numeric(brasilP$População)


brasilP <- ddply(brasilP, .(Ano), 
                     transform, pos = (cumsum(População) - 0.5*População)/sum(População, na.rm = TRUE),
                     lab = (População)/sum(População, na.rm = TRUE)
)

brasilP$Raça <- as.factor(brasilP$Raça)


  
  
brasilPP <-   ggplot(brasilP, aes(Ano, População/100, fill = Raça)) +
  geom_bar(stat = "identity")+
  labs(x = "", y = "(%)") +
  scale_y_continuous(labels=percent)+
  ggtitle("Evolução da composição racial da população brasileira")+
  geom_text_repel(data = brasilP, aes(label=percent(lab), y = pos))+
  theme_economist_white() + 
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  theme(axis.text = element_text(size = 6))+
  theme(plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  theme(legend.text = element_text(family = "Verdana"))+
  theme(axis.text = element_text(family = "Verdana"))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_brewer(palette = "Paired")


#---------------------------------

brasilPop <- read_excel("./DADOS/ibge-populacao-2001-2011.xlsx")
brasilPop<- select(brasilPop, -c(1))
brasilPop <- melt(brasilPop, id.vars = "OPCAO")
names(brasilPop) <- c("Raça", "Ano", "População")
brasilPop$População <- as.numeric(brasilPop$População)
brasilPop$Ano <- as.numeric(as.character(brasilPop$Ano))


brasilPopP <-   ggplot(brasilPop, aes(Ano, População, color = Raça)) +
  geom_point()+
  geom_line()+
  labs(x = "", y = "(1000 hab.)") +
  ggtitle("Evolução da composição racial da população brasileira")+
  theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = c(0.8,0.6),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  scale_x_continuous(breaks = seq(first(brasilPop$Ano),last(brasilPop$Ano),by = 2))+
  guides(col = guide_legend(nrow = 3))


################
################
################
################
################
################
################
################
################
#-------------------------------------------
hdi <- read.csv2("./DADOS/human-development-index.csv", dec = ".", sep = ",")
names(hdi) <- c("País", "Ano", "IDH")
Anos <- hdi$Ano

idhSelecao <-   ggplot(hdi, aes(Ano, IDH, color = País)) +
  geom_point()+
  geom_line()+
  geom_text_repel(data = filter(hdi, País =="Brazil"), aes(label=IDH))+
  labs(x = "", y = "IDH") +
  ggtitle("IDH - seleção de países")+
  expand_limits(y=c(0,1))+
  theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = c(0.27,0.2),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(angle = 75, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(breaks = Anos)
 
#----------------------------------
hdiLR <- read.csv2("./DADOS/HDI_byWorldRegion_Since1870_DeLaEscosura.csv", dec = ".", sep = ",")

hdiLR <- melt(hdiLR, id.vars = "year" )


names(hdiLR) <- c("Ano", "Região", "IDH")
Anos <- hdiLR$Ano

idhSelecaoLR <-   ggplot(hdiLR, aes(Ano, IDH, color = Região)) +
  geom_point()+
  geom_line(alpha=0.3)+
  geom_text_repel(data = filter(hdiLR, Região =="Latin.America"), aes(label=IDH))+
  labs(x = "", y = "IDH") +
  ggtitle("IDH - seleção de países")+
  expand_limits(y=c(0,1))+
  theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = c(0.45,0.8),
        legend.direction = "horizontal",
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(angle = 75, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(breaks = Anos)

idhSelecaoLR

#-----------------------------------

brAnosestudo <- read_excel("./DADOS/ibge-anosdeestudo-1992-2009.xlsx")
brAnosestudo <- select(brAnosestudo, -c(1))
brAnosestudo <- melt(brAnosestudo, id.vars = "OPCAO")
names(brAnosestudo) <- c("Cor.Raça", "Ano", "Estudo")
brAnosestudo$Ano <- as.numeric(as.character(brAnosestudo$Ano))
Anos <- brAnosestudo$Ano


brAnosestudog <-ggplot(brAnosestudo, aes(Ano, Estudo, color = Cor.Raça)) +
  geom_point()+
  geom_line(alpha = 0.6)+
  labs(x = "", y = "Anos de Estudo") +
  ggtitle("Brasil - Anos de Estudo (pessoas de 25 anos ou mais)")+
  geom_text_repel(data = brAnosestudo, aes(label=format(Estudo, digits = 2)),
                  box.padding = unit(0.2, 'lines'),
                  point.padding = unit(0.5, 'lines'),
                  segment.size = 0)+
  expand_limits(y=c(0,1))+
  theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = c(0.45,0.2),
        legend.direction = "horizontal",
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(angle = 75, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  scale_fill_brewer(palette = "Paired")+
  scale_x_continuous(breaks = Anos)


#----------------

brquintos <- read_excel("./DADOS/PNAD2016.xlsx")
brquintos$Quinto <- as.factor(brquintos$Quinto)
brquintos$Ano <- as.factor(brquintos$Ano)
brquintos$Publica.anos <- as.numeric(brquintos$Publica.anos)
Anos <- brquintos$Ano

brquintos <- ddply(brquintos, .(Ano), 
              transform, pos = cumsum(Publica.Participacao) - (0.5 * Publica.Participacao)
)

brquintosP <-ggplot(brquintos, aes(Ano, Publica.Participacao, fill = Quinto)) +
  geom_bar(stat = "identity")+
  labs(x = "", y = "%") +
  ggtitle("Brasil - Ensino Superior - Quintos de Renda")+
  geom_text_repel(data = brquintos, aes(label=format(Publica.Participacao, digits = 2), y = pos),
                size = 3)+
  expand_limits(y=c(0,1))+
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  theme(axis.text = element_text(size = 6, hjust = 0))+
  theme(plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  theme(legend.text = element_text(family = "Verdana"))+
  theme(axis.text = element_text(family = "Verdana"))+
  guides(fill=guide_legend(nrow=1))+
  scale_fill_brewer(palette = "Paired")


brquintos$Ano <- as.numeric(as.character(brquintos$Ano))
brquintos$Publica.anos <- as.numeric(brquintos$Publica.anos)
Anos <- brquintos$Ano

brquintosA <-ggplot(brquintos, aes(Ano, Publica.anos, color = Quinto)) +
  geom_point()+
  geom_line(alpha = 0.3)+
  labs(x = "", y = "Anos de Estudo") +
  ggtitle("Brasil - Ensino Superior - Quintos de Renda")+
  geom_text_repel(data = filter(brquintos, Quinto %in% c(1,3,5)), aes(label=format(Publica.anos, digits = 2)),
                  size = 3)+
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  theme(axis.text = element_text(size = 6, hjust = 0))+
  theme(plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  theme(legend.text = element_text(family = "Verdana"))+
  theme(axis.text = element_text(family = "Verdana"))+
  guides(fill=guide_legend(nrow=1))+
  scale_fill_brewer(palette = "Paired")+
  scale_x_continuous(breaks = Anos)


#---------------------------

# #-----------------------------------------------------------------------------
#População Ensino Médio

ativos <- alunosUFCGativos("2016.1",SIGLAS)

ativos <- filter(ativos, MEDIA_INGRESSO < 1000, MEDIA_INGRESSO > 350)


labelsEM <- c("Particular", "Público")

emalunos <- filtraAlunosCENTRO("2016.1","TIPO_ENSINO_MEDIO")
emalunos <- emalunos %>% filter(TIPO_ENSINO_MEDIO %in% c("1.0","2.0")) %>% droplevels()

emalunos$SIGLA <- as.factor(emalunos$SIGLA)
emalunos$TIPO_ENSINO_MEDIO <- as.factor(emalunos$TIPO_ENSINO_MEDIO)
levels(emalunos$TIPO_ENSINO_MEDIO) <- labelsEM

pensinomedio <-   ggplot(emalunos, aes(TIPO_ENSINO_MEDIO, Alunos, fill = TIPO_ENSINO_MEDIO)) +
  geom_bar(stat="identity")+
  ggtitle("Composição da população - Tipo de Ensino Médio") +
  labs(x = "", y = "Alunos") +
  theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = c(0.25,0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))+
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
                   aes(COTISTA, Alunos, fill = COTISTA)) + 
  geom_bar(stat="identity")+
  ggtitle("Composição da população de cotistas") +
  labs(x = "", y = "Alunos") +
  theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = c(0.5,0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  scale_fill_brewer(palette = "Spectral")
#----------------------------------------------------------

livressumario <- ddply(cotasalunos[which(cotasalunos$COTISTA == "Livres"),], c("SIGLA"), summarise, Alunos = sum(Alunos))
livressumario$Status <- "Livres"
cotistasumario <- ddply(cotasalunos[which(cotasalunos$COTISTA != "Livres"),], c("SIGLA"), summarise, Alunos = sum(Alunos))
cotistasumario$Status <- "Cotistas"

cotassumario <- rbind(livressumario, cotistasumario)

pcotas1 <-   ggplot(cotassumario, aes(Status, Alunos, fill = Status)) + 
  geom_bar(stat="identity")+
  ggtitle("Composição da população - Cotas") +
  labs(x = "", y = "Alunos") +
  theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = c(0.25,0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  scale_fill_brewer(palette = "Spectral")

#-----------------------------------------------------------------------------
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
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = c(0.6,0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  scale_fill_brewer(palette = "Paired")

#-----------------------------------------------------------------------------

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
  theme(axis.text = element_text(size = 6, hjust = 0))+
  theme(plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  theme(legend.text = element_text(family = "Verdana"))+
  theme(axis.text = element_text(family = "Verdana"))+
  guides(fill=guide_legend(nrow=1))+
  scale_fill_brewer(palette = "Paired")

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
  theme(axis.text = element_text(size = 6, hjust = 0))+
  theme(plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  theme(legend.text = element_text(family = "Verdana"))+
  theme(axis.text = element_text(family = "Verdana"))+
  guides(fill=guide_legend(nrow=1))+
  scale_fill_brewer(palette = "Paired")

#------------------------
labels <- c("Livres", "NAPPI>", "APPI>", "NAPPI<", "APPI<")

data_typesA  <-c("text","text", "text", "numeric", "text", "text", "text", 
                 "text", "text", "text", "text", "text", "text", "text" )  
alunosTotal <- read_excel("./DADOS/Alunos/Alunos.xlsx", col_types = data_typesA)

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
  theme(plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  theme(legend.text = element_text(family = "Verdana"))+
  theme(axis.text = element_text(family = "Verdana"))+
  guides(fill=guide_legend(nrow=1))+
  scale_fill_brewer(palette = "Set2")

#-------------------------------
#População Escrava

escravos <- read_excel("./DADOS/ibge-populacao-2001-2011.xlsx", sheet = 2)
escravos <- melt(escravos)
names(escravos) <- c("Tipo", "Anos", "População")
escravos <- select(escravos, -c(1))
escravos$Anos <- as.numeric(as.character(escravos$Anos))
escravos$População <- as.numeric(escravos$População)

escravosg <-   ggplot(escravos, aes(Anos, População/1000)) +
  geom_point(color = "steelblue2")+
  geom_line(color = "steelblue4", alpha = 0.3)+
  geom_text_repel(data = escravos, aes(label=format(População/1000, digits = 5)), color = "steelblue2") +
  ggtitle("População Escrava no Brasil - sec. XIX")+
  labs(x = "", y = "1.000 hab") +
  expand_limits(y=0)+
  theme_economist_white() + 
  theme(aspect.ratio = 0.56,
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.text = element_text(family = "Verdana"), 
        legend.position = c(0.5,0.9),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(family = "Verdana"),
        axis.text = element_text(size = 6, hjust = 0),
        plot.title = element_text(size=8, face = "bold", family="Verdana"))+
  scale_x_continuous(breaks = escravos$Anos)
      
#-----------------------------------------
#Estatística
ativos <- filter(ativos, MEDIA_INGRESSO < 1000, MEDIA_INGRESSO > 350)

estatT <- select(ativos, MATRÍCULA, SEXO, COTISTA, MEDIA_INGRESSO, CRA, Semestres, IEA)



aov.outENEM = aov(MEDIA_INGRESSO ~ COTISTA, data = estatT)
testeENEM <- TukeyHSD(aov.outENEM, conf.level = 0.99)

p1 <- mplot(testeENEM, system = "ggplot") + ggtitle("ENEM - 99%CF") + 
  theme_economist_white(base_size = 7)+
  theme(legend.position = "bottom")

estatT2 <- filter(ativos, Semestres == 2)
aov.outCRA = aov(CRA ~ COTISTA, data = estatT2)

testeCRA2 <- TukeyHSD(aov.outCRA, conf.level = 0.99)
p2 <- mplot(testeCRA2, system = "ggplot") + ggtitle("CRA - 2 Período - 99%CF") + 
  theme_economist_white(base_size = 7)+
  theme(legend.position = "bottom",
        axis.text.y = element_blank())

estatT6 <- filter(ativos, Semestres == 6)
aov.outCRA6 = aov(CRA ~ COTISTA, data = estatT6)

testeCRA6 <- TukeyHSD(aov.outCRA6, conf.level = 0.99)
p3 <- mplot(testeCRA6, system = "ggplot") + ggtitle("CRA - 6 Período - 99%CF") + 
  theme_economist_white(base_size = 7)+
  theme(legend.position = "bottom",
        axis.text.y = element_blank())


#-------------------------

estatisticas <- plot_grid(p1,p2,p3, ncol = 3)


toc(quiet = FALSE)

