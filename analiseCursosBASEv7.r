#Relatorio Padronizado para a Analise dos Cursos UFCG
#####10 de Setembro : Precisa de revisão. Cuidado com as outras versões.

#################
#BIBLIOTECAS
suppressMessages(library("ggplot2"))
suppressMessages(library("readxl"))
suppressMessages(library("dplyr"))
suppressMessages(library("plyr"))
suppressMessages(library("magrittr"))
#----------------------------------------------------------------------------------
#Definições Globais
#----------------------------------------------------------------------------------
#Definindo codigos de Ingresso
INGRESSO    <-  c(1,11,16,17,18)
FDERIVADAS  <-  c(2,3,4,5,6,9,10,12,13)
#----------------------------------------------------------------------------------
#Definindo Codigos de Evasao
CONCLUINTES <- c(1, 10, 19, 20)
EVASAO      <- c(3, 5, 7, 9, 11, 13, 21, 22, 23, 25)
DESISTENCIA <- c(2, 4, 8, 15, 16, 17, 18)
INGRESSANTES <- c(15, 16, 17, 18, 25)
FALSOFERA   <- c(12)
NOVOCURRICULO <- c(24)
MOBUFCG <- c(6,14)
EVADIDOS <- append(EVASAO, append(DESISTENCIA,append(INGRESSANTES, append(FALSOFERA, append(NOVOCURRICULO,MOBUFCG)))))
#----------------------------------------------------------------------------------
periodoUFCG <- c("2002.1","2002.2","2003.1","2003.2","2004.1","2004.2","2005.1","2005.2","2006.1","2006.2","2007.1","2007.2","2008.1", "2008.2","2009.1","2009.2","2010.1","2010.2","2011.1","2011.2","2012.1","2012.2","2013.1","2013.2", "2014.1", "2014.2", "2015.1", "2015.2", "2016.1")
periodoREUNI <- c("2006.1","2006.2","2007.1","2007.2","2008.1", "2008.2","2009.1","2009.2","2010.1","2010.2","2011.1","2011.2","2012.1","2012.2","2013.1","2013.2", "2014.1", "2014.2", "2015.1", "2016.1")
SIGLAS <- c("CCT", "CCBS", "CH", "CEEI","CTRN", "CFP", "CCJS", "CSTR", "CES", "CDSA", "CCTA")
#----------------------------------------------------------------------------------
cursos <- read_excel("DADOS/BASE/CURSOS.xlsx", sheet = 1)
cursos <- cursos[which(cursos$`Situação EMEC`=="Ativo"),]
cursos$CURSO <- as.factor(cursos$CURSO)
cursos$SIGLA <- as.factor(cursos$SIGLA)
cursos$`ÁREA DO CONHECIMENTO` <- as.factor(cursos$`ÁREA DO CONHECIMENTO`)
#----------------------------------------------------------------------------------
# Funções Principais
#----------------------------------------------------------------------------------
desempenhoAlunos <- function(PERIODO){
  #---------------- 
  ##Le dados de desempenho de alunos 
  ## Apenas para variavel PERIODO
  #Cód. Curso|Curso|Aluno|CH integralizada|Faixa CH %|CRA|Faixa CRA|Semestres|Anos|MC|IEA
  
  #parse args
  if (grepl("\\d{4}.\\d",PERIODO, fixed = FALSE)) {
  
  
      PERIODO <- sub(".","",PERIODO,fixed = TRUE)
      data_types  <-c("text","text","text","numeric","text","numeric","text",
                      "numeric", "numeric", "numeric", "numeric")
      files       <- list.files("./DADOS/Desempenho","ChCRAAnos")
  
      files       <- lapply(files[grepl(PERIODO,files)], function(x) {
                        paste("./DADOS/Desempenho/",x, sep = "" )
                                                                    }
                            ) 
  
      getfiles    <- lapply(files[grep(PERIODO,files, fixed = TRUE)], function(x) { 
                        suppressWarnings( read_excel(x, skip = 1))
                                                }
                        )
  
      desempenho  <- do.call(rbind, getfiles)
      desempenho[is.na(desempenho)] <- 0
      return(desempenho)
      
  } else {
    cat("Formato de argumento período invalido: 'dddd.d'\n")
  }
}
#----------------------------------------------------------------------------------
alunosUFCG <- function(PERIODOS, ES) {
  
  tipos <- c("INGRESSO", "EVASÃO") 
  #Coleciona base de dados de alunos conforme período, insere informações de Cursos e reordena
  #colunas, estabelece fatores.
  #Apenas alunos de cursos ativos para manter comparabilidade
  
  #argumento deve ser um vetor de periodos
  if (!("FALSE" %in% grepl("\\d{4}.\\d",PERIODOS, fixed = FALSE))) {
    if (ES %in% tipos ) {
      
      alunos <- read_excel("DADOS/Alunos/Alunos.xlsx", sheet = 1)
      alunos <- merge(alunos, cursos, by = "CURSO", all = FALSE)
      #Uso do ddplyr: cuidado com as dependências com outras bibliotecas
      alunos <- select(alunos, CPF,	MATRÍCULA, SEXO, NASCIMENTO, NATURALIDADE,	ESTADO,	
                       TIPO_ENSINO_MEDIO,	COTISTA,	PERIODO_INGRESSO,	COD_INGRESSO,	PERIODO_EVASAO,
                       COD_EVASAO,	CURSO,	`Cod. EMEC`,	`ÁREA DO CONHECIMENTO`, CAMPUS,	SIGLA,	Curso,	turno,	CH,
                       `Tempo Mínimo`,	`VAGAS 1`,	`VAGAS 2`,	VAGAS,	PG,	FATOR,	DURACAO,	BFS,	BT,	NOVO,
                       MEDIA_INGRESSO)
      #Filtra  Periodo
      if (ES == "INGRESSO") {
        alunos <- filter(alunos, PERIODO_INGRESSO %in% PERIODOS)  
      }
      if (ES == "EVASÃO") {
        alunos <- filter(alunos, PERIODO_EVASAO %in% PERIODOS | is.na(PERIODO_EVASAO))  
      }
      #Fatora Variáveis
      alunos$SEXO <- as.factor(alunos$SEXO)
      alunos$NATURALIDADE <- as.factor(alunos$NATURALIDADE)
      alunos$ESTADO <- as.factor(alunos$ESTADO)
      alunos$TIPO_ENSINO_MEDIO <- as.factor(alunos$TIPO_ENSINO_MEDIO)
      alunos$COTISTA <- as.factor(alunos$COTISTA)
      alunos$COD_INGRESSO <- as.factor(alunos$COD_INGRESSO)
      alunos$COD_EVASAO <- as.factor(alunos$COD_EVASAO)
      alunos$CURSO <- as.factor(alunos$CURSO)
      alunos$CAMPUS <- as.factor(alunos$CAMPUS)
      alunos$SIGLA <- as.factor(alunos$SIGLA)
      alunos$turno <- as.factor(alunos$turno)
      #-------------------------------------
      #Fatores com nomes compreensíveis
      ##levels(alunos$TIPO_ENSINO_MEDIO) <- c("1","2","3","4","5")
      ##COTISTA <- c("2","3","4","5")
      ##COD_INGRESSO (19 indices)
      ##COD_EVASAO 0,1,3,3,4,5,6,7,8,9,10,12,13,14,19,21,22,24
      #alunos$TIPO_ENSINO_MEDIO <- ifelse(is.na(alunos$TIPO_ENSINO_MEDIO),0,alunos$TIPO_ENSINO_MEDIO)
      #alunos$COTISTA <- ifelse(is.na(alunos$COTISTA),1,alunos$COTISTA)
      
      
      return(alunos)
      
    } else {
      cat("Formato de argumento inválido: insira um INGRESSO/EVASÃO\n")
      cat("inseriu:")
      cat(ES)
    }
    
  } else {
    cat("Formato de argumento períodos inválido: insira um vetor de elementos na forma dddd.d\n")
    cat(PERIODOS)
    cat("\n")
    cat(grepl("\\d{4}.\\d",PERIODOS, fixed = FALSE))
  }
}
#----------------------------------------------------------------------------------
alunosUFCGativos <- function(PERIODO, CENTRO){

  #parse args
  if  (grepl("\\d{4}.\\d",PERIODO, fixed = FALSE)) {
    if (setequal(intersect(CENTRO, SIGLAS),CENTRO)){
      
      alunosativos  <- alunosUFCG(c(PERIODO), "EVASÃO")
      alunosativos  <- filter(alunosativos, is.na(PERIODO_EVASAO) )
      rendimento    <- desempenhoAlunos(PERIODO)
      rendimento    <- select(rendimento, -`Cód. Curso`,-Curso )
      alunos        <- merge(alunosativos, rendimento, by.x = "MATRÍCULA", by.y = "Aluno", all = FALSE)
      alunos        <- filter(alunos, SIGLA %in% CENTRO )
      alunos$MEDIA_INGRESSO <- as.numeric(alunos$MEDIA_INGRESSO)
      alunos       <- ddply(alunos, c("CURSO"), transform, 
                            ENEMP = 500 + 100*(MEDIA_INGRESSO- mean(MEDIA_INGRESSO))/sd(MEDIA_INGRESSO),
                            CRAP  = 500 + 100*(CRA- mean(CRA))/sd(CRA),
                            IEAP  = 500 + 100*(IEA- mean(IEA))/sd(IEA)) 
      
      
      
      return(alunos)
      
    }else {
      cat("Voce deve informar uma lista de SIGLAS de centros\n")
    } 
  } else {
    cat("Formato de argumento período invalido. Informe LISTA 'dddd.d'\n")
  }
 
}
#----------------------------------------------------------------------------------
filtraAlunosCENTRO <- function(PERIODO, FILTRO) {
  
  filtros <- c("SEXO", "TIPO_ENSINO_MEDIO", "COTISTA")
  if (FILTRO %in% filtros) {
    dados <- alunosUFCGativos(PERIODO, SIGLAS)
    dados <- ddply(dados, c("SIGLA",FILTRO), summarise, Alunos = length(MATRÍCULA))
    
    return(dados)
    
  } else {
    cat("Filtro:\n")
    cat(filtros)
  }
  
}
#----------------------------------------------------------------------------------
strataCursoanual <- function(curso){
  
  dados <- alunosUFCG(periodoUFCG,"EVASÃO")
  dados <- filter(dados, CURSO == curso)
  nomecur <- as.character(head(as.factor(dados$Curso), n=1))
  dados$PERIODO_EVASAO <- as.numeric(substr(dados$PERIODO_EVASAO,1,4))
  dados$PERIODO_INGRESSO <- as.numeric(substr(dados$PERIODO_INGRESSO,1,4))
  
  dados$COD_EVASAO = as.numeric(as.character(dados$COD_EVASAO))
  periodoUFCGa <- substr(periodoUFCG,1,4)

  
  
   totIng <- ddply(dados[which(dados$COD_INGRESSO %in% INGRESSO),], c("PERIODO_INGRESSO"), summarise, Status = "Ingressantes", N = length(unique(MATRÍCULA))) 
   totDer <- ddply(dados[which(dados$COD_INGRESSO %in% FDERIVADAS),], c("PERIODO_INGRESSO"), summarise, Status = "Derivadas", N = length(unique(MATRÍCULA))) 
   totCon <- ddply(dados[which(dados$COD_EVASAO %in% CONCLUINTES),], c("PERIODO_EVASAO"), summarise, Status = "Concluintes", N = -length(unique(MATRÍCULA)))
   totEvad <- ddply(dados[which(dados$COD_EVASAO %in% EVASAO),],c("PERIODO_EVASAO"), summarise, Status = "Evadidos", N = -length(unique(MATRÍCULA))) 
   totFalso <- ddply(dados[which(dados$COD_EVASAO %in% FALSOFERA),],c("PERIODO_EVASAO"), summarise, Status = "Falso Fera", N = length(unique(MATRÍCULA))) 
   totDesis <- ddply(dados[which(dados$COD_EVASAO %in% DESISTENCIA),],c("PERIODO_EVASAO"), summarise, Status = "Desistência", N = -length(unique(MATRÍCULA))) 
   totMob <- ddply(dados[which(dados$COD_EVASAO %in% MOBUFCG),],c("PERIODO_EVASAO"), summarise, Status = "Mobilidade", N = -length(unique(MATRÍCULA))) 
   totCurr <- ddply(dados[which(dados$COD_EVASAO %in% NOVOCURRICULO),],c("PERIODO_EVASAO"), summarise, Status = "Novo Currículo", N = length(unique(MATRÍCULA))) 
 
   
  
  names(totCon)   <- c("ANOS", "Status", "N")
  names(totIng)   <- c("ANOS", "Status", "N")
  names(totDer)   <- c("ANOS", "Status", "N")
  names(totEvad)  <- c("ANOS", "Status", "N")
  names(totFalso) <- c("ANOS", "Status", "N")
  names(totDesis) <- c("ANOS", "Status", "N")
  names(totMob)   <- c("ANOS", "Status", "N")
  names(totCurr)  <- c("ANOS", "Status", "N")
  
  frames <- rbind(totIng, totDer, totCon, totEvad, totFalso, totDesis, totMob, totCurr)
  mat <- as.data.frame(unique(periodoUFCGa))
  mat$Status <- "Matriculado" 
  mat$N <- 0
  
  mat$N <- sapply(as.numeric(as.character(unique(periodoUFCGa))), function(x) { 
    length(unique(dados[which((dados$PERIODO_INGRESSO <= x) &
                                (dados$PERIODO_EVASAO >= x | 
                                   is.na(dados$PERIODO_EVASAO))),]$MATRÍCULA)) })
  
  names(mat) <- c("ANOS", "Status", "N")
  frames     <- rbind(frames, mat)
  frames$ANOS <- as.numeric(frames$ANOS)
  
  
  info <- select(slice(dados,1), CH:NOVO)
  cols <- c(1:length(info))
  
  
  info$PG <- as.numeric(info$PG)
  info$BT <- as.numeric(info$BT)
  info$FATOR <- as.numeric(info$FATOR)
  info$`Tempo Mínimo` <- as.numeric(info$`Tempo Mínimo`)
  info$BFS <- as.numeric(info$BFS)
  
  
  mats <- function(A){ ifelse(is.numeric(select(filter(frames, ANOS == A & Status == "Matriculado"), N)[[1]]),
                              select(filter(frames, ANOS == A & Status == "Matriculado"), N)[[1]],
                              0)}
  
  conc <- function(A){ ifelse(is.numeric(select(filter(frames, ANOS == A & Status == "Concluintes"), N)[[1]]),
                              select(filter(frames, ANOS == A & Status == "Concluintes"), N)[[1]],
                              0)}
  
  ings <- function(A){ ifelse(is.numeric(select(filter(frames, ANOS == A & Status == "Ingressantes"), N)[[1]]),
                              select(filter(frames, ANOS == A & Status == "Ingressantes"), N)[[1]],
                              0)}
  
  eqs <- as.data.frame(unique(periodoUFCGa))
  eqs$Status <- "Equivalente" 
  
  eqs$Eq <- sapply(as.numeric(as.character(unique(periodoUFCGa))), 
                   function(x) {
                     ifelse(info$NOVO==1,
                            mats(x) * info$PG * info$BFS * info$BT,
                            ifelse(ings(x) >= -conc(x),
                                   ((-conc(x) * info$`Tempo Mínimo` * (1+info$FATOR)) +
                                      (ings(x) + conc(x)) * info$`Tempo Mínimo`/4) *
                                     info$PG*info$BFS*info$BT,
                                   ((-conc(x)*info$`Tempo Mínimo`*(1+info$FATOR))) *
                                     info$PG*info$BFS*info$BT
                                   )
                            )
                   }
                   )
  
  
  names(eqs) <- c("ANOS", "Status", "N")
  frames <- rbind(frames, eqs)
  frames$Curso <- nomecur
  return(frames)
}
#----------------------------------------------------------------------------------
strataCursoperiodo <- function(curso){
  
  dados <- alunosUFCG(periodoUFCG,"EVASÃO")
  dados <- filter(dados, CURSO == curso) 
  nomecur <- as.character(head(as.factor(dados$Curso), n=1))
  dados$PERIODO_EVASAO <- as.numeric(sub(".","",dados$PERIODO_EVASAO, fixed = TRUE))
  dados$PERIODO_INGRESSO <- as.numeric(sub(".","",dados$PERIODO_INGRESSO, fixed = TRUE))
  
  dados$COD_EVASAO = as.numeric(as.character(dados$COD_EVASAO))
  periodoUFCGa <- unique(sub(".","",periodoUFCG, fixed = TRUE))
  
  
  
  totIng <- ddply(dados[which(dados$COD_INGRESSO %in% INGRESSO),], c("PERIODO_INGRESSO"), summarise, Status = "Ingressantes", N = length(unique(MATRÍCULA))) 
  totDer <- ddply(dados[which(dados$COD_INGRESSO %in% FDERIVADAS),], c("PERIODO_INGRESSO"), summarise, Status = "Derivadas", N = length(unique(MATRÍCULA))) 
  totCon <- ddply(dados[which(dados$COD_EVASAO %in% CONCLUINTES),], c("PERIODO_EVASAO"), summarise, Status = "Concluintes", N = -length(unique(MATRÍCULA)))
  totEvad <- ddply(dados[which(dados$COD_EVASAO %in% EVASAO),],c("PERIODO_EVASAO"), summarise, Status = "Evadidos", N = -length(unique(MATRÍCULA))) 
  totFalso <- ddply(dados[which(dados$COD_EVASAO %in% FALSOFERA),],c("PERIODO_EVASAO"), summarise, Status = "Falso Fera", N = length(unique(MATRÍCULA))) 
  totDesis <- ddply(dados[which(dados$COD_EVASAO %in% DESISTENCIA),],c("PERIODO_EVASAO"), summarise, Status = "Desistência", N = -length(unique(MATRÍCULA))) 
  totMob <- ddply(dados[which(dados$COD_EVASAO %in% MOBUFCG),],c("PERIODO_EVASAO"), summarise, Status = "Mobilidade", N = -length(unique(MATRÍCULA))) 
  totCurr <- ddply(dados[which(dados$COD_EVASAO %in% NOVOCURRICULO),],c("PERIODO_EVASAO"), summarise, Status = "Novo Currículo", N = length(unique(MATRÍCULA))) 
  
  
  
  names(totCon)   <- c("PERIODO", "Status", "N")
  names(totIng)   <- c("PERIODO", "Status", "N")
  names(totDer)   <- c("PERIODO", "Status", "N")
  names(totEvad)  <- c("PERIODO", "Status", "N")
  names(totFalso) <- c("PERIODO", "Status", "N")
  names(totDesis) <- c("PERIODO", "Status", "N")
  names(totMob)   <- c("PERIODO", "Status", "N")
  names(totCurr)  <- c("PERIODO", "Status", "N")
  
  frames <- rbind(totIng, totDer, totCon, totEvad, totFalso, totDesis, totMob, totCurr)
  
  mat <- as.data.frame(periodoUFCGa)
  mat$Status <- "Matriculado" 
  
  mat$N <- sapply(as.numeric(as.character(periodoUFCGa)), function(x) { 
    length(unique(dados[which((dados$PERIODO_INGRESSO <= x) &
                                (dados$PERIODO_EVASAO >= x | 
                                   is.na(dados$PERIODO_EVASAO))),]$MATRÍCULA)) })
  
  names(mat) <- c("PERIODO", "Status", "N")
  frames     <- rbind(frames, mat)
  
  frames$Curso <- nomecur
  frames$PERIODO <- paste(substr(frames$PERIODO,1,4),".",substr(frames$PERIODO,5,5), sep = "")
  return(frames)
}
#----------------------------------------------------------------------------------
StrataCentroa <- function(centro) {
  
  curso <-  cursos %>% filter(SIGLA == centro, `Situação EMEC`=="Ativo") %>% droplevels()
  cursos <- levels(curso$CURSO)
  
  stratcurs <- lapply(cursos, function(x) {strataCursoanual(as.numeric(as.character(x))) })
  eqTot <- do.call(rbind, stratcurs)
  
  eqTot$CENTRO <- centro
  return(eqTot)
  
}  
#----------------------------------------------------------------------------------
StrataCentrop <- function(centro) {
  
  curso <-  cursos %>% filter(SIGLA == centro, `Situação EMEC`=="Ativo") %>% droplevels()
  cursos <- levels(curso$CURSO)
  
  stratcurs <- lapply(cursos, function(x) strataCursoperiodo(x))
  eqTot <- do.call(rbind, stratcurs)
  
  eqTot$CENTRO <- centro
  return(eqTot)
  
}  
#----------------------------------------------------------------------------------
eqUFCGanual <- function(){
  
  tot <- lapply(SIGLAS, function(x) StrataCentroa(x))
  tot <- do.call(rbind,tot)
  return(tot)
}
#----------------------------------------------------------------------------------
eqUFCGp <- function(){
  
  tot <- lapply(SIGLAS, function(x) StrataCentrop(x))
  tot <- do.call(rbind,tot)
  return(tot)
}
#----------------------------------------------------------------------------------
evasaoLobo <- function(){
  
  setwd("DADOS/Evasao/")
  
  files       <- list.files("./","evasao")
  periodos    <- lapply(files, function(x) { paste(substr(x,1,4),".",substr(x,5,5), sep = "")})
  
  getfiles    <- lapply(files, function(x) {
    read_excel(x)})
  
  setwd("../../")
  
  for (n in (1:length(periodos)) ){
    getfiles[[n]]$PERIODO <- periodos[[n]]
  }
  
  evasao  <- do.call(rbind, getfiles)
  
  #Evasao da UFCG Instituto Lobo
  
  evadUFCG <- ddply(evasao, "PERIODO", summarise, Mat0 = sum(`Vincula. n-1`), Mat1 = sum(Vincula.), Ing1 = sum(Ingress.), Con0 = sum(`Gradua. n-1`), Con1 = sum(Gradua.))
  
  evadUFCG$Ev <- 1- (evadUFCG$Mat1 - evadUFCG$Ing1)/(evadUFCG$Mat0 - evadUFCG$Con0)
  evadUFCG$PERIODO <- as.numeric(evadUFCG$PERIODO)                  
  
  
  
  return(evadUFCG)
}
#----------------------------------------------------------------------------------
evasaoOCDE <- function(){

  
  tot <- eqUFCGanual()
 
  OCDE <- sapply(as.numeric(tot$ANOS[-c(1:4)]), function(x) { 1- tot[which(tot$ANOS==x),]$Con / tot[which(tot$ANOS==(x-3)),]$Ing})
  
 


  return(EvadOCDE)
  
}
#----------------------------------------------------------------------------------

