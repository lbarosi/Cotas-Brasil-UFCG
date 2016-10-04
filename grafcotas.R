source("analiseCursosBASEv7.r")
source("cotas.r")

#Set Environment Variables
TEXINPUTS="./" #Path to tex file in Windows

Sys.setenv(TEXINPUTS="./", BIBINPUTS=TEXINPUTS,BSTINPUTS=TEXINPUTS) 

#Path to texfiles in Windows, set BIB files and BST files the same

#Run before clicking "Compile PDF"


#Topo de renda 1%, 0.5% 0.1% INDIA 1920 - 2000
desigualdadeIndia

escravosg
#Topo de renda 1%, 0.5% 0.1% US 1920 - 2000
desigualdadeUS

#Curva de Preston Expextativa de Vida x GDP per capita - 1965
grafPreston1965

#Curva de Preston Expextativa de Vida x GDP per capita - 2005
grafPreston2005

#Brasil Composição Racial 1872 - 2010 - porcentual
brasilPP

#Brasil Composição Racial 2001 - 2011 - absoluta
brasilPopP

#Brasil Pib per capita 1960 - 2015
brasildataPIB

#Brasil GINI per capita 1960 - 2015
brasildataGINI


#IDH seleção de países
idhSelecao

#IDH Long run
idhSelecaoLR

#Brasil anos de estudo raça
brAnosestudog

#Brasil - Ensino Superior Público - Quintos de Renda
brquintosP


#Brasil anos de estudo quintos
brquintosA

#--
pensinomedio

pcotas

pcotas1


pENEMcotas

pcotistaIEAevol     

pcotistaCRAevol


#Evasão de cotas
evadCOTA


estatisticas
