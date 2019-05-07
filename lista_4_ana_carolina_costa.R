#################### UFPE ####################
############# ANÁLISE DE DADOS ###############
####### ANA CAROLINA DOS SANTOS COSTA ########

#### EXERCICIO 1 ####
#LINK PARA O GITHUB: https://github.com/scostacarolina/lista_4_ana_carolina_costa #

#### EXERCICIO 2 ####
## REQUERER PACOTES NECESSARIOS ##
library(tidyverse)

library(ffbase)

#### PRE PROCESSAMENTO DOS DADOS ####

## DEFINIR DIRETÓRIO UTILIZADO ##

setwd("C:/Users/ana carolina/Desktop/cadeiras-mestrado/análise de dados - davi moreira/aula 02 do R")

## ABRIR BASES DE DADOS DO CENSO 2016 ##

load("matricula_pe_censo_escolar_2016.RData")
load("docentes_pe_censo_escolar_2016.RData")
load("turmas_pe_censo_escolar_2016.RData")
load("escolas_pe_censo_escolar_2016.RData")

#### EXERCICIO 2A ## ## ABRIR BASE DE DADOS PNUD #### 
getwd()

library(readxl)

library(readxl)

pnud <- read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet = 2)

#### CONHECENDO DIMENSOES DA BASE ####

head(pnud)

unique(pnud$ANO)

# SELECIONAR APENAS OS DADOS REFERENTES DO ANO 2010 e DO ESTADO DO PE #

pnud_pe_2010 <- pnud %>% filter(ANO == 2010 & UF == 26)

# REMOVER BASES QUE NÃO SERÃO MAIS UTILIZADAS #

rm(pnud)


# PROCESSAR AS BASES DO CENSO 2016 #

# TURMAS PERNAMBUCO #

turmas_pe_sel <- turmas_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_turmas = n(), 
            turmas_disc_prof = sum(IN_DISC_PROFISSIONALIZANTE, na.rm = T),
            turmas_disc_inf = sum(IN_DISC_INFORMATICA_COMPUTACAO, na.rm = T),
            turmas_disc_mat = sum(IN_DISC_MATEMATICA, na.rm = T),
            turmas_disc_pt = sum(IN_DISC_LINGUA_PORTUGUESA, na.rm = T),
            turmas_disc_en = sum(IN_DISC_LINGUA_INGLES, na.rm = T))

# VERIFICANDO SE ESTA TUDO CERTO # 

dim(turmas_pe_sel)[1] == length(unique(turmas_pe$CO_MUNICIPIO))

summary(turmas_pe_sel)

# ESCOLAS PERNAMBUCO # 

escolas_pe_sel <- escolas_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_escolas = n(), 
            n_escolas_priv = sum(TP_DEPENDENCIA == 4, na.rm = T),
            escolas_func = sum(TP_SITUACAO_FUNCIONAMENTO == 1, na.rm = T),
            escolas_agua_inex = sum(IN_AGUA_INEXISTENTE, na.rm = T),
            escolas_energia_inex = sum(IN_ENERGIA_INEXISTENTE, na.rm = T),
            escolas_esgoto_inex = sum(IN_ESGOTO_INEXISTENTE, na.rm = T),
            escolas_internet = sum(IN_INTERNET, na.rm = T),
            escolas_alimentacao = sum(IN_ALIMENTACAO, na.rm = T))


# VERIFICANDO SE ESTA TUDO CERTO #

dim(escolas_pe_sel)[1] == length(unique(escolas_pe$CO_MUNICIPIO))

summary(escolas_pe_sel)


# DOCENTES PERNAMBUCO # 

docentes_pe_sel <- docentes_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_docentes = n(),
            docentes_media_idade = mean(NU_IDADE),
            docentes_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            docentes_superior = sum(TP_ESCOLARIDADE == 4, na.rm = T),
            docentes_contrato = sum(TP_TIPO_CONTRATACAO %in% c(1, 4), na.rm = T))

# VERIFICANDO SE ESTÁ TUDO CERTO #
summary(docentes_pe_sel)

# MATRICULAS PERNAMBUCO #

matriculas_pe_sel <- matricula_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_matriculas = n(), 
            alunos_media_idade = mean(NU_IDADE),
            alunos_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            alunos_negros = sum(TP_COR_RACA %in% c(2, 3), na.rm = T),
            alunos_indigenas = sum(TP_COR_RACA == 5, na.rm = T),
            alunos_cor_nd = sum(TP_COR_RACA == 0, na.rm = T),
            matriculas_educ_inf = sum(TP_ETAPA_ENSINO %in% c(1, 2), na.rm = T),
            matriculas_educ_fund = sum(TP_ETAPA_ENSINO %in% c(4:21, 41), na.rm = T),
            matriculas_educ_medio = sum(TP_ETAPA_ENSINO %in% c(25:38), na.rm = T)
  )

# VERIFICANDO SE ESTA TUDO CERTO

dim(matriculas_pe_sel)[1] == length(unique(matricula_pe$CO_MUNICIPIO))

summary(matriculas_pe_sel)

#### JUNTAR TODAS AS BASES DE DADOS EM UMA SO #### 

# JUNTAR PNUD COM MATRICULAS #

censo_pnud_pe_sel <- pnud_pe_2010 %>% full_join(matriculas_pe_sel, 
                                                by = c("Codmun7" = "CO_MUNICIPIO")
)

#VERIFICAR DIMENSOES PARA VER SE DEU CERTO #

dim(pnud_pe_2010)

dim(matriculas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# JUNTAR PNUD + MATRICULAS + ESCOLAS #

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(escolas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)

# VERIFICAR DIMENSOES PARA VER SE DEU CERTO#

dim(escolas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# JUNTAR A BASE ANTERIOR COM TURMAS #

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(turmas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)

# VERIFICAR AS DIMENSOES PARA VER SE DEU CERTO #

dim(turmas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# JUNTAR A BASE ANTERIOR COM DOCENTES #

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(docentes_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)

# VERIFICAR DIMENSOES PARA VER SE DEU CERTO #

dim(docentes_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

#### EXERCICIO 2.G ## SALVAR A BASE DE DADOS #### 

setwd()

getwd()

dir()

save(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.RData")

write.csv2(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.csv",
           row.names = F)

#### EXERCICIO 2B ####
## NAO DEVE HAVER DOCENTE COM MAIS DE 70 OU MENOS DE 18 ANOS ##

# OBSERVAR O RESUMO DA BASE #
summary(censo_pnud_pe_sel)

# OBSERVAR O RESUMO DA BASE FILTRANDO A MEDIA DE IDADE #
summary(censo_pnud_pe_sel$alunos_media_idade)

# CARREGANDO A NOVA BASE UNIDA #

load("docentes_pe_censo_escolar_2016.RData")

# FILTRAR A BASE DE ACORDO COM A IDADE MINIMA E MAXIMA # 

docentes_pe_selecao <- docentes_pe%>% filter(NU_IDADE > 18, NU_IDADE < 70)

# VERIFICAR DIMENSOES PARA VER SE DEU CERTO #

dim(docentes_pe_selecao)

#### EXERCICIO 2C ####
## NAO DEVE HAVER ALUNO COM MAIS DE 25 ANOS OU MENOS DE 1 ANO ##

## CARREGAR A BASE DE MATRICULA ##

load("matricula_pe_censo_escolar_2016.RData")

## FILTRAR A BASE DE ACORDO COM AS IDADES MAXIMA E MINIMA ##

matricula_pe_selecao <- matricula_pe%>% filter(NU_IDADE > 1, NU_IDADE < 25)

## VERIFICAR DIMENSOES PARA VER SE DEU CERTO ##

dim(matricula_pe_selecao)

summary(matricula_pe_selecao$NU_IDADE)

#### EXERCICIO 2D ####

## MOSTRANDO ESTATITICAS DESCRITIVAS DO NUMERO DE ALUNOS POR DOCENTE ##
##                            NOS MUNICIPIOS DO ESTADO DE PERNAMBUCO ##

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

## CRIAR A  NOVA VARIAVEL ATRAVES DA UNIAO DAS DUAS BASES MATRICULA E DOCENTE ##

DocentesAlunos <- censo_pnud_pe_sel$n_matriculas/censo_pnud_pe_sel$n_docentes

## VERIFICANDO A ESTATISTICA DESCRITIVA DA NOVA VARIAVEL DOCENTESALUNOS ##

summary(DocentesAlunos)

plot(DocentesAlunos)

library(GGally)
library(rlang)

## SOLICITANDO GRAFICO DE HISTOGRAMA DA NOVA VARIAVEL DOCENTES POR ALUNOS ##

ggplot(censo_pnud_pe_sel, aes(DocentesAlunos))+geom_histogram()

#### EXERCICIO 2E ####
## APRESENTAR O MUNICIPIO COM MAIOR NUMERO DE ALUNO POR DOCENTE E SEU IDHM ## 

names(censo_pnud_pe_sel)

summary(DocentesAlunos)

## DESCOBRI QUE O MUNICIPIO COM MAIOR NUMERO DE ALUNO POR DOCENTE E SEU IDHM 
## E O MUNICIPIO QUE APARECE NA LINHA 177

## JUNTANDO AS VARIAVEIS DOCENTESALUNOS AO BANCO DO PNUD DE PE ##

censo_pnud_pe_sel_docentesalunos <- censo_pnud_pe_sel %>%  mutate(DocentesAlunos)

## SOLICITO PARA VER O NOVO BANCO ## 

View(censo_pnud_pe_sel_docentesalunos)

## SOLICITO PARA VISUALIZAR A LINHA 177 ##
censo_pnud_pe_sel_docentesalunos["177", ]

## A CIDADE E TUPANATINGA ##

#### EXERCICIO 2F ####
## FACA O TESTTE  DO COEFICIENTE DE CORRELACAO DE PEARSON 
## E APRESENTE A SUA RESPOSTA 




# SOLICITO A CORRELACAO DE PEARSON #
cor(censo_pnud_pe_sel_docentesalunos$DocentesAlunos, censo_pnud_pe_sel_docentesalunos$IDHM)

# SOLICITO O TESTE DE CORRELACAO DE PEARSON #

cor.test(censo_pnud_pe_sel_docentesalunos$DocentesAlunos, censo_pnud_pe_sel_docentesalunos$IDHM)

## O RESULTADO DO TESTE DA CORRELACAO E -0,4796604, ## 
## LOGO E UMA CORRELACAO ESTATISTICAMENTE NEGATIVA ##

#### EXERCICIO 3 ####

require(ggplot2)

## SOLICITO O GRAFICO DE DISPERSAO COM O PACOTE GGPLOT ##

ggplot(censo_pnud_pe_sel_docentesalunos, aes(DocentesAlunos, IDHM, color = IDHM))+geom_point()



