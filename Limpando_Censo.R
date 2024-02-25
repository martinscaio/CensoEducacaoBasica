library(tidyverse)
library(readxl)


# Censos Brutos---------------------------------------------------------------------------------------------



# 2023
censo2023 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_censo_escolar_2023\\dados\\microdados_ed_basica_2023.csv", 
  delim = ";", locale = locale(encoding = "latin1"))




# 2022
censo2022 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\Microdados do Censo Escolar da Educação Básica 2022\\dados\\microdados_ed_basica_2022.csv", 
  delim = ";", locale = locale(encoding = "latin1"))



#2021
censo2021 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2021\\dados\\microdados_ed_basica_2021.csv", 
  delim = ";", locale = locale(encoding = "latin1"))




#2020
censo2020 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2020\\dados\\microdados_ed_basica_2020.csv", 
  delim = ";", locale = locale(encoding = "latin1"))




#2019
censo2019 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2019\\dados\\microdados_ed_basica_2019.csv", 
  delim = ";", locale = locale(encoding = "latin1"))




#2018
censo2018 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2018\\dados\\microdados_ed_basica_2018.csv", 
  delim = ";", locale = locale(encoding = "latin1"))




#2017
censo2017 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2017\\dados\\microdados_ed_basica_2017.csv", 
  delim = ";", locale = locale(encoding = "latin1"))





#2016
censo2016 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2016\\dados\\microdados_ed_basica_2016.csv", 
  delim = ";", locale = locale(encoding = "latin1"))





#2015
censo2015 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2015\\dados\\microdados_ed_basica_2015.csv", 
  delim = ";", locale = locale(encoding = "latin1"))





#2014
censo2014 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2014\\dados\\microdados_ed_basica_2014.csv", 
  delim = ";", locale = locale(encoding = "latin1"))



#2013
censo2013 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2013\\dados\\microdados_ed_basica_2013.csv", 
  delim = ";", locale = locale(encoding = "latin1"))





#2012
censo2012 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2012\\dados\\microdados_ed_basica_2012.csv", 
  delim = ";", locale = locale(encoding = "latin1"))


#2011
censo2011 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2011\\dados\\microdados_ed_basica_2011.csv", 
  delim = ";", locale = locale(encoding = "latin1"))


#2010
censo2010 <- read_delim(
  "C:\\Users\\Caio\\Desktop\\Censo Educação Básica\\Dados\\microdados_ed_basica_2010\\dados\\microdados_ed_basica_2010.csv", 
  delim = ";", locale = locale(encoding = "latin1"))



# Função para limpar--------------------------------------------------------------------------------------


limpar_censo <- function(dado){
  
  base_provisoria <- dado %>% dplyr::select(NU_ANO_CENSO, 
                                            NO_REGIAO, 
                                            CO_REGIAO, 
                                            NO_UF, 
                                            CO_UF, 
                                            NO_MUNICIPIO, 
                                            CO_MUNICIPIO, 
                                            NO_MESORREGIAO, 
                                            NO_MICRORREGIAO, 
                                            NO_ENTIDADE, 
                                            CO_ENTIDADE, 
                                            TP_DEPENDENCIA, 
                                            TP_CATEGORIA_ESCOLA_PRIVADA,
                                            TP_LOCALIZACAO, 
                                            DS_ENDERECO, 
                                            NU_ENDERECO, 
                                            DS_COMPLEMENTO, 
                                            NO_BAIRRO,
                                            TP_SITUACAO_FUNCIONAMENTO, 
                                            DT_ANO_LETIVO_INICIO, 
                                            DT_ANO_LETIVO_TERMINO, 
                                            IN_MANT_ESCOLA_PRIVADA_EMP,
                                            IN_MANT_ESCOLA_PRIVADA_ONG,
                                            IN_MANT_ESCOLA_PRIVADA_OSCIP ,
                                            IN_MANT_ESCOLA_PRIV_ONG_OSCIP,
                                            IN_MANT_ESCOLA_PRIVADA_SIST_S,
                                            IN_MANT_ESCOLA_PRIVADA_S_FINS,
                                            NU_CNPJ_ESCOLA_PRIVADA, 
                                            NU_CNPJ_MANTENEDORA,
                                            IN_LOCAL_FUNC_PREDIO_ESCOLAR,
                                            QT_MAT_BAS,
                                            QT_MAT_INF,
                                            QT_MAT_INF_CRE,
                                            QT_MAT_INF_PRE,
                                            QT_MAT_FUND,
                                            QT_MAT_FUND_AI,
                                            QT_MAT_FUND_AF,
                                            QT_MAT_MED,
                                            QT_MAT_BAS_FEM,
                                            QT_MAT_BAS_MASC,
                                            QT_MAT_BAS_ND,
                                            QT_MAT_BAS_BRANCA,
                                            QT_MAT_BAS_PRETA,
                                            QT_MAT_BAS_PARDA,
                                            QT_MAT_BAS_AMARELA,
                                            QT_MAT_BAS_INDIGENA,
                                            QT_MAT_BAS_0_3,
                                            QT_MAT_BAS_4_5,
                                            QT_MAT_BAS_6_10,
                                            QT_MAT_BAS_11_14,
                                            QT_MAT_BAS_15_17,
                                            QT_MAT_BAS_18_MAIS,
                                            QT_MAT_BAS_D,
                                            QT_MAT_BAS_N,
                                            QT_MAT_BAS_EAD,
                                            QT_MAT_INF_INT,
                                            QT_MAT_INF_CRE_INT,
                                            QT_MAT_INF_PRE_INT,
                                            QT_MAT_FUND_INT,
                                            QT_MAT_FUND_AI_INT,
                                            QT_MAT_FUND_AF_INT,
                                            QT_MAT_MED_INT) %>% 
    dplyr::filter(NO_MUNICIPIO == "São Paulo" | NO_MUNICIPIO == "São Caetano do Sul"| NO_MUNICIPIO == "Manaus"|
                    NO_MUNICIPIO == "Niterói"| NO_MUNICIPIO == "Londrina" | NO_MUNICIPIO == "São Bernardo" | NO_MUNICIPIO == "Santo André")
  
  
  
  
  
}


# Conversão tipo de dado


censo2022$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2022$NU_CNPJ_ESCOLA_PRIVADA)
censo2022$NU_CNPJ_MANTENEDORA <- as.numeric(censo2022$NU_CNPJ_MANTENEDORA)
censo2021$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2021$NU_CNPJ_ESCOLA_PRIVADA)
censo2021$NU_CNPJ_MANTENEDORA <- as.numeric(censo2021$NU_CNPJ_MANTENEDORA)
censo2020$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2020$NU_CNPJ_ESCOLA_PRIVADA)
censo2020$NU_CNPJ_MANTENEDORA <- as.numeric(censo2020$NU_CNPJ_MANTENEDORA)
censo2019$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2019$NU_CNPJ_ESCOLA_PRIVADA)
censo2019$NU_CNPJ_MANTENEDORA <- as.numeric(censo2019$NU_CNPJ_MANTENEDORA)
censo2018$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2018$NU_CNPJ_ESCOLA_PRIVADA)
censo2018$NU_CNPJ_MANTENEDORA <- as.numeric(censo2018$NU_CNPJ_MANTENEDORA)
censo2017$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2017$NU_CNPJ_ESCOLA_PRIVADA)
censo2017$NU_CNPJ_MANTENEDORA <- as.numeric(censo2017$NU_CNPJ_MANTENEDORA)
censo2016$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2016$NU_CNPJ_ESCOLA_PRIVADA)
censo2016$NU_CNPJ_MANTENEDORA <- as.numeric(censo2016$NU_CNPJ_MANTENEDORA)
censo2015$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2015$NU_CNPJ_ESCOLA_PRIVADA)
censo2015$NU_CNPJ_MANTENEDORA <- as.numeric(censo2015$NU_CNPJ_MANTENEDORA)
censo2014$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2014$NU_CNPJ_ESCOLA_PRIVADA)
censo2014$NU_CNPJ_MANTENEDORA <- as.numeric(censo2014$NU_CNPJ_MANTENEDORA)
censo2013$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2013$NU_CNPJ_ESCOLA_PRIVADA)
censo2013$NU_CNPJ_MANTENEDORA <- as.numeric(censo2013$NU_CNPJ_MANTENEDORA)
censo2012$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2012$NU_CNPJ_ESCOLA_PRIVADA)
censo2012$NU_CNPJ_MANTENEDORA <- as.numeric(censo2012$NU_CNPJ_MANTENEDORA)
censo2011$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2011$NU_CNPJ_ESCOLA_PRIVADA)
censo2011$NU_CNPJ_MANTENEDORA <- as.numeric(censo2011$NU_CNPJ_MANTENEDORA)
censo2010$NU_CNPJ_ESCOLA_PRIVADA <- as.numeric(censo2010$NU_CNPJ_ESCOLA_PRIVADA)
censo2010$NU_CNPJ_MANTENEDORA <- as.numeric(censo2010$NU_CNPJ_MANTENEDORA)





# Joinando os dados


censo <- list(censo2010,censo2011,censo2012,censo2013,censo2014,censo2015,censo2016,censo2017, censo2018,censo2019,censo2020,censo2021,censo2022, censo2023) %>% 
  map(limpar_censo) %>% 
  reduce(full_join)




write_csv(censo, "C:\\Users\\Caio\\Desktop\\CensoEducacaoBasica.csv")












# Censo dos colégios Cognita-------------------------------------------------------------------



censo_cognita <- censo %>% filter(NO_ENTIDADE == "LAVINIENSE ENS INTEG E PINGO DE GENTE" | 
                 NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO" | 
                 NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E PINGO DE GENTE" |
                 NO_ENTIDADE == "MAXI COL - EDUC INF ENS FUND E MED"| 
                 NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E CE PINGO DE GENTE" | 
                 NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E C E E PINGO DE GENTE" |
                 NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E C E PINGO DE GENTE" |
                 NO_ENTIDADE == "CIDADE JARDIM ESCOLA" |
                 NO_ENTIDADE == "VILLARE COLEGIO" |
                 NO_ENTIDADE == "INSTITUTO GAYLUSSAC - JARDIM" |
                 NO_ENTIDADE == "INSTITUTO GAYLUSSAC - ENS FUNDAMENTAL E MEDIO" |
                 NO_ENTIDADE == "ESCOLA VILLARE")
                 



censo_cognita <- censo_cognita %>% mutate(NO_ENTIDADE = case_when(NO_ENTIDADE == "ESCOLA VILLARE" ~ "VILLARE",
                                                  NO_ENTIDADE == "VILLARE COLEGIO" ~ "VILLARE",
                                                  NO_ENTIDADE == "MAXI COL - EDUC INF ENS FUND E MED" ~ "MAXI",
                                                  NO_ENTIDADE == "INSTITUTO GAYLUSSAC - ENS FUNDAMENTAL E MEDIO" ~ "GAYLUSSAC",
                                                  NO_ENTIDADE == "INSTITUTO GAYLUSSAC - JARDIM" ~ "GAYLUSSAC",
                                                  NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E C E PINGO DE GENTE" ~ "LAVINIENSE",
                                                  NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E C E E PINGO DE GENTE" ~ "LAVINIENSE",
                                                  NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E CE PINGO DE GENTE" ~ "LAVINIENSE",
                                                  NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E PINGO DE GENTE" ~ "LAVINIENSE",
                                                  NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO" ~ "LAVINIENSE",
                                                  NO_ENTIDADE == "LAVINIENSE ENS INTEG E PINGO DE GENTE" ~ "LAVINIENSE", TRUE ~ NO_ENTIDADE))








censo_cognita_app <- censo_cognita %>% group_by(NU_ANO_CENSO, NO_ENTIDADE) %>% summarise(QT_MAT_BAS = sum(QT_MAT_BAS))



censo_cognita_app <- censo_cognita_app %>% rename("ANO" = "NU_ANO_CENSO",
                             "ESCOLA" = "NO_ENTIDADE",
                             "MATRICULAS" = "QT_MAT_BAS") 
  DT::datatable(options = list(searching = FALSE,
                               dom = "",
                               pageLength = 14))


  
write_csv(censo_cognita_app, "C:\\Users\\Caio\\Desktop\\censo_cognita_app.csv")


censo_cognita_app <- read_csv("C:\\Users\\Caio\\Desktop\\censo_cognita_app.csv")


#Grafico

censo_cognita_app %>% 
  filter(ESCOLA == "VILLARE") %>% 
  mutate(ANO = factor(ANO)) %>% 
  highcharter::hchart('column', highcharter::hcaes(x = ANO, y = MATRICULAS), name = "Nº de Matrículas") %>% 
  highcharter::hc_xAxis(title = list(text = ""), labels = list(visible = TRUE, style = list(fontWeight = "bold"))) %>% 
  highcharter::hc_yAxis(title = list(text = ""), gridLineWidth = 0)









# Censo dos colégios Concorrentes---------------------------------------------------------------




censo %>% 
  filter(NO_MUNICIPIO == "Londrina") %>% 
  filter(TP_CATEGORIA_ESCOLA_PRIVADA == 1 | TP_CATEGORIA_ESCOLA_PRIVADA == 4) %>% 
  filter(str_detect(NO_ENTIDADE, "PREMIER")) %>% distinct(NO_ENTIDADE) %>%  view()


censo_concorrentes <- censo %>% 
  filter(TP_CATEGORIA_ESCOLA_PRIVADA == 1 | TP_CATEGORIA_ESCOLA_PRIVADA == 4 | TP_CATEGORIA_ESCOLA_PRIVADA == 3) %>% 
  filter(NO_ENTIDADE =="CENTRO DE ENSINO ALPHA DELTA" | NO_ENTIDADE == "CENTRO EDUCACIONAL LATO SENSU" |
         NO_ENTIDADE == "CENTRO EDUC LATO SENSU II"|
         NO_ENTIDADE == "LATO SENSU IV" |
         NO_ENTIDADE == "ESCOLAS IDAAM - UNIDADE DJALMA BATISTA" |
         NO_ENTIDADE == "ESCOLAS IDAAM KIDS" |
         NO_ENTIDADE == "ESCOLAS IDAAM - UNIDADE CACHOEIRINHA" |
         NO_ENTIDADE == "ESCOLA IDAAM - UNIDADE TORQUATO TAPAJOS" |
         NO_ENTIDADE == "ESCOLA IDAAM - UNIDADE CONSTANTINO NERY"|
         NO_ENTIDADE == "ESCOLAS IDAAM UNIDADE MORADA DO SOL" |
         NO_ENTIDADE == "ESCOLAS IDAAM- UNIDADE CIDADE NOVA" |
         NO_ENTIDADE == "CENTRO EDUCACIONAL SECULO" |
         NO_ENTIDADE == "CENTRO EDUCACIONAL ADALBERTO VALLE" |
         NO_ENTIDADE == "CENTRO EDUCACIONAL ADALBERTO VALLE - UNIDADE I"|
         NO_ENTIDADE == "SOCIEDADE DE ASSISTENCIA MATERNO INFANTIL BEBE BOMBOM LTDA" |
         NO_ENTIDADE == "CONNEXUS ENSINO FUNDAMENTAL E MEDIO LTDA" |
         NO_ENTIDADE == "MAPLE BEAR CANADIAN SCHOOL MANAUS"|
         NO_ENTIDADE == "PINOCCHIO CENTRO EDUCACIONAL LTDA" |
         NO_ENTIDADE == "INSTITUTO BATISTA IDA NELSON" |
         NO_ENTIDADE == "COLEGIO BATISTA BRASIL IDA NELSON" |
         NO_ENTIDADE == "ASSOCIACAO DE CIENCIAS EDUCACAO E TECNOLOGIA DA AMAZONIA - COLEGIO MARTHA FALCAO" |
         NO_ENTIDADE == "ASSOCIACAO DE CIENCIAS EDUCACAO E TECNOLOGIA DA AMAZONIA"|
         NO_ENTIDADE == "COLEGIO ARBOS UNIDADE SAO CAETANO DO SUL" |
         NO_ENTIDADE == "COLEGIO ARBOS UNIDADE MARECHAL DEODORO" |
         NO_ENTIDADE == "ARBOS COLEGIO UNIDADE MARECHAL DEODORO" |
         NO_ENTIDADE == "ARBOS COLEGIO UNIDADE MARECHAL DEODORO" |
         NO_ENTIDADE == "ARBOS COLEGIO UNIDADE RIO GRANDE DO SUL"|
         NO_ENTIDADE == "COLEGIO ARBOS UNIDADE MARECHAL DEODORO - EF I"|
         NO_ENTIDADE == "ARBOS COLEGIO UNIDADE RIO GRANDE DO SUL - EI" |
         NO_ENTIDADE == "ARBOS COLEGIO UNIDADE MARECHAL DEODORO - EF II"|
         NO_ENTIDADE == "ARBOS COLEGIO UNIDADE MARECHAL DEODORO - EM" |
         NO_ENTIDADE == "COLEGIO ARBOS - UNIDADE SAO CAETANO DO SUL" |
         NO_ENTIDADE == "MAPLE BEAR DE SAO CAETANO DO SUL" |
         NO_ENTIDADE == "MAPLE BEAR DE SAO CAETANO DO SUL"|
         NO_ENTIDADE == "ATENEU DE SAO CAETANO DO SUL COLEGIO UNIDADE I" |
         NO_ENTIDADE == "ATENEU DE SAO CAETANO DO SUL COLEGIO UNIDADE II" |
         NO_ENTIDADE == "ATENEU DE SAO CAETANO DO SUL COLEGIO UNIDADE III" |
         NO_ENTIDADE == "ESCOLA EVOLUTION TEENS" |
         NO_ENTIDADE == "ESCOLA EVOLUTION SAO CAETANO DE ENSINO BILINGUE LTDA" |
         NO_ENTIDADE == "SAGRADA FAMILIA INSTITUTO DE ENSINO" |
         NO_ENTIDADE == "EDUARDO GOMES COLEGIO" |
         NO_ENTIDADE == "SINGULAR UNIDADE SAO CAETANO COLEGIO" |
         NO_ENTIDADE == "FENIX SANTA PAULA COLEGIO UND I E II" |
         NO_ENTIDADE == "FENIX SANTA PAULA COLEGIO UNDADE I E II" |
         NO_ENTIDADE == "FENIX SANTA PAULA ESCOLA UNDADE I E II"|
         NO_ENTIDADE == "LICEU JARDIM"|
         NO_ENTIDADE == "PORTO SEGURO VISCONDE COLEGIO"|
         NO_ENTIDADE == "VISCONDE DE PORTO SEGURO ESCOLA DA COMUNIDADE"|
         NO_ENTIDADE == "VISCONDE DE PORTO SEGURO COLEGIO UNID III"|
         NO_ENTIDADE == "VISCONDE DE PORTO SEGURO UNID II ESCOLA DA COMUNIDADE"|
         NO_ENTIDADE == "VISCONDE DE PORTO SEGURO COLEGIO UNIDADE III"|
         NO_ENTIDADE == "VISCONDE DE PORTO SEGURO UNIDADE II ESCOLA DA COMUNIDADE"|
         NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO - CAMPUS MORUMBI"|
         NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO - CAMPUS PANAMBY"|
         NO_ENTIDADE == "ESCOLA DA COMUNIDADE VISCONDE DE PORTO SEGURO - CAMPUS JARDIM MORUMBI"|
         NO_ENTIDADE == "ESCOLA DA COMUNIDADE VISCONDE DE PORTO SEGURO - CAMPUS VILA ANDRADE"|
         NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO CAMPUS MORUMBI"|
         NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO CAMPUS PANAMBY"|
         NO_ENTIDADE == "ESCOLA DA COMUNIDADE VISCONDE DE PORTO SEGURO CAMPUS JARDIM MORUMBI"|
         NO_ENTIDADE == "ESCOLA DA COMUNIDADE VISCONDE DE PORTO SEGURO CAMPUS VILA ANDRADE"|
         NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO - CAMPUS VILA ANDRADE"|
         NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO EDUCACAO DE JOVENS E ADULTOS"|
         NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO CAMPUS VILA ANDRADE"|
         NO_ENTIDADE == "PUERI DOMUS ESCOLA - VERBO DIVINO UNID I"|
         NO_ENTIDADE == "PUERI DOMUS ESCOLA ITAIM UNID II"|
         NO_ENTIDADE == "PUERI DOMUS ESCOLA UNIDADE CRESCER SEMPRE"|
         NO_ENTIDADE == "PUERI DOMUS ESCOLA ITAIM UNIDADE II"|
         NO_ENTIDADE == "PUERI DOMUS ESCOLA - VERBO DIVINO UNIDADE I"|
         NO_ENTIDADE == "PUERI DOMUS ESCOLA VERBO DIVINO UNIDADE I"|
         NO_ENTIDADE == "ESCOLA PUERI DOMUS UNIDADE ACLIMACAO"|
         NO_ENTIDADE == "ESCOLA BILINGUE PUERI DOMUS - UNIDADE III - PERDIZES"|
         NO_ENTIDADE == "ESCOLA BILINGUE PUERI DOMUS UNIDADE III PERDIZES"|
         NO_ENTIDADE == "ESCOLA BILINGUE PUERI DOMUS UNIDADE V SUMARE"|
         NO_ENTIDADE == "ESCOLA BILINGUE PUERI DOMUS UNIDADE I VERBO DIVINO"|
         NO_ENTIDADE == "ST NICHOLAS ESCOLA ANGLO BRASILEIRA"|
         NO_ENTIDADE == "AVENUES SAO PAULO"|
         NO_ENTIDADE == "THE BRITISH COLLEGE OF BRAZIL"|
         NO_ENTIDADE == "THE BRITISH COLLEGE OF BRAZIL UNIDADE CHACARA FLORA"|
         NO_ENTIDADE == "BRITANICA DE SAO PAULO ESCOLA ST PAULS SCHOOL" |
         NO_ENTIDADE == "BRITANICA DE SAO PAULO ESCOLA ST PAUL S SCHOOL"|
         NO_ENTIDADE == "BRITANICA DE SAO PAULO ESCOLA STPAUL S SCHOOL" |
         NO_ENTIDADE == "ST FRANCIS COLEGIO INTERNACIONAL"|
         NO_ENTIDADE == "ST FRANCIS ESCOLA INTERNACIONAL"|
         NO_ENTIDADE == "ST FRANCIS COLEGIO INTERNACIONAL UNI PINHEIROS"|
         NO_ENTIDADE == "ST FRANCIS COLEGIO INTERNACIONAL UNIDADE PINHEIROS"|
         NO_ENTIDADE == "ST FRANCIS COLLEGE UNIDADE II"|
         NO_ENTIDADE == "ST FRANCIS COLLEGE UNIDADE I"|
         NO_ENTIDADE == "ST FRANCIS COLLEGE"|
         NO_ENTIDADE == "MARIA IMACULADA CHAPEL SCHOOL"|
         NO_ENTIDADE == "CHAPEL SCHOOL"|
         NO_ENTIDADE == "ESCOLA CONCEPT"|
         NO_ENTIDADE == "MOBILE ESCOLA PRATICA EST ELEMENTARES"|
         NO_ENTIDADE == "MOBILE COLEGIO"|
         NO_ENTIDADE == "MOBILE ESCOLA PRATICA DE ESTUDOS ELEMENTARES"|
         NO_ENTIDADE == "MIGUEL DE CERVANTES COLEGIO"|
         NO_ENTIDADE == "GRADUADA DE SAO PAULO ASSOCIACAO ESCOLA" |
         NO_ENTIDADE == "GRADUADA CENTRO DE RECREACAO INFANTIL" |
         NO_ENTIDADE == "COLEGIO E CURSO PENSI"|
         NO_ENTIDADE == "COLEGIO PENSI LOBO TORRES"|
         NO_ENTIDADE == "COLEGIO E CURSO PENSI - ICARAI 1"|
         NO_ENTIDADE == "COLEGIO E CURSO PENSI - ITAIPU"|
         NO_ENTIDADE == "COLEGIO E CURSO PENSI - ICARAI 2"|
         NO_ENTIDADE == "COLEGIO E CURSO PENSI - LOBO TORRES"|
         NO_ENTIDADE == "COLEGIO E CURSO PENSI - LOBO TORRES" |
         NO_ENTIDADE == "COLEGIO PH"|
         NO_ENTIDADE == "MAPLE BEAR CANADIAN SCHOOL"|
         NO_ENTIDADE == "PB CURSO ICARAI LTDA" |
         NO_ENTIDADE == "ESCOLA CANADENSE EDUCACAO GLOBAL"|
         NO_ENTIDADE == "ESCOLA CANADENSE EDUCACAO GLOBAL CANADIAN SCHOOL OF NITEROI" |
         NO_ENTIDADE == "COLEGIO CANADENSE NITEROI"|
         NO_ENTIDADE == "COLEGIO UNIVERSITARIO EDUCACAO INFANTIL ENSINO FUNDAMENTAL E MEDIO"|
         NO_ENTIDADE == "COLEGIO UNIVERSITARIO ED INFANT ENS FUND E MEDIO"|
         NO_ENTIDADE == "COLEGIO LONDRINENSE"|
         NO_ENTIDADE == "ESCOLA POSITIVO SANTA MARIA - EDUCACAO INFANTIL E ENSINO FUNDAMENTAL"|
         NO_ENTIDADE == "COLEGIO POSITIVO SANTA MARIA - EDUCACAO INFANTIL ENSINO FUNDAMENTAL E MEDIO"|
         NO_ENTIDADE == "COLEGIO POSITIVO LONDRINA - EDUCACAO INFANTIL ENSINO FUNDAMENTAL E MEDIO"|
         NO_ENTIDADE == "CENTRO EDUCACIONAL ST JAMES"|
         NO_ENTIDADE == "ESCOLA ST JAMES EDUCACAO INFANTIL - ENSINO FUNDAMENTAL E MEDIO"|
         NO_ENTIDADE == "ESCOLA ST JAMES EDUCACAO INFANTIL E ENSINO FUNDAMENTAL"|
         NO_ENTIDADE == "COLEGIO PGD - ED INF ENSINO FUNDAMENTAL E MEDIO"|
         NO_ENTIDADE == "ESCOLA PREMIER - EDUCACAO INFANTIL E ENSINO FUNDAMENTAL"|
         NO_ENTIDADE == "COLEGIO PREMIER - EDUCACAO INFANTIL ENSINO FUNDAMENTAL E MEDIO"|
         NO_ENTIDADE == "LAVINIENSE ENS INTEG E PINGO DE GENTE" | 
         NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO" | 
         NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E PINGO DE GENTE" |
         NO_ENTIDADE == "MAXI COL - EDUC INF ENS FUND E MED"| 
         NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E CE PINGO DE GENTE" | 
         NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E C E E PINGO DE GENTE" |
         NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E C E PINGO DE GENTE" |
         NO_ENTIDADE == "CIDADE JARDIM ESCOLA" |
         NO_ENTIDADE == "VILLARE COLEGIO" |
         NO_ENTIDADE == "INSTITUTO GAYLUSSAC - JARDIM" |
         NO_ENTIDADE == "INSTITUTO GAYLUSSAC - ENS FUNDAMENTAL E MEDIO" |
         NO_ENTIDADE == "ESCOLA VILLARE")




# Agrupando mesmos colégios

censo_concorrentes <- censo_concorrentes %>% mutate(NO_ENTIDADE = case_when(NO_ENTIDADE == "CENTRO EDUCACIONAL ADALBERTO VALLE" ~ "ADALBERTO VALLE",
                                                      NO_ENTIDADE == "CENTRO EDUCACIONAL ADALBERTO VALLE - UNIDADE I" ~ "ADALBERTO VALLE",
                                                      NO_ENTIDADE == "ASSOCIACAO DE CIENCIAS EDUCACAO E TECNOLOGIA DA AMAZONIA - COLEGIO MARTHA FALCAO" ~ "MARTHA FALCAO",
                                                      NO_ENTIDADE == "ASSOCIACAO DE CIENCIAS EDUCACAO E TECNOLOGIA DA AMAZONIA" ~ "MARTHA FALCAO",
                                                      NO_ENTIDADE == "COLEGIO BATISTA BRASIL IDA NELSON" ~ "IDA NELSON",
                                                      NO_ENTIDADE == "INSTITUTO BATISTA IDA NELSON" ~ "IDA NELSON",
                                                      NO_ENTIDADE == "COLEGIO ARBOS UNIDADE SAO CAETANO DO SUL" ~ "ARBOS",
                                                      NO_ENTIDADE == "ARBOS COLEGIO UNIDADE MARECHAL DEODORO" ~ "ARBOS",
                                                      NO_ENTIDADE == "ARBOS COLEGIO UNIDADE MARECHAL DEODORO" ~ "ARBOS",
                                                      NO_ENTIDADE == "COLEGIO ARBOS UNIDADE MARECHAL DEODORO - EF I" ~ "ARBOS",
                                                      NO_ENTIDADE == "ARBOS COLEGIO UNIDADE RIO GRANDE DO SUL - EI" ~ "ARBOS",
                                                      NO_ENTIDADE == "ARBOS COLEGIO UNIDADE MARECHAL DEODORO - EF II" ~ "ARBOS",
                                                      NO_ENTIDADE == "ARBOS COLEGIO UNIDADE MARECHAL DEODORO - EM" ~ "ARBOS",
                                                      NO_ENTIDADE == "ARBOS COLEGIO UNIDADE RIO GRANDE DO SUL" ~ "ARBOS",
                                                      NO_ENTIDADE == "COLEGIO ARBOS UNIDADE MARECHAL DEODORO" ~ "ARBOS",
                                                      NO_ENTIDADE == "COLEGIO ARBOS - UNIDADE SAO CAETANO DO SUL" ~ "ARBOS",
                                                      NO_ENTIDADE == "ATENEU DE SAO CAETANO DO SUL COLEGIO UNIDADE I" ~ "ATENEU",
                                                      NO_ENTIDADE == "ATENEU DE SAO CAETANO DO SUL COLEGIO UNIDADE II" ~ "ATENEU",
                                                      NO_ENTIDADE == "ATENEU DE SAO CAETANO DO SUL COLEGIO UNIDADE III" ~ "ATENEU",
                                                      NO_ENTIDADE == "ESCOLA EVOLUTION TEENS" ~ "EVOLUTION KIDS",
                                                      NO_ENTIDADE == "ESCOLA EVOLUTION SAO CAETANO DE ENSINO BILINGUE LTDA" ~ "EVOLUTION KIDS",
                                                      NO_ENTIDADE == "FENIX SANTA PAULA COLEGIO UND I E II" ~ "FENIX",
                                                      NO_ENTIDADE == "FENIX SANTA PAULA COLEGIO UNDADE I E II" ~ "FENIX",
                                                      NO_ENTIDADE == "FENIX SANTA PAULA ESCOLA UNDADE I E II" ~ "FENIX",
                                                      NO_ENTIDADE == "PORTO SEGURO VISCONDE COLEGIO" ~ "PORTO SEGURO",
                                                      NO_ENTIDADE == "VISCONDE DE PORTO SEGURO COLEGIO UNID III" ~ "PORTO SEGURO",
                                                      NO_ENTIDADE == "VISCONDE DE PORTO SEGURO COLEGIO UNIDADE III" ~ "PORTO SEGURO",
                                                      NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO - CAMPUS MORUMBI" ~ "PORTO SEGURO",
                                                      NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO - CAMPUS PANAMBY" ~ "PORTO SEGURO",
                                                      NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO CAMPUS MORUMBI" ~ "PORTO SEGURO",
                                                      NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO CAMPUS PANAMBY" ~ "PORTO SEGURO",
                                                      NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO - CAMPUS VILA ANDRADE" ~ "PORTO SEGURO",
                                                      NO_ENTIDADE == "COLEGIO VISCONDE DE PORTO SEGURO CAMPUS VILA ANDRADE" ~ "PORTO SEGURO",
                                                      NO_ENTIDADE == "PUERI DOMUS ESCOLA - VERBO DIVINO UNID I" ~ "PUERI DOMUS",
                                                      NO_ENTIDADE == "PUERI DOMUS ESCOLA ITAIM UNID II" ~ "PUERI DOMUS",
                                                      NO_ENTIDADE == "PUERI DOMUS ESCOLA UNIDADE CRESCER SEMPRE" ~ "PUERI DOMUS",
                                                      NO_ENTIDADE == "PUERI DOMUS ESCOLA ITAIM UNIDADE II" ~ "PUERI DOMUS",
                                                      NO_ENTIDADE == "PUERI DOMUS ESCOLA - VERBO DIVINO UNIDADE I" ~ "PUERI DOMUS",
                                                      NO_ENTIDADE == "PUERI DOMUS ESCOLA VERBO DIVINO UNIDADE I" ~ "PUERI DOMUS",
                                                      NO_ENTIDADE == "ESCOLA PUERI DOMUS UNIDADE ACLIMACAO" ~ "PUERI DOMUS",
                                                      NO_ENTIDADE == "ESCOLA BILINGUE PUERI DOMUS - UNIDADE III - PERDIZES" ~ "PUERI DOMUS",
                                                      NO_ENTIDADE == "ESCOLA BILINGUE PUERI DOMUS UNIDADE III PERDIZES" ~ "PUERI DOMUS",
                                                      NO_ENTIDADE == "ESCOLA BILINGUE PUERI DOMUS UNIDADE V SUMARE" ~ "PUERI DOMUS",
                                                      NO_ENTIDADE == "ESCOLA BILINGUE PUERI DOMUS UNIDADE I VERBO DIVINO" ~ "PUERI DOMUS",
                                                      NO_ENTIDADE == "THE BRITISH COLLEGE OF BRAZIL" ~ "BRITISH COLLEGE",
                                                      NO_ENTIDADE == "THE BRITISH COLLEGE OF BRAZIL UNIDADE CHACARA FLORA" ~ "BRITISH COLLEGE",
                                                      NO_ENTIDADE == "ST FRANCIS COLEGIO INTERNACIONAL" ~ "ST FRANCIS",
                                                      NO_ENTIDADE == "ST FRANCIS ESCOLA INTERNACIONAL" ~ "ST FRANCIS",
                                                      NO_ENTIDADE == "ST FRANCIS COLEGIO INTERNACIONAL UNI PINHEIROS" ~ "ST FRANCIS",
                                                      NO_ENTIDADE == "ST FRANCIS COLEGIO INTERNACIONAL UNIDADE PINHEIROS" ~ "ST FRANCIS",
                                                      NO_ENTIDADE == "ST FRANCIS COLLEGE UNIDADE II" ~ "ST FRANCIS",
                                                      NO_ENTIDADE == "ST FRANCIS COLLEGE UNIDADE I" ~ "ST FRANCIS",
                                                      NO_ENTIDADE == "ST FRANCIS COLLEGE" ~ "ST FRANCIS",
                                                      NO_ENTIDADE == "MARIA IMACULADA CHAPEL SCHOOL" ~ "CHAPEL SCHOOL",
                                                      NO_ENTIDADE == "CHAPEL SCHOOL" ~ "CHAPEL SCHOOL",
                                                      NO_ENTIDADE == "MOBILE ESCOLA PRATICA EST ELEMENTARES" ~ "MOBILE",
                                                      NO_ENTIDADE == "MOBILE COLEGIO" ~ "MOBILE",
                                                      NO_ENTIDADE == "MOBILE ESCOLA PRATICA DE ESTUDOS ELEMENTARES" ~ "MOBILE",
                                                      NO_ENTIDADE == "GRADUADA DE SAO PAULO ASSOCIACAO ESCOLA" ~ "GRADED",
                                                      NO_ENTIDADE == "GRADUADA CENTRO DE RECREACAO INFANTIL" ~ "GRADED",
                                                      NO_ENTIDADE == "ESCOLA CANADENSE EDUCACAO GLOBAL" ~ "CANADENSE",
                                                      NO_ENTIDADE == "ESCOLA CANADENSE EDUCACAO GLOBAL CANADIAN SCHOOL OF NITEROI" ~ "CANADENSE",
                                                      NO_ENTIDADE == "COLEGIO CANADENSE NITEROI" ~ "CANADENSE",
                                                      NO_ENTIDADE == "COLEGIO UNIVERSITARIO EDUCACAO INFANTIL ENSINO FUNDAMENTAL E MEDIO" ~ "UNIVERSITARIO",
                                                      NO_ENTIDADE == "COLEGIO UNIVERSITARIO ED INFANT ENS FUND E MEDIO" ~ "UNIVERSITARIO",
                                                      NO_ENTIDADE == "ESCOLA POSITIVO SANTA MARIA - EDUCACAO INFANTIL E ENSINO FUNDAMENTAL" ~ "POSITIVO",
                                                      NO_ENTIDADE == "COLEGIO POSITIVO SANTA MARIA - EDUCACAO INFANTIL ENSINO FUNDAMENTAL E MEDIO" ~ "POSITIVO",
                                                      NO_ENTIDADE == "COLEGIO POSITIVO LONDRINA - EDUCACAO INFANTIL ENSINO FUNDAMENTAL E MEDIO" ~ "POSITIVO",
                                                      NO_ENTIDADE == "CENTRO EDUCACIONAL ST JAMES" ~ "ST JAMES",
                                                      NO_ENTIDADE == "ESCOLA ST JAMES EDUCACAO INFANTIL - ENSINO FUNDAMENTAL E MEDIO" ~ "ST JAMES",
                                                      NO_ENTIDADE == "ESCOLA ST JAMES EDUCACAO INFANTIL E ENSINO FUNDAMENTAL" ~ "ST JAMES",
                                                      NO_ENTIDADE == "ESCOLA PREMIER - EDUCACAO INFANTIL E ENSINO FUNDAMENTAL" ~ "PREMIER",
                                                      NO_ENTIDADE == "COLEGIO PREMIER - EDUCACAO INFANTIL ENSINO FUNDAMENTAL E MEDIO" ~ "PREMIER",
                                                      NO_ENTIDADE == "COLEGIO E CURSO PENSI - LOBO TORRES" ~ "COLEGIO PENSI LOBO TORRES",
                                                      NO_ENTIDADE == "BRITANICA DE SAO PAULO ESCOLA ST PAULS SCHOOL" ~ "ST PAULS",
                                                      NO_ENTIDADE == "BRITANICA DE SAO PAULO ESCOLA ST PAUL S SCHOOL" ~ "ST PAULS",
                                                      NO_ENTIDADE == "BRITANICA DE SAO PAULO ESCOLA STPAUL S SCHOOL" ~ "ST PAULS",
                                                      NO_ENTIDADE == "ESCOLA VILLARE" ~ "VILLARE",
                                                      NO_ENTIDADE == "VILLARE COLEGIO" ~ "VILLARE",
                                                      NO_ENTIDADE == "MAXI COL - EDUC INF ENS FUND E MED" ~ "MAXI",
                                                      NO_ENTIDADE == "INSTITUTO GAYLUSSAC - ENS FUNDAMENTAL E MEDIO" ~ "GAYLUSSAC",
                                                      NO_ENTIDADE == "INSTITUTO GAYLUSSAC - JARDIM" ~ "GAYLUSSAC",
                                                      NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E C E PINGO DE GENTE" ~ "LAVINIENSE",
                                                      NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E C E E PINGO DE GENTE" ~ "LAVINIENSE",
                                                      NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E CE PINGO DE GENTE" ~ "LAVINIENSE",
                                                      NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO E PINGO DE GENTE" ~ "LAVINIENSE",
                                                      NO_ENTIDADE == "LAVINIENSE ENSINO INTEGRADO" ~ "LAVINIENSE",
                                                      NO_ENTIDADE == "LAVINIENSE ENS INTEG E PINGO DE GENTE" ~ "LAVINIENSE",TRUE ~ NO_ENTIDADE))






censo_concorrentes_agrupado <- censo_concorrentes %>% group_by(NU_ANO_CENSO, NO_ENTIDADE, NO_MUNICIPIO) %>% summarise(MATRICULAS = sum(QT_MAT_BAS)) 



censo_concorrentes_app <- censo_concorrentes_agrupado %>%
  pivot_wider(names_from = NU_ANO_CENSO, values_from = MATRICULAS) %>%
  mutate(across(where(is.numeric), ~coalesce(., 0))) 
                              
                              
                              
                              
                              
censo_concorrentes_app <- censo_concorrentes_app %>% filter(!str_detect(NO_ENTIDADE, "COMUNIDADE") & !str_detect(NO_ENTIDADE, "JOVENS E ADULTOS"))

write_csv(censo_concorrentes_app, "C:\\Users\\Caio\\Desktop\\censo_concorrentes_app.csv")


censo_concorrentes_app <- read_csv("C:\\Users\\Caio\\Desktop\\censo_concorrentes_app.csv")


censo_concorrentes_app %>% filter(NO_MUNICIPIO == "Manaus") %>% DT::datatable(options = list(searching = FALSE,
                                                                                             dom = ""))
censo2023 %>% filter(NO_MUNICIPIO == "São Paulo") %>% 
  filter(str_detect(NO_ENTIDADE, "MIGUEL")) %>% view()



# Calcular o CAGR

CAGR <- function(valor_inicial, valor_final, n) {
  cagr <- ((valor_final / valor_inicial)^(1 / n)) - 1
  return(cagr)
}


dados_filtrados <- censo_concorrentes_app %>% pivot_longer(cols = -c(NO_ENTIDADE, NO_MUNICIPIO), 
                                        names_to = "ANO", 
                                        values_to = "VALOR") %>% 
  filter(ANO >= 2018 & ANO <= 2023)


valores_iniciais <- dados_filtrados %>%
  filter(ANO == 2018) %>%
  select(NO_ENTIDADE, VALOR) %>%
  rename(VALOR_INICIAL = VALOR)

valores_finais <- dados_filtrados %>%
  filter(ANO == 2023) %>%
  select(NO_ENTIDADE, VALOR) %>%
  rename(VALOR_FINAL = VALOR)


dados_juntos <- inner_join(valores_iniciais, valores_finais, by = "NO_ENTIDADE")


dados_juntos <- dados_juntos %>%
  mutate(CAGR = ((VALOR_FINAL / VALOR_INICIAL) ^ (1 / 6)) - 1) 


censo_concorrentes_app <- dados_filtrados %>% left_join(dados_juntos,by = "NO_ENTIDADE" )


censo_concorrentes_app <- censo_concorrentes_app %>% select(NO_ENTIDADE, NO_MUNICIPIO, ANO, VALOR, CAGR) %>%  
  pivot_wider(names_from = ANO, values_from = VALOR)


censo_concorrentes_app <- censo_concorrentes_app %>% mutate(CAGR = CAGR * 100,
                                  CAGR = round(CAGR, digits = 2))


censo_concorrentes_app <- censo_concorrentes_app %>% mutate(CAGR = ifelse(is.infinite(CAGR), NA, CAGR))


write_csv(censo_concorrentes_app, "C:\\Users\\Caio\\Desktop\\censo_concorrentes_app.csv")


censo_concorrentes_app <- read_csv("C:\\Users\\Caio\\Desktop\\censo_concorrentes_app.csv")






# DADOS PARA PESQUISA DE MANAUS


censo_bairro_manaus <- censo %>% filter(NO_MUNICIPIO == "Manaus") %>% 
  filter(NO_BAIRRO == "ADRIANOPOLIS" | NO_BAIRRO == "PONTA NEGRA" )



write_csv(censo_bairro_manaus, "C:\\Users\\Caio\\Desktop\\censo_bairros_manaus.csv")



matriculas_bairro_manaus <- censo_bairro_manaus  %>% 
  group_by(NO_BAIRRO, NU_ANO_CENSO, TP_CATEGORIA_ESCOLA_PRIVADA) %>% 
  summarise(Matriculas = sum(QT_MAT_BAS,na.rm = TRUE),
            Matriculas_Fundamental1 = sum(QT_MAT_FUND_AI,na.rm = TRUE),
            Matriculas_Fundamental2 = sum(QT_MAT_FUND_AF,na.rm = TRUE),
            Matriculas_MEDIO = sum(QT_MAT_MED,na.rm = TRUE))


write_csv(matriculas_bairro_manaus, "C:\\Users\\Caio\\Desktop\\censo_bairros_matriculas.csv")


manaus_geral_matriculas <- censo %>% filter(NO_MUNICIPIO == "Manaus") %>% 
  group_by(NO_MUNICIPIO,NU_ANO_CENSO, TP_CATEGORIA_ESCOLA_PRIVADA) %>% 
  summarise(Matriculas = sum(QT_MAT_BAS, na.rm = TRUE),
            Matriculas_Fundamental1 = sum(QT_MAT_FUND_AI, na.rm = TRUE),
            Matriculas_Fundamental2 = sum(QT_MAT_FUND_AF, na.rm = TRUE),
            Matriculas_MEDIO = sum(QT_MAT_MED, na.rm = TRUE))



write_csv(manaus_geral_matriculas, "C:\\Users\\Caio\\Desktop\\manaus_geral_matriculas.csv")


