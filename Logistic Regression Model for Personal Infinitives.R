                  ####### Good morning! Here to mess with some beautiful data?#########
                  ####### To start, load the library for the glm and glmer models. We need ingredients!######
library(lme4)
library(tidyverse)
library(effects)
library(officer)
library(flextable)
library(broom)
library(openxlsx)

                 ####### Next part: go to the 'session' tab on top, then 'set working directory', then 'choose directory'.
                 ####### Make sure the folder you select can see all of the .csv files listed below, but don't go any deeper.
                 ######## This section below creates tables from the .csv files those folders.
                  
########Para##########
          #Subject
                    #Brazilian:
                    Sub_BR_Blog <- read.csv("Para-Subject/BR/Para-BR-Blog.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Sub_BR_Legal <- read.csv("Para-Subject/BR/Para-BR-Legal.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Sub_BR_News <- read.csv("Para-Subject/BR/Para-BR-News.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Sub_BR_Encyclopedia <- read.csv("Para-Subject/BR/Para-BR-Encyclopedia.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    
                    
                    #European:
                    Sub_PT_Blog <- read.csv("Para-Subject/PT/Para-PT-Blog.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Sub_PT_Legal <- read.csv("Para-Subject/PT/Para-PT-Legal.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Sub_PT_News <- read.csv("Para-Subject/PT/Para-PT-News.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Sub_PT_Encyclopedia <- read.csv("Para-Subject/PT/Para-PT-Encyclopedia.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)     

                    
                    
        #Object query:
                    #Brazilian:
                    Obj_BR_Blog <- read.csv("Para-Object/BR/Para-BR-Blog.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Obj_BR_Legal <- read.csv("Para-Object/BR/Para-BR-Legal.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Obj_BR_News <- read.csv("Para-Object/BR/Para-BR-News.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Obj_BR_Encyclopedia <- read.csv("Para-Object/BR/Para-BR-Encyclopedia.csv", header=T, sep=",", dec=".", stringsAsFactors=T)
                    
                    #European:
                    Obj_PT_Blog <- read.csv("Para-Object/PT/Para-PT-Blog.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Obj_PT_Legal <- read.csv("Para-Object/PT/Para-PT-Legal.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Obj_PT_News <- read.csv("Para-Object/PT/Para-PT-News.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Obj_PT_Encyclopedia <- read.csv("Para-Object/PT/Para-PT-Encyclopedia.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                
        
#################
######Pra########
#################

        #Subject
                    #Brazilian:
                    Sub_BR_BlogPra <- read.csv("Pra-Subject/Pra-BR-Blog.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)

                    Sub_BR_NewsPra <- read.csv("Pra-Subject/Pra-BR-News.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Sub_BR_EncyclopediaPra <- read.csv("Pra-Subject/Pra-BR-Encyclopedia.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    
                    #European: 
                    #N/A                    
          #Object:                    
                    #Brazilian:
                    Obj_BR_BlogPra <- read.csv("Pra-Object/Pra-PT-Blog.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)

                    Obj_BR_NewsPra <- read.csv("Pra-Object/Pra-BR-News.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    Obj_BR_EncyclopediaPra <- read.csv("Pra-Object/Pra-BR-Encyclopedia.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    
                    #European:
                    Obj_PT_BlogPra <- read.csv("Pra-Object/Pra-PT-Blog.csv", header=T, sep=",", dec=".", stringsAsFactors=T, skip=4)
                    
                    
                    
#this part adds 3 columns to each data-frame: variety, register, and structure


#Pronoun queries:
  #Brazilian Para: 
          Sub_BR_Blog_RegVarStr <- Sub_BR_Blog  %>% add_column(Variety = "Brazilian", Register="Blog", Structure="Subject", Type="Para")
          Sub_BR_Legal_RegVarStr <- Sub_BR_Legal  %>% add_column(Variety = "Brazilian", Register="Legal", Structure="Subject", Type="Para")
          Sub_BR_News_RegVarStr <- Sub_BR_News  %>% add_column(Variety = "Brazilian", Register="News", Structure="Subject", Type="Para")
          Sub_BR_Encyclopedia_RegVarStr <- Sub_BR_Encyclopedia  %>% add_column(Variety = "Brazilian", Register="Encyclopedia", Structure="Subject", Type="Para")
   #European Para
          Sub_PT_Blog_RegVarStr <- Sub_PT_Blog  %>% add_column(Variety = "European", Register="Blog", Structure="Subject", Type="Para")
          Sub_PT_Legal_RegVarStr <- Sub_PT_Legal  %>% add_column(Variety = "European", Register="Legal", Structure="Subject", Type="Para")
          Sub_PT_News_RegVarStr <- Sub_PT_News  %>% add_column(Variety = "European", Register="News", Structure="Subject", Type="Para")
          Sub_PT_Encyclopedia_RegVarStr <- Sub_PT_Encyclopedia  %>% add_column(Variety = "European", Register="Encyclopedia", Structure="Subject", Type="Para")
 
          
#Object queries:
  #Brazilian Para
          Obj_BR_Blog_RegVarStr <- Obj_BR_Blog  %>% add_column(Variety = "Brazilian", Register="Blog", Structure="Object", Type="Para")
          Obj_BR_Legal_RegVarStr <- Obj_BR_Legal  %>% add_column(Variety = "Brazilian", Register="Legal", Structure="Object", Type="Para")
          Obj_BR_News_RegVarStr <- Obj_BR_News  %>% add_column(Variety = "Brazilian", Register="News", Structure="Object", Type="Para")
          Obj_BR_Encyclopedia_RegVarStr <- Obj_BR_Encyclopedia  %>% add_column(Variety = "Brazilian", Register="Encyclopedia", Structure="Object", Type="Para")
  #European Para
          Obj_PT_Blog_RegVarStr <- Obj_PT_Blog  %>% add_column(Variety = "European", Register="Blog", Structure="Object", Type="Para")
          Obj_PT_Legal_RegVarStr <- Obj_PT_Legal  %>% add_column(Variety = "European", Register="Legal", Structure="Object", Type="Para")
          Obj_PT_News_RegVarStr <- Obj_PT_News  %>% add_column(Variety = "European", Register="News", Structure="Object", Type="Para")
          Obj_PT_Encyclopedia_RegVarStr <- Obj_PT_Encyclopedia  %>% add_column(Variety = "European", Register="Encyclopedia", Structure="Object", Type="Para")

          
          
          
#################Pra################      
#Brazilian Pra
          Sub_BR_BlogPra_RegVarStr <- Sub_BR_BlogPra  %>% add_column(Variety = "Brazilian", Register="Blog", Structure="Subject", Type="Pra")
          Sub_BR_NewsPra_RegVarStr <- Sub_BR_BlogPra  %>% add_column(Variety = "Brazilian", Register="News", Structure="Subject", Type="Pra")
          Sub_BR_EncyclopediaPra_RegVarStr <- Sub_BR_BlogPra  %>% add_column(Variety = "Brazilian", Register="Encyclopedia", Structure="Subject", Type="Pra")
          #European Pra: N/A
          

  #Brazilian Pra
          Obj_BR_BlogPra_RegVarStr <- Obj_BR_BlogPra  %>% add_column(Variety = "Brazilian", Register="Blog", Structure="Object", Type="Pra")
          Obj_BR_NewsPra_RegVarStr <- Obj_BR_BlogPra  %>% add_column(Variety = "Brazilian", Register="News", Structure="Object", Type="Pra")
          Obj_BR_EncyclopediaPra_RegVarStr <- Obj_BR_BlogPra  %>% add_column(Variety = "Brazilian", Register="Encyclopedia", Structure="Object", Type="Pra")
  #European Pra
          Obj_PT_BlogPra_RegVarStr <- Obj_PT_BlogPra  %>% add_column(Variety = "European", Register="Blog", Structure="Object", Type="Pra")
          
          
          
          

          
#Michael Coding Here:
#this stretch binds all of the .csvs into one dataframe so we can play with it
        Sub <- rbind(Sub_BR_Blog_RegVarStr, Sub_BR_Legal_RegVarStr, Sub_BR_News_RegVarStr, Sub_BR_Encyclopedia_RegVarStr, Sub_PT_Blog_RegVarStr, Sub_PT_Legal_RegVarStr, Sub_PT_News_RegVarStr, Sub_PT_Encyclopedia_RegVarStr)
        Obj <- rbind(Obj_BR_Blog_RegVarStr, Obj_BR_Legal_RegVarStr, Obj_BR_News_RegVarStr, Obj_BR_Encyclopedia_RegVarStr, Obj_PT_Blog_RegVarStr, Obj_PT_Legal_RegVarStr, Obj_PT_News_RegVarStr, Obj_PT_Encyclopedia_RegVarStr)

  df1 <- bind_rows(Sub_PT_News, Sub)
  
    Obj$KWIC <- tolower(Obj$KWIC)
    Sub$KWIC <- tolower(Sub$KWIC)

    
    #pra   
    SubPra <- rbind(Sub_BR_BlogPra_RegVarStr, Sub_BR_NewsPra_RegVarStr, Sub_BR_EncyclopediaPra_RegVarStr)
    ObjPra <- rbind(Obj_BR_BlogPra_RegVarStr, Obj_BR_NewsPra_RegVarStr, Obj_BR_EncyclopediaPra_RegVarStr, Obj_PT_BlogPra_RegVarStr)
    
    ObjPra$KWIC <- tolower(ObjPra$KWIC)
    SubPra$KWIC <- tolower(SubPra$KWIC)

 
#Pronoun
  #Para 
      ObjP <- Obj %>% mutate(Pronoun = str_extract(KWIC, ".{2,4}(?=\\s)"))
      SubP <- Sub %>% mutate(Pronoun = str_extract(KWIC, "(?<=\\spara\\s).*?(?=\\s)")) #() is capturing group
    

  #pra
      ObjPPra <- ObjPra %>% mutate(Pronoun = str_extract(KWIC, ".{2,4}(?=\\s)"))
      SubPPra <- SubPra %>% mutate(Pronoun = str_extract(KWIC, "(?<=\\spra\\s).*?(?=\\s)"))
      
#Jill learning Michael's coding here
#Realization column: this uses regular expressions to extract the verb from the KWIC column and places it into a new column called MatrixVerb
  ObjPinf <- ObjP %>% mutate(Realization = str_extract(KWIC, "(?<=\\bpara\\s).*"))
  firstverb <- ObjPinf %>% mutate(MatrixVerb = str_extract(KWIC, "(?<=\\s)\\b.*(?=\\s\\bpara\\s)"))
  SubPinf <- SubP %>% mutate(Realization = str_extract(KWIC, "(?<=nós|tu|vós|eles|las|nos|vos).*"))
  firstverbs <- SubPinf %>% mutate(MatrixVerb = str_extract(KWIC, "\\b.*(?=\\s\\bpara\\s)"))

  WithInf <- rbind(firstverb, firstverbs)
  
  
  
#pra
  ObjPinfPra <- ObjPPra %>% mutate(Realization = str_extract(KWIC, "(?<=\\bpra\\s).*"))
  firstverbPra <- ObjPinfPra %>% mutate(MatrixVerb = str_extract(KWIC, "(?<=\\s)\\b.*(?=\\s\\bpra\\s)"))
  SubPinfPra <- SubPPra %>% mutate(Realization = str_extract(KWIC, "(?<=nós|tu|vós|eles|las|nos|vos).*"))
  firstverbsPra <- SubPinfPra %>% mutate(MatrixVerb = str_extract(KWIC, "\\b.*(?=\\s\\bpra\\s)"))
  

    WithInfPra <- rbind(firstverbPra, firstverbsPra)
  
#person of infinitive: this looks in the Realization column for verbs marked with the 'res', 'rdes', 'rmos', or 'ren' and markes them as '2SG', '2PL', '1PL', and '3PL'
  PersonofInf <- WithInf %>% 
    mutate(Person_of_Infinitive = ifelse(str_detect(Realization, "(r)?(?<!d)es$"),"2SG",
                                         ifelse(str_detect(Realization, "(r)?des$"), "2PL", 
                                                ifelse(str_detect(Realization, "(r)?mos$"), "1PL",
                                                ifelse(str_detect(Realization, "(r)?en$"),"3PL",
                                                       ifelse(str_detect(Realization, "\\-"),"Compound",
                                                              ""))))))
      
  PersonofInfPra <- WithInfPra %>% 
    mutate(Person_of_Infinitive = ifelse(str_detect(Realization, "(r)?(?<!d)es$"),"2SG",
                                         ifelse(str_detect(Realization, "(r)?des$"), "2PL", 
                                                ifelse(str_detect(Realization, "(r)?mos$"), "1PL",
                                                       ifelse(str_detect(Realization, "(r)?en$"),"3PL",
                                                              ifelse(str_detect(Realization, "\\-"),"Compound",
                                                                     ""))))))
  
    
  #this looks in the register column and creates a new column called genre, populating genre with 'formal' if register shows 'legal', 'news', or 'encyclopedia'
Genres <- PersonofInf %>% mutate(Genre = ifelse(str_detect(Register, "Legal|News|Encyclopedia"), "Formal", "Informal"))
CorrectedGenres <- Genres


GenresPra <- PersonofInfPra %>% mutate(Genre = ifelse(str_detect(Register, "Legal|News|Encyclopedia"), "Formal", "Informal"))
CorrectedGenresPra <- GenresPra



#this stretch is my father the amazing programmer switching each of the websites below to the opposite genre, from informal --> formal
for (row in 1:nrow(CorrectedGenres))
{
  url <- CorrectedGenres[row, "Reference"]
  Register <- CorrectedGenres[row, "Register"]
  if (url == "jus.com.br" & Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "espada.eti.br" & Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "montfort.org.br"& Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "ocaminho.com.br"& Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "luteranos.com.br"& Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "direitonet.com.br"& Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "espacojames.com.br"& Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "rainhamaria.com.br"& Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "peticaopublica.com.br"& Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "portalsaofrancisco.com.br"& Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "carloscastellobranco.com.br"& Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "mec.gov.br"& Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
  else
    if (url == "uol.com.br"& Register=="Blog") CorrectedGenres[row, 'Register'] <- "BlogFormal"
}


for (row in 1:nrow(CorrectedGenresPra))
{
  url <- CorrectedGenresPra[row, "Reference"]
  Register <- CorrectedGenresPra[row, "Register"]
  
  if (url == "jus.com.br"& Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
  else
    if (url == "espada.eti.br"& Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
    else
      if (url == "montfort.org.br"& Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
      else
        if (url == "ocaminho.com.br" & Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
        else
          if (url == "luteranos.com.br" & Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
          else
            if (url == "direitonet.com.br" & Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
            else
              if (url == "espacojames.com.br" & Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
              else
                if (url == "rainhamaria.com.br" & Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
                else
                  if (url == "peticaopublica.com.br" & Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
                  else
                    if (url == "portalsaofrancisco.com.br" & Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
                    else
                      if (url == "carloscastellobranco.com.br"& Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
                      else
                        if (url == "mec.gov.br"& Register=="Blog") CorrectedGenresPra[row, 'Register'] <- "BlogFormal"
                        else
                          if (url == "uol.com.br"& Register=="Blog") CorrectedGenresPra[row, 'Genre'] <- "BlogFormal"
}



#this takes the dataframe containing the corrected genres from the infinite for loop strech above and puts it back into another dataframe called 'outcome'. 
#Just to be consistent. Note that this 'outcome' is the full table of all data, not the column in that table called Outcome. 
#So there are two Outcomes.
Outcome <- CorrectedGenres  %>% mutate(Outcome = ifelse(str_detect(Person_of_Infinitive, "1SG|1PL|2SG|2PL|3PL"), "Inflected infinitive", "Uninflected infinitive"))
OutcomePra <- CorrectedGenresPra  %>% mutate(OutcomePra = ifelse(str_detect(Person_of_Infinitive, "1SG|1PL|2SG|2PL|3PL"), "Inflected infinitive", "Uninflected infinitive"))

names(OutcomePra) <- names(Outcome)
OutcomeBoth <- rbind(Outcome, OutcomePra) %>% filter(Pronoun!="vo") %>% filter(Person_of_Infinitive!="Compound")


#these sections take out any KWIC hits that contain <s> or -  
OutcomeBoth <- OutcomeBoth[!grepl("<s>", OutcomeBoth$KWIC),]
OutcomeBoth <- OutcomeBoth[!grepl("-", OutcomeBoth$MatrixVerb),]


  OutcomeBoth$Outcome <- factor(OutcomeBoth$Outcome)
  OutcomeBoth$Outcome2 <- factor(OutcomeBoth$Outcome, labels = c(0,1))
  OutcomeBoth$Register <- factor(OutcomeBoth$Register)
  OutcomeBoth$Variety <- factor(OutcomeBoth$Variety)
  OutcomeBoth$Structure <- factor(OutcomeBoth$Structure)
  OutcomeBoth$Pronoun <- factor(OutcomeBoth$Pronoun)
  OutcomeBoth$Person_of_Infinitive <- factor(OutcomeBoth$Person_of_Infinitive)
  OutcomeBoth$Type <- factor(OutcomeBoth$Type)

  #This is the stats model itself, the logistic regression and its variables. It breaks the least,
  #is least complicated, and explains the most, relative to other models. Running this line won't
  #actually show you anything, it just saves the result in a thing (called a variable or vector)
  #that I named Model.
  #
 
  #why are there more rows than in the observations? Testing here by adding rows. R does notice these. 
#  OutcomeBoth[nrow(OutcomeBoth) + 1,]
  
  OutcomeBoth
#standard model: use this one
  Model <- glm(Outcome ~ Register + Type + Variety + Pronoun, data=OutcomeBoth, family="binomial")
  summary(Model)
  

  #change the reference value back
#  variablep <- relevel(OutcomeBoth$Pronoun, ref = "nos")
#  Model2 <- glm(Outcome ~ Register + Variety + variablep, data=OutcomeBoth, family="binomial")


#log odds are the same as your estimate on your log. regression, and p-values stay the same too
log_odds <- coef(Model)
odds_ratio <- exp(log_odds)
cr <- confint(Model)
odds_ratio <- exp(cbind(OR = coef(Model), cr))

#output to table
tidy_model <- tidy(Model, conf.int = TRUE)
tidy_model$odds_ratio <- exp(tidy_model$estimate) 
tidy_model$conf.low <- exp(tidy_model$conf.low)
tidy_model$conf.high <- exp(tidy_model$conf.high)

#put the tidy_model containing odds ratio and high and low confidence intervals into a table called ft
ft <- flextable(tidy_model)


#print to word documents
doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  body_add_par("Odds Ratio from Logistic Regression", style = "Normal")

print(doc, target = "Odds Ratio Table-nos ref.docx")

nrow(OutcomeBoth)
ncol(OutcomeBoth)

  plot(allEffects(Model))
  plot(ft)
  xtabs(~Pronoun, data=OutcomeBoth)
  xtabs(~Outcome, data=OutcomeBoth)
  xtabs(~Register, data=OutcomeBoth)
  xtabs(~Person_of_Infinitive, data=OutcomeBoth)

  
  #once you run that, run the summary() function to actually see the results with significance
  #listed in table form.


variable2 <- ifelse (OutcomeBoth$Pronoun == "nos", "nos1", OutcomeBoth$Pronoun)

correlationmatrixP <-     table(OutcomeBoth$Pronoun, OutcomeBoth$Outcome)
correlationmatrixV <-     table(OutcomeBoth$Variety, OutcomeBoth$Outcome)      
      
      
  ################ no need to run this unless you want to know specific numbers  about the 'nrows' or 'number of rows' in a given column that match the string of text in "". So Outcome$Outcome == "Inflected infinitive" says
  ################ Hey look in the Outcome dataframe, then search for a column in that dataframe named Pronoun (dataframe$column), and if in that column you find exactly the characters (==) that make up what you're looking for, count it


  nrow(OutcomeBoth[OutcomeBoth$Pronoun == "nós"])

#and this is how to cite stuff in R. 
citation(package ="lme4")
citation(package ="tidyverse")