                  ####### Good morning! Here to mess with some beautiful data?#########
                  ####### To start, load the library for the glm and glmer models. We need ingredients!######
library(lme4)
library(tidyverse)

                 ####### Next part: go to the 'session' tab on top, then 'set working directory', then 'choose directory'.
                 ####### Make sure the folder you select can see all of the .csv files listed below, but don't go any deeper.
                 ######## This section below creates tables from the .csv files those folders.

#subject query:
  #Brazilian:
    Sub_BR_Blog <- read.csv("Para-Subject/BR/Para-BR-Blog.csv", sep=",", dec=".", stringsAsFactors=T, skip=5)
    Sub_BR_Legal <- read.csv("Para-Subject/BR/Para-BR-Legal.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    Sub_BR_News <- read.csv("Para-Subject/BR/Para-BR-News.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    Sub_BR_Encyclopedia <- read.csv("Para-Subject/BR/Para-BR-Encyclopedia.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)

  #European:
    Sub_PT_Blog <- read.csv("Para-Subject/PT/Para-PT-Blog.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    Sub_PT_Legal <- read.csv("Para-Subject/PT/Para-PT-Legal.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    Sub_PT_News <- read.csv("Para-Subject/PT/Para-PT-News.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    Sub_PT_Encyclopedia <- read.csv("Para-Subject/PT/Para-PT-Encyclopedia.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    
#Object query:
  #Brazilian:
    Obj_BR_Blog <- read.csv("Para-Object/BR/Para-BR-Blog.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    Obj_BR_Legal <- read.csv("Para-Object/BR/Para-BR-Legal.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    Obj_BR_News <- read.csv("Para-Object/BR/Para-BR-News.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    Obj_BR_Encyclopedia <- read.csv("Para-Object/BR/Para-BR-Encyclopedia.csv", sep=",", dec=".", stringsAsFactors=T)
    
  #European:
    Obj_PT_Blog <- read.csv("Para-Object/PT/Para-PT-Blog.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    Obj_PT_Legal <- read.csv("Para-Object/PT/Para-PT-Legal.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    Obj_PT_News <- read.csv("Para-Object/PT/Para-PT-News.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)
    Obj_PT_Encyclopedia <- read.csv("Para-Object/PT/Para-PT-Encyclopedia.csv", sep=",", dec=".", stringsAsFactors=T, skip=4)

#this part adds 3 columns to each data-frame to include variety, register, and structure
#Personal Pronoun queries:
  #Brazilian: 
  Sub_BR_Blog_RegVarStr <- Sub_BR_Blog  %>% add_column(Variety = "Brazilian", Register="Blog", Structure="Subject")
  Sub_BR_Legal_RegVarStr <- Sub_BR_Legal  %>% add_column(Variety = "Brazilian", Register="Legal", Structure="Subject")
  Sub_BR_News_RegVarStr <- Sub_BR_News  %>% add_column(Variety = "Brazilian", Register="News", Structure="Subject")
  Sub_BR_Encyclopedia_RegVarStr <- Sub_BR_Encyclopedia  %>% add_column(Variety = "Brazilian", Register="Encyclopedia", Structure="Subject")
  
  #European
  Sub_PT_Blog_RegVarStr <- Sub_PT_Blog  %>% add_column(Variety = "European", Register="Blog", Structure="Subject")
  Sub_PT_Legal_RegVarStr <- Sub_PT_Legal  %>% add_column(Variety = "European", Register="Legal", Structure="Subject")
  Sub_PT_News_RegVarStr <- Sub_PT_News  %>% add_column(Variety = "European", Register="News", Structure="Subject")
  Sub_PT_Encyclopedia_RegVarStr <- Sub_PT_Encyclopedia  %>% add_column(Variety = "European", Register="Encyclopedia", Structure="Subject")

  
#Object Clitic queries:
  #Brazilian
  Obj_BR_Blog_RegVarStr <- Obj_BR_Blog  %>% add_column(Variety = "Brazilian", Register="Blog", Structure="Object")
  Obj_BR_Legal_RegVarStr <- Obj_BR_Legal  %>% add_column(Variety = "Brazilian", Register="Legal", Structure="Object")
  Obj_BR_News_RegVarStr <- Obj_BR_News  %>% add_column(Variety = "Brazilian", Register="News", Structure="Object")
  Obj_BR_Encyclopedia_RegVarStr <- Obj_BR_Encyclopedia  %>% add_column(Variety = "Brazilian", Register="Encyclopedia", Structure="Object")
  
  
  #European
  Obj_PT_Blog_RegVarStr <- Obj_PT_Blog  %>% add_column(Variety = "European", Register="Blog", Structure="Object")
  Obj_PT_Legal_RegVarStr <- Obj_PT_Legal  %>% add_column(Variety = "European", Register="Legal", Structure="Object")
  Obj_PT_News_RegVarStr <- Obj_PT_News  %>% add_column(Variety = "European", Register="News", Structure="Object")
  Obj_PT_Encyclopedia_RegVarStr <- Obj_PT_Encyclopedia  %>% add_column(Variety = "European", Register="Encyclopedia", Structure="Object")

#Michael Coding Here:
#this stretch binds all of the .csvs into one dataframe so we can play with it
  Sub <- rbind(Sub_BR_Blog_RegVarStr, Sub_BR_Legal_RegVarStr, Sub_BR_News_RegVarStr, Sub_BR_Encyclopedia_RegVarStr, Sub_PT_Blog_RegVarStr, Sub_PT_Legal_RegVarStr, Sub_PT_Encyclopedia_RegVarStr)
  Obj <- rbind(Obj_BR_Blog_RegVarStr, Obj_BR_Legal_RegVarStr, Obj_BR_News_RegVarStr, Obj_BR_Encyclopedia_RegVarStr, Obj_PT_Blog_RegVarStr, Obj_PT_Legal_RegVarStr, Obj_PT_News_RegVarStr, Obj_PT_Encyclopedia_RegVarStr)

  df1 <- bind_rows(Sub_PT_News, Sub)
  
    Obj$KWIC <- tolower(Obj$KWIC)
    Sub$KWIC <- tolower(Sub$KWIC)

 
#Pronoun
      ObjP <- Obj %>% mutate(Pronoun = str_extract(KWIC, ".{2,4}(?=\\s)"))
      SubP <- Sub %>% mutate(Pronoun = str_extract(KWIC, "(?<=para).*(=?\\s)"))

#Jill learning Michael's coding here
#Realization column: this uses regular expressions to extract the verb from the KWIC column and places it into a new column called MatrixVerb
  ObjPinf <- ObjP %>% mutate(Realization = str_extract(KWIC, "(?<=\\bpara\\s).*"))
  firstverb <- ObjPinf %>% mutate(MatrixVerb = str_extract(KWIC, "(?<=\\s)\\b.*(?=\\s\\bpara\\s)"))
  SubPinf <- SubP %>% mutate(Realization = str_extract(KWIC, "(?<=nós|tu|vós|eles|las|nos|vos).*"))
  firstverbs <- SubPinf %>% mutate(MatrixVerb = str_extract(KWIC, "\\b.*(?=\\s\\bpara\\s)"))
  
  
    WithInf <- rbind(firstverb, firstverbs)
  
#person of infinitive: this looks in the Realization column for verbs marked with the 'res', 'rdes', 'rmos', or 'ren' and markes them as '2SG', '2PL', '1PL', and '3PL'
  PersonofInf <- WithInf %>% 
    mutate(Person_of_Infinitive = ifelse(str_detect(Realization, "(r)?(?<!d)es$"),"2SG",
                                         ifelse(str_detect(Realization, "(r)?des$"), "2PL", 
                                                ifelse(str_detect(Realization, "(r)?mos$"), "1PL",
                                                ifelse(str_detect(Realization, "(r)?en$"),"3PL",
                                                       ifelse(str_detect(Realization, "\\-"),"Compound",
                                                              ""))))))
      
#this looks in the register column and creates a new column called genre, populating genre with 'formal' if register shows 'legal', 'news', or 'encyclopedia'
Genres <- PersonofInf %>% mutate(Genre = ifelse(str_detect(Register, "Legal|News|Encyclopedia"), "Formal", "Informal"))

#this takes the dataframe with all the updated columns/variables including the genre and places it in a new dataframe called corrected genres
CorrectedGenres <- Genres

#this stretch is my father the amazing programmer switching each of the websites below to the opposite genre, from informal --> formal
for (row in 1:nrow(CorrectedGenres))
{
  url <- CorrectedGenres[row, "Reference"]
  if (url == "jus.com.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "espada.eti.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "montfort.org.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "ocaminho.com.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "luteranos.com.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "direitonet.com.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "espacojames.com.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "rainhamaria.com.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "peticaopublica.com.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "portalsaofrancisco.com.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "carloscastellobranco.com.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "mec.gov.br") CorrectedGenres[row, 'Genre'] <- "Formal"
  else
    if (url == "uol.com.br") CorrectedGenres[row, 'Genre'] <- "Formal"
}

#this takes the dataframe containing the corrected genres from the infinite for loop strech above and puts it back into another dataframe called 'outcome'. Just to be consistent. Note that this 'outcome' is the full table of all data, not the column in that table called Outcome. So there are two Outcomes. If that's confusing the names can easily be changed.
Outcome <- CorrectedGenres  %>% mutate(Outcome = ifelse(str_detect(Person_of_Infinitive, "1SG|1PL|2SG|2PL|3PL"), "Personal infinitive", "Impersonal infinitive"))

#these sections take out any KWIC hits that contain <s> or -  
Outcome <- Outcome[!grepl("<s>", Outcome$KWIC),]
Outcome <- Outcome[!grepl("-", Outcome$MatrixVerb),]


  Outcome$Outcome <- as.factor(Outcome$Outcome)
  Outcome$Register <- as.factor(Outcome$Register)
  Outcome$Variety <- as.factor(Outcome$Variety)
  Outcome$Structure <- as.factor(Outcome$Structure)
  Outcome$Pronoun <- as.factor(Outcome$Pronoun)
  Outcome$Person_of_Infinitive <- as.factor(Outcome$Person_of_Infinitive)
  
  #This is the stats model itself, the logistic regression and its variables. It breaks the least,
  #is least complicated, and explains the most, relative to other models. Running this line won't
  #actually show you anything, it just saves the result in a thing (called a variable or vector)
  #that I named PersonalInf_noStr for 'no structure' variable.
  PersonalInf_noStr <- glm(Outcome ~ Genre + Variety + Pronoun, data=Outcome, family="binomial")

  #once you run that, run the summary() function to actually see the results with significance
  #listed in table form.
    
  summary(PersonalInf_noStr)

      drop1(PersonalInf_6)


      
      
      
      
      
      
      
      
      
      
      
      
      
  ################ no need to run this unless you want to know specific numbers  about the 'nrows' or 'number of rows' in a given column that match the string of text in "". So Outcome$Outcome == "Personal infinitive" says
  ################ Hey look in the Outcome dataframe, then search for a column i nthat dataframe named Outcome (dataframe$column), and if in that column you find exactly the characters (==) that make up Personal infinitive, count it

  nrow(Outcome[Outcome$Pronoun == "nos" & Outcome$Outcome == "Personal infinitive", ])
  nrow(Outcome[Outcome$Pronoun =="nós" & Outcome$Outcome == "Personal infinitive", ])
  nrow(Outcome[Outcome$Pronoun == "tu" & Outcome$Outcome == "Personal infinitive", ])
  nrow(Outcome[Outcome$Pronoun == "te" & Outcome$Outcome == "Personal infinitive", ])
  nrow(Outcome[Outcome$Pronoun == "vos" & Outcome$Outcome == "Personal infinitive", ])
  nrow(Outcome[Outcome$Pronoun == "eles"& Outcome$Outcome == "Personal infinitive",  ])
  nrow(Outcome[Outcome$Pronoun == "las"& Outcome$Outcome == "Personal infinitive",  ])
  
  nrow(Outcome[Outcome$Person_of_Infinitive == "1PL" & Outcome$Outcome == "Personal infinitive",])
  nrow(Outcome[Outcome$Person_of_Infinitive == "2SL" & Outcome$Outcome == "Personal infinitive",])
  nrow(Outcome[Outcome$Person_of_Infinitive == "2PL" & Outcome$Outcome == "Personal infinitive",])
  nrow(Outcome[Outcome$Person_of_Infinitive == "3PL" & Outcome$Outcome == "Personal infinitive",])

#and this is how to cite stuff in R. 
citation(package ="lme4")
citation(package ="tidyverse")
