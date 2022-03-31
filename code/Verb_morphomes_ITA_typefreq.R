###  
#   verb_morphomes_ITA_typefreq.R
#
#    R E T R I E V A L   of   M O R P H O M E    P A T T E R N S  (TYPES)
#
#             +++ italian verbal morphology +++   
#
#      v.4.1.0  - - T Y P E  F R E Q U E N C Y - -
#
#     https://github.com/franfranz/Verb_morphomes_ITA
#
### 


#   This script counts the verbs with a L-morphomic pattern in Italian
#   Verb types are collected from morph-it! (Zanchetta & Baroni, 2005)

#   careful: this script is for counting types only. 
#   For token frequency, refer to "verb_morphomes_ITA_tokenfreq.R"


# REFERENCE LEGEND - regular latin graphemes ere used instead of IPA to ensure compatibility
#
# S = voiceless postalveolar fricative 
# tS= voiceless alveloar affricate (with postalveolar solution)
# dG= voiced alveloar affricate (with postalveolar solution)
# N = nasal palatal 
# L = lateral palatal


# clear ws
rm(list=ls())

# required packages: stringr, xtable, 


###
#                                   
#           S E T   I N P U T S 
#            
#
###

# ### #
#
#   INPUT REQUIRED: GRAPHICAL SETTINGS 
#

library(pryr)
library(stringr)




# ### #
#
#   INPUT REQUIRED: PATHS
#


# input directory
inwd=getwd()

# output directories

# to save database
outwd=paste0(inwd,"/", "out")
#dir.create(outwd)

# to save graphs
graphwd=paste0(inwd,"/", "graphs")
#dir.create(graphwd)

# where the code is stored
codewd=inwd



###
#                                   
#           I M P O R T   a n d   P R E P R O C E S S   
#           
#
###

setwd(inwd)


# import morphit#
morphit <- read.delim('morph-it_048.txt', 
                      header = F, 
                      sep='\t', 
                      enc = 'UTF-8')

colnames(morphit) <- c('form', 'lemma_morphit', 'POS')


# morphit$lemma_morphit=NULL # not sure this is the right move - try it out 
morphit$form <- as.character(morphit$form)

#
unique(morphit$POS)


#
# extract forms
#

# present indicative 1 - 3 ps, 1 pp
# present subjunctive 1 ps
# present infinitive 


verbs0=  morphit[morphit$POS=="VER:ind+pres+1+s"|
                   morphit$POS=="VER:ind+pres+3+s"|
                   morphit$POS=="VER:ind+pres+1+p"|
                   morphit$POS=="VER:sub+pres+1+s"|
                   morphit$POS=="VER:inf+pres", 
]


head(verbs0)

verbs0$verbscons=stringr::str_sub(verbs0$form, -1,-1)
unique(verbs0$verbscons)


# discard truncated forms
verbs1=verbs0[verbs0$verbscons=="a"|
                verbs0$verbscons=="e"|
                verbs0$verbscons=="i"|
                verbs0$verbscons=="o"
              , ]

verbs1$verbscons=NULL

#
# conjugation
#
verbs1$conj=str_sub(verbs1$lemma_morphit, -3, -1)
unique(verbs1$conj)

# discard residual clitics
verbs1=verbs1[verbs1$conj!="rsi", ]


# 
verbs2=verbs1
#
#
# increase phonetic transperency



verbs2$fonform=stringr::str_replace(verbs2$form, " ", "") 

## voiceless postalveolar fricative "S"
# list exceptions "sciare"

verbs_ski=verbs2[verbs2$lemma_morphit=="sciare", ] 
verbs2=verbs2[verbs2$form %in% verbs_ski$form== F, ]
verbs_ski$fonform= stringr::str_replace(verbs_ski$form, "^sci", "Si")
verbs2=rbind(verbs2, verbs_ski)

# word initial 
verbs2$fonform=stringr::str_replace(verbs2$fonform, "^scia", "Sa")
verbs2$fonform=stringr::str_replace(verbs2$fonform, "^scio", "So")
verbs2$fonform=stringr::str_replace(verbs2$fonform, "^sciu", "Su")
verbs2$fonform=stringr::str_replace(verbs2$fonform, "^sce", "Se")
verbs2$fonform=stringr::str_replace(verbs2$fonform, "^sci", "Si")
summary(verbs2$fonform==verbs2$form)


# midword
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "scia", "SSa")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "scio", "SSo")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "sciu", "SSu")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "sce", "SSe")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "sci", "SSi")
summary(verbs2$fonform==verbs2$form)
head(verbs2)

## voiceless alveolar fricative + voiceless velar stop
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "sche", "ske")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "schi", "ski")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "sca", "ska")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "sco", "sko")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "scu", "sku")
summary(verbs2$fonform==verbs2$form)
head(verbs2)

## voiceless alveloar+postalveolar affricate, geminate
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ccia", "ttSa")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ccie", "ttSe")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ccio", "ttSo")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cciu", "ttSu")

verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cce", "ttSe")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cci", "ttSi")

summary(verbs2$fonform==verbs2$form)
head(verbs2)


## voiceless alveloar+postalveolar affricate, short
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cia", "tSa")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cie", "tSe")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cio", "tSo")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ciu", "tSu")
summary(verbs2$fonform==verbs2$form)
head(verbs2)

## voiceless velar stop, geminate
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cche", "kke")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cchi", "kki")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cca", "kka")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cco", "kko")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ccu", "kku")
summary(verbs2$fonform==verbs2$form)
head(verbs2)

## voiceless velar stop, short, 3 letters
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "che", "ke")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "chi", "ki")

## 2 letter clusters
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ce", "tSe")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ci", "tSi")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ca", "ka")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "co", "ko")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cu", "ku")
summary(verbs2$fonform==verbs2$form)
head(verbs2)

# residual k 
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "c", "k")
summary(verbs2$fonform==verbs2$form)
head(verbs2)


## voiceless alveloar+postalveolar affricate, geminate
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ggia", "ddGa")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ggie", "ddGe")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ggio", "ddGo")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ggiu", "ddGu")

verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gge", "ddGe")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ggi", "ddGi")

summary(verbs2$fonform==verbs2$form)
head(verbs2)

## voiceless alveloar+postalveolar affricate, short
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gia", "dGa")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gie", "dGe")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gio", "dGo")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "giu", "dGu")
summary(verbs2$fonform==verbs2$form)
head(verbs2)

# two-letter clusters dG/g
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ge", "dGe")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gi", "dGi")

summary(verbs2$fonform==verbs2$form)
head(verbs2)

## voiceless velar stop, geminate
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gghe", "gge")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gghi", "ggi")


summary(verbs2$fonform==verbs2$form)
head(verbs2)

## voiceless velar stop, short 
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ghe", "ge")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ghi", "gi")

# nasal palatal
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "^gn", "N")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gn", "NN")
summary(verbs2$fonform==verbs2$form)
head(verbs2)

# lateral palatal 
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gli", "LL")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "^LL", "L")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "LL$", "LLi")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "LLdG", "glidG")
verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "LLg", "glig")
summary(verbs2$fonform==verbs2$form)
head(verbs2)


#
#
# strip root from inflected form
verbs3= verbs2

#
# root (from infinitive form as reported in lemma)
#
verbs_inf=verbs3[verbs3$POS=="VER:inf+pres", ]
verbs_inf$verblength=(nchar(verbs_inf$fonform)-3)
verbs_inf$inf_root=str_sub(verbs_inf$fonform, 1, verbs_inf$verblength)


# does root of infinitive end in palatal
verbs_inf$endroot=str_sub(verbs_inf$inf_root, -2, -1)
verbs_inf$pal_end=ifelse(verbs_inf$endroot=="tS"|
                           verbs_inf$endroot=="dG"|
                           verbs_inf$endroot=="NN"|
                           verbs_inf$endroot=="LL"|
                           grepl("k$",verbs_inf$endroot)|
                           grepl("g$",verbs_inf$endroot)|
                           verbs_inf$endroot=="lg"|
                           verbs_inf$endroot=="ng",
                         verbs_inf$endroot, "-")
verbs_inf$pal_end=ifelse(verbs_inf$pal_end=="tS"|
                           verbs_inf$pal_end=="dG"|
                           verbs_inf$pal_end=="NN"|
                           verbs_inf$pal_end=="LL"|
                           verbs_inf$pal_end=="lg"|
                           verbs_inf$pal_end=="ng",
                         verbs_inf$pal_end,
                         str_sub(verbs_inf$pal_end, -1, -1)
)

verbs_inf$pal_end_inf=verbs_inf$pal_end


###
#
#   split conjugations 
#
###

# 1st conjugation ere
are_verbs=verbs3[verbs3$conj=="are", ]

# 2nd conjugation ere
ere_verbs=verbs3[verbs3$conj=="ere", ]

# 3rd conjugation ire
ire_verbs=verbs3[verbs3$conj=="ire", ]

# rre_verbs - think about it 
rre_verbs=verbs3[verbs3$conj=="rre", ]



##
#
#
#     1st conjugation
#                   " -are "
#
##

#
# split cells 
#

#
# 1st conjugation
# present indicative 1 ps
#

ARE_indpres_1s=are_verbs[are_verbs$POS=="VER:ind+pres+1+s", ]

ARE_indpres_1s$fonroot=str_sub(ARE_indpres_1s$fonform, 1, -2)
ARE_indpres_1s$fonsuff=str_sub(ARE_indpres_1s$fonform, -1, -1)

unique(ARE_indpres_1s$fonsuff)

# merge with root of present infinitive
ARE_indpres_1s=merge(ARE_indpres_1s, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit",  all.x = F)

# does root end in palatal
ARE_indpres_1s$endroot=str_sub(ARE_indpres_1s$fonroot, -2, -1)
ARE_indpres_1s$pal_end=ifelse(ARE_indpres_1s$endroot=="tS"|
                                ARE_indpres_1s$endroot=="dG"|
                                ARE_indpres_1s$endroot=="NN"|
                                ARE_indpres_1s$endroot=="LL"|
                                grepl("k$",ARE_indpres_1s$endroot)|
                                grepl("g$",ARE_indpres_1s$endroot)|
                                ARE_indpres_1s$endroot=="lg"|
                                ARE_indpres_1s$endroot=="ng",
                              ARE_indpres_1s$endroot, "-")
unique(ARE_indpres_1s$pal_end)
ARE_indpres_1s$pal_end=ifelse(ARE_indpres_1s$pal_end=="tS"|
                                ARE_indpres_1s$pal_end=="dG"|
                                ARE_indpres_1s$pal_end=="NN"|
                                ARE_indpres_1s$pal_end=="LL"|
                                ARE_indpres_1s$pal_end=="lg"|
                                ARE_indpres_1s$pal_end=="ng",
                              ARE_indpres_1s$pal_end,
                              str_sub(ARE_indpres_1s$pal_end, -1, -1)
)


# assign columns for next merging 
ARE_indpres_1s$form_1ps_ind=ARE_indpres_1s$form
ARE_indpres_1s$fonform_1ps_ind=ARE_indpres_1s$fonform
ARE_indpres_1s$fonroot_1ps_ind=ARE_indpres_1s$fonroot
ARE_indpres_1s$fonsuff_1ps_ind=ARE_indpres_1s$fonsuff
ARE_indpres_1s$pal_end_1ps_ind=ARE_indpres_1s$pal_end
#ARE_indpres_1s$formfreq_1ps_ind=ARE_indpres_1s$Freq


#
# 1st conjugation
# present indicative 3 ps
#

ARE_indpres_3s=are_verbs[are_verbs$POS=="VER:ind+pres+3+s", ]

ARE_indpres_3s$fonroot=str_sub(ARE_indpres_3s$fonform, 1, -2)
ARE_indpres_3s$fonsuff=str_sub(ARE_indpres_3s$fonform, -1, -1)
unique(ARE_indpres_3s$fonsuff)

ARE_indpres_3s=merge(ARE_indpres_3s, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit",  all.x = F)

# does root end in palatal
ARE_indpres_3s$endroot=str_sub(ARE_indpres_3s$fonroot, -2, -1)
ARE_indpres_3s$pal_end=ifelse(ARE_indpres_3s$endroot=="tS"|
                                ARE_indpres_3s$endroot=="dG"|
                                ARE_indpres_3s$endroot=="NN"|
                                ARE_indpres_3s$endroot=="LL"|
                                grepl("k$",ARE_indpres_3s$endroot)|
                                grepl("g$",ARE_indpres_3s$endroot)|
                                ARE_indpres_3s$endroot=="lg"|
                                ARE_indpres_3s$endroot=="ng",
                              ARE_indpres_3s$endroot, "-")
unique(ARE_indpres_3s$pal_end)
ARE_indpres_3s$pal_end=ifelse(ARE_indpres_3s$pal_end=="tS"|
                                ARE_indpres_3s$pal_end=="dG"|
                                ARE_indpres_3s$pal_end=="NN"|
                                ARE_indpres_3s$pal_end=="LL"|
                                ARE_indpres_3s$pal_end=="lg"|
                                ARE_indpres_3s$pal_end=="ng",
                              ARE_indpres_3s$pal_end,
                              str_sub(ARE_indpres_3s$pal_end, -1, -1)
)

# assign columns
ARE_indpres_3s$form_3ps_ind=ARE_indpres_3s$form
ARE_indpres_3s$fonform_3ps_ind=ARE_indpres_3s$fonform
ARE_indpres_3s$fonroot_3ps_ind=ARE_indpres_3s$fonroot
ARE_indpres_3s$pal_end_3ps_ind=ARE_indpres_3s$pal_end
#ARE_indpres_3s$formfreq_3ps_ind=ARE_indpres_3s$Freq


#
# 1st conjugation
# present indicative 1 pp
#

ARE_indpres_1p=are_verbs[are_verbs$POS=="VER:ind+pres+1+p", ]

ARE_indpres_1p=merge(ARE_indpres_1p, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit", all.x = F)

# strip morpheme 
ARE_indpres_1p$fonsuff= stringr::str_replace(ARE_indpres_1p$fonform, ARE_indpres_1p$inf_root, "")
ARE_indpres_1p$fonroot= stringr::str_replace(ARE_indpres_1p$fonform, ARE_indpres_1p$fonsuff, "")

# does root end in palatal
ARE_indpres_1p$endroot=str_sub(ARE_indpres_1p$fonroot, -2, -1)
ARE_indpres_1p$pal_end=ifelse(ARE_indpres_1p$endroot=="tS"|
                                ARE_indpres_1p$endroot=="dG"|
                                ARE_indpres_1p$endroot=="NN"|
                                ARE_indpres_1p$endroot=="LL"|
                                grepl("k$",ARE_indpres_1p$endroot)|
                                grepl("g$",ARE_indpres_1p$endroot)|
                                ARE_indpres_1p$endroot=="lg"|
                                ARE_indpres_1p$endroot=="ng",
                              ARE_indpres_1p$endroot, "-")
unique(ARE_indpres_1p$pal_end)
ARE_indpres_1p$pal_end=ifelse(ARE_indpres_1p$pal_end=="tS"|
                                ARE_indpres_1p$pal_end=="dG"|
                                ARE_indpres_1p$pal_end=="NN"|
                                ARE_indpres_1p$pal_end=="LL"|
                                ARE_indpres_1p$pal_end=="lg"|
                                ARE_indpres_1p$pal_end=="ng",
                              ARE_indpres_1p$pal_end,
                              str_sub(ARE_indpres_1p$pal_end, -1, -1)
)

# assign columns
ARE_indpres_1p$form_1pp_ind=ARE_indpres_1p$form
ARE_indpres_1p$fonform_1pp_ind=ARE_indpres_1p$fonform
ARE_indpres_1p$fonroot_1pp_ind=ARE_indpres_1p$fonroot
ARE_indpres_1p$pal_end_1pp_ind=ARE_indpres_1p$pal_end
#ARE_indpres_1p$formfreq_1pp_ind=ARE_indpres_1p$Freq


#
# 1st conjugation
# present subjunctive 1 ps
#

ARE_subjpres_1s=are_verbs[are_verbs$POS=="VER:sub+pres+1+s", ]
unique(ARE_subjpres_1s$fonsuff)


ARE_subjpres_1s$fonroot=str_sub(ARE_subjpres_1s$fonform, 1, -2)
ARE_subjpres_1s$fonsuff=str_sub(ARE_subjpres_1s$fonform, -1, -1)
unique(ARE_subjpres_1s$fonsuff)


# does root end in palatal
ARE_subjpres_1s$endroot=str_sub(ARE_subjpres_1s$fonroot, -2, -1)
ARE_subjpres_1s$pal_end=ifelse(ARE_subjpres_1s$endroot=="tS"|
                                 ARE_subjpres_1s$endroot=="dG"|
                                 ARE_subjpres_1s$endroot=="NN"|
                                 ARE_subjpres_1s$endroot=="LL"|
                                 grepl("k$",ARE_subjpres_1s$endroot)|
                                 grepl("g$",ARE_subjpres_1s$endroot)|
                                 ARE_subjpres_1s$endroot=="lg"|
                                 ARE_subjpres_1s$endroot=="ng",
                               ARE_subjpres_1s$endroot, "-")
unique(ARE_subjpres_1s$pal_end)
ARE_subjpres_1s$pal_end=ifelse(ARE_subjpres_1s$pal_end=="tS"|
                                 ARE_subjpres_1s$pal_end=="dG"|
                                 ARE_subjpres_1s$pal_end=="NN"|
                                 ARE_subjpres_1s$pal_end=="LL"|
                                 ARE_subjpres_1s$pal_end=="lg"|
                                 ARE_subjpres_1s$pal_end=="ng",
                               ARE_subjpres_1s$pal_end,
                               str_sub(ARE_subjpres_1s$pal_end, -1, -1)
)


# assign columns
ARE_subjpres_1s$form_1ps_sub=ARE_subjpres_1s$form
ARE_subjpres_1s$fonform_1ps_sub=ARE_subjpres_1s$fonform
ARE_subjpres_1s$fonroot_1ps_sub=ARE_subjpres_1s$fonroot
ARE_subjpres_1s$pal_end_1ps_sub=ARE_subjpres_1s$pal_end
#ARE_subjpres_1s$formfreq_1ps_sub=ARE_subjpres_1s$Freq


# merge indicative singular forms
ARE_verbs_morph0=merge(ARE_indpres_1s[, c("form_1ps_ind", #"formfreq_1ps_ind", 
                                          "fonform_1ps_ind", "fonroot_1ps_ind", "pal_end_1ps_ind", "lemma_morphit", "conj", "inf_root", "pal_end_inf")], 
                       ARE_indpres_3s[ , c("form_3ps_ind", #"formfreq_3ps_ind", 
                                           "fonform_3ps_ind", "fonroot_3ps_ind", "pal_end_3ps_ind",
                                           "lemma_morphit")], 
                       by.x="lemma_morphit", 
                       by.y="lemma_morphit", 
                       all.x = F)

ARE_verbs_morph1=merge(ARE_verbs_morph0, 
                       ARE_indpres_1p[ , c("form_1pp_ind",# "formfreq_1pp_ind", 
                                           "fonform_1pp_ind", "fonroot_1pp_ind", "pal_end_1pp_ind",
                                           "lemma_morphit")], 
                       by.x="lemma_morphit", 
                       by.y="lemma_morphit", 
                       all.x = F)

# merge subjunctive forms
ARE_verbs_morph=merge(ARE_verbs_morph1, 
                      ARE_subjpres_1s[ , c("form_1ps_sub", #"formfreq_1ps_sub", 
                                           "fonform_1ps_sub", "fonroot_1ps_sub", "pal_end_1ps_sub", 
                                           "lemma_morphit")], 
                      by.x="lemma_morphit", 
                      by.y="lemma_morphit", 
                      all.x = F)

# L-morphome
ARE_verbs_morph$L_morph=ifelse(ARE_verbs_morph$fonroot_1ps_ind==ARE_verbs_morph$fonroot_1ps_sub 
                               &!ARE_verbs_morph$fonroot_1ps_ind==ARE_verbs_morph$fonroot_3ps_ind
                               &!ARE_verbs_morph$fonroot_1ps_ind==ARE_verbs_morph$fonroot_1pp_ind,
                               1, 0)


# is root of indicative 1 pers singular == the root of infinitive 
ARE_verbs_morph$indpres_1s_eq_inf= ifelse(ARE_verbs_morph$fonroot_1ps_ind==ARE_verbs_morph$inf_root, 1, 0)

summary(ARE_verbs_morph$indpres_1s_eq_inf)
ARE_verbs_morph[ARE_verbs_morph$indpres_1s_eq_inf==0, ]

# drop duplicates
summary(duplicated(ARE_verbs_morph))
ARE_verbs_morph$dup=duplicated(ARE_verbs_morph)

ARE_verbs_morph=ARE_verbs_morph[ARE_verbs_morph$dup==F, ]
ARE_verbs_morph$dup=NULL


##
#
#
#     2nd conjugation
#                   " -ere "
#
##

#
# split cells 
#

#
# 2nd conjugation
# present indicative 1 ps
#

ERE_indpres_1s=ere_verbs[ere_verbs$POS=="VER:ind+pres+1+s", ]

ERE_indpres_1s$fonroot=str_sub(ERE_indpres_1s$fonform, 1, -2)
ERE_indpres_1s$fonsuff=str_sub(ERE_indpres_1s$fonform, -1, -1)

unique(ERE_indpres_1s$fonsuff)

# merge with root of present infinitive
ERE_indpres_1s=merge(ERE_indpres_1s, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit",  all.x = F)

# does root end in palatal
ERE_indpres_1s$endroot=str_sub(ERE_indpres_1s$fonroot, -2, -1)
ERE_indpres_1s$pal_end=ifelse(ERE_indpres_1s$endroot=="tS"|
                                ERE_indpres_1s$endroot=="dG"|
                                ERE_indpres_1s$endroot=="NN"|
                                ERE_indpres_1s$endroot=="LL"|
                                grepl("k$",ERE_indpres_1s$endroot)|
                                grepl("g$",ERE_indpres_1s$endroot)|
                                ERE_indpres_1s$endroot=="lg"|
                                ERE_indpres_1s$endroot=="ng",
                              ERE_indpres_1s$endroot, "-")
unique(ERE_indpres_1s$pal_end)
ERE_indpres_1s$pal_end=ifelse(ERE_indpres_1s$pal_end=="tS"|
                                ERE_indpres_1s$pal_end=="dG"|
                                ERE_indpres_1s$pal_end=="NN"|
                                ERE_indpres_1s$pal_end=="LL"|
                                ERE_indpres_1s$pal_end=="lg"|
                                ERE_indpres_1s$pal_end=="ng",
                              ERE_indpres_1s$pal_end,
                              str_sub(ERE_indpres_1s$pal_end, -1, -1)
)


# assign columns for next merging 
ERE_indpres_1s$form_1ps_ind=ERE_indpres_1s$form
ERE_indpres_1s$fonform_1ps_ind=ERE_indpres_1s$fonform
ERE_indpres_1s$fonroot_1ps_ind=ERE_indpres_1s$fonroot
ERE_indpres_1s$fonsuff_1ps_ind=ERE_indpres_1s$fonsuff
ERE_indpres_1s$pal_end_1ps_ind=ERE_indpres_1s$pal_end
#ERE_indpres_1s$formfreq_1ps_ind=ERE_indpres_1s$Freq


#
# 2nd conjugation
# present indicative 3 ps
#

ERE_indpres_3s=ere_verbs[ere_verbs$POS=="VER:ind+pres+3+s", ]

ERE_indpres_3s$fonroot=str_sub(ERE_indpres_3s$fonform, 1, -2)
ERE_indpres_3s$fonsuff=str_sub(ERE_indpres_3s$fonform, -1, -1)
unique(ERE_indpres_3s$fonsuff)

ERE_indpres_3s=merge(ERE_indpres_3s, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit",  all.x = F)

# does root end in palatal
ERE_indpres_3s$endroot=str_sub(ERE_indpres_3s$fonroot, -2, -1)
ERE_indpres_3s$pal_end=ifelse(ERE_indpres_3s$endroot=="tS"|
                                ERE_indpres_3s$endroot=="dG"|
                                ERE_indpres_3s$endroot=="NN"|
                                ERE_indpres_3s$endroot=="LL"|
                                grepl("k$",ERE_indpres_3s$endroot)|
                                grepl("g$",ERE_indpres_3s$endroot)|
                                ERE_indpres_3s$endroot=="lg"|
                                ERE_indpres_3s$endroot=="ng",
                              ERE_indpres_3s$endroot, "-")
unique(ERE_indpres_3s$pal_end)
ERE_indpres_3s$pal_end=ifelse(ERE_indpres_3s$pal_end=="tS"|
                                ERE_indpres_3s$pal_end=="dG"|
                                ERE_indpres_3s$pal_end=="NN"|
                                ERE_indpres_3s$pal_end=="LL"|
                                ERE_indpres_3s$pal_end=="lg"|
                                ERE_indpres_3s$pal_end=="ng",
                              ERE_indpres_3s$pal_end,
                              str_sub(ERE_indpres_3s$pal_end, -1, -1)
)

# assign columns
ERE_indpres_3s$form_3ps_ind=ERE_indpres_3s$form
ERE_indpres_3s$fonform_3ps_ind=ERE_indpres_3s$fonform
ERE_indpres_3s$fonroot_3ps_ind=ERE_indpres_3s$fonroot
ERE_indpres_3s$pal_end_3ps_ind=ERE_indpres_3s$pal_end
#ERE_indpres_3s$formfreq_3ps_ind=ERE_indpres_3s$Freq


#
# 2nd conjugation
# present indicative 1 pp
#

ERE_indpres_1p=ere_verbs[ere_verbs$POS=="VER:ind+pres+1+p", ]

ERE_indpres_1p=merge(ERE_indpres_1p, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit", all.x = F)

# strip morpheme 
ERE_indpres_1p$fonsuff= stringr::str_replace(ERE_indpres_1p$fonform, ERE_indpres_1p$inf_root, "")
ERE_indpres_1p$fonroot= stringr::str_replace(ERE_indpres_1p$fonform, ERE_indpres_1p$fonsuff, "")

# does root end in palatal
ERE_indpres_1p$endroot=str_sub(ERE_indpres_1p$fonroot, -2, -1)
ERE_indpres_1p$pal_end=ifelse(ERE_indpres_1p$endroot=="tS"|
                                ERE_indpres_1p$endroot=="dG"|
                                ERE_indpres_1p$endroot=="NN"|
                                ERE_indpres_1p$endroot=="LL"|
                                grepl("k$",ERE_indpres_1p$endroot)|
                                grepl("g$",ERE_indpres_1p$endroot)|
                                ERE_indpres_1p$endroot=="lg"|
                                ERE_indpres_1p$endroot=="ng",
                              ERE_indpres_1p$endroot, "-")
unique(ERE_indpres_1p$pal_end)
ERE_indpres_1p$pal_end=ifelse(ERE_indpres_1p$pal_end=="tS"|
                                ERE_indpres_1p$pal_end=="dG"|
                                ERE_indpres_1p$pal_end=="NN"|
                                ERE_indpres_1p$pal_end=="LL"|
                                ERE_indpres_1p$pal_end=="lg"|
                                ERE_indpres_1p$pal_end=="ng",
                              ERE_indpres_1p$pal_end,
                              str_sub(ERE_indpres_1p$pal_end, -1, -1)
)

# assign columns
ERE_indpres_1p$form_1pp_ind=ERE_indpres_1p$form
ERE_indpres_1p$fonform_1pp_ind=ERE_indpres_1p$fonform
ERE_indpres_1p$fonroot_1pp_ind=ERE_indpres_1p$fonroot
ERE_indpres_1p$pal_end_1pp_ind=ERE_indpres_1p$pal_end
#ERE_indpres_1p$formfreq_1pp_ind=ERE_indpres_1p$Freq


#
# 2nd conjugation
# present subjunctive 1 ps
#

ERE_subjpres_1s=ere_verbs[ere_verbs$POS=="VER:sub+pres+1+s", ]
unique(ERE_subjpres_1s$fonsuff)


ERE_subjpres_1s$fonroot=str_sub(ERE_subjpres_1s$fonform, 1, -2)
ERE_subjpres_1s$fonsuff=str_sub(ERE_subjpres_1s$fonform, -1, -1)
unique(ERE_subjpres_1s$fonsuff)


# does root end in palatal
ERE_subjpres_1s$endroot=str_sub(ERE_subjpres_1s$fonroot, -2, -1)
ERE_subjpres_1s$pal_end=ifelse(ERE_subjpres_1s$endroot=="tS"|
                                 ERE_subjpres_1s$endroot=="dG"|
                                 ERE_subjpres_1s$endroot=="NN"|
                                 ERE_subjpres_1s$endroot=="LL"|
                                 grepl("k$",ERE_subjpres_1s$endroot)|
                                 grepl("g$",ERE_subjpres_1s$endroot)|
                                 ERE_subjpres_1s$endroot=="lg"|
                                 ERE_subjpres_1s$endroot=="ng",
                               ERE_subjpres_1s$endroot, "-")
unique(ERE_subjpres_1s$pal_end)
ERE_subjpres_1s$pal_end=ifelse(ERE_subjpres_1s$pal_end=="tS"|
                                 ERE_subjpres_1s$pal_end=="dG"|
                                 ERE_subjpres_1s$pal_end=="NN"|
                                 ERE_subjpres_1s$pal_end=="LL"|
                                 ERE_subjpres_1s$pal_end=="lg"|
                                 ERE_subjpres_1s$pal_end=="ng",
                               ERE_subjpres_1s$pal_end,
                               str_sub(ERE_subjpres_1s$pal_end, -1, -1)
)


# assign columns
ERE_subjpres_1s$form_1ps_sub=ERE_subjpres_1s$form
ERE_subjpres_1s$fonform_1ps_sub=ERE_subjpres_1s$fonform
ERE_subjpres_1s$fonroot_1ps_sub=ERE_subjpres_1s$fonroot
ERE_subjpres_1s$pal_end_1ps_sub=ERE_subjpres_1s$pal_end
#ERE_subjpres_1s$formfreq_1ps_sub=ERE_subjpres_1s$Freq


# merge indicative singular forms
ERE_verbs_morph0=merge(ERE_indpres_1s[, c("form_1ps_ind",# "formfreq_1ps_ind", 
                                          "fonform_1ps_ind", "fonroot_1ps_ind", "pal_end_1ps_ind", "lemma_morphit", "conj", "inf_root", "pal_end_inf")], 
                       ERE_indpres_3s[ , c("form_3ps_ind",# "formfreq_3ps_ind", 
                                           "fonform_3ps_ind", "fonroot_3ps_ind", "pal_end_3ps_ind",
                                           "lemma_morphit")], 
                       by.x="lemma_morphit", 
                       by.y="lemma_morphit", 
                       all.x = F)

ERE_verbs_morph1=merge(ERE_verbs_morph0, 
                       ERE_indpres_1p[ , c("form_1pp_ind",# "formfreq_1pp_ind", 
                                           "fonform_1pp_ind", "fonroot_1pp_ind", "pal_end_1pp_ind",
                                           "lemma_morphit")], 
                       by.x="lemma_morphit", 
                       by.y="lemma_morphit", 
                       all.x = F)

# merge subjunctive forms
ERE_verbs_morph=merge(ERE_verbs_morph1, 
                      ERE_subjpres_1s[ , c("form_1ps_sub", #"formfreq_1ps_sub", 
                                           "fonform_1ps_sub", "fonroot_1ps_sub", "pal_end_1ps_sub", 
                                           "lemma_morphit")], 
                      by.x="lemma_morphit", 
                      by.y="lemma_morphit", 
                      all.x = F)

# L-morphome
ERE_verbs_morph$L_morph=ifelse(ERE_verbs_morph$fonroot_1ps_ind==ERE_verbs_morph$fonroot_1ps_sub 
                               &!ERE_verbs_morph$fonroot_1ps_ind==ERE_verbs_morph$fonroot_3ps_ind
                               &!ERE_verbs_morph$fonroot_1ps_ind==ERE_verbs_morph$fonroot_1pp_ind,
                               1, 0)


# is root of indicative 1 pers singular == the root of infinitive 
ERE_verbs_morph$indpres_1s_eq_inf= ifelse(ERE_verbs_morph$fonroot_1ps_ind==ERE_verbs_morph$inf_root, 1, 0)

summary(ERE_verbs_morph$indpres_1s_eq_inf)
ERE_verbs_morph[ERE_verbs_morph$indpres_1s_eq_inf==0, ]

# drop duplicates
summary(duplicated(ERE_verbs_morph))
ERE_verbs_morph$dup=duplicated(ERE_verbs_morph)

ERE_verbs_morph=ERE_verbs_morph[ERE_verbs_morph$dup==F, ]
ERE_verbs_morph$dup=NULL


##
#
#
#     3rd conjugation
#                   " -ire "
#
##

#
# split cells 
#

#
# 3rd conjugation
# present indicative 1 ps
#

IRE_indpres_1s=ire_verbs[ire_verbs$POS=="VER:ind+pres+1+s", ]

IRE_indpres_1s$fonroot=str_sub(IRE_indpres_1s$fonform, 1, -2)

IRE_indpres_1s$fonroot_noinc_1ps_ind=stringr::str_replace_all(IRE_indpres_1s$fonroot, "isk$", "")

str_sub(IRE_indpres_1s$fonform, 1, -2)

IRE_indpres_1s$fonsuff=str_sub(IRE_indpres_1s$fonform, -1, -1)

unique(IRE_indpres_1s$fonsuff)

# merge with root of present infinitive
IRE_indpres_1s=merge(IRE_indpres_1s, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit",  all.x = F)

# does root end in palatal
IRE_indpres_1s$endroot=str_sub(IRE_indpres_1s$fonroot, -2, -1)
IRE_indpres_1s$pal_end=ifelse(IRE_indpres_1s$endroot=="tS"|
                                IRE_indpres_1s$endroot=="dG"|
                                IRE_indpres_1s$endroot=="NN"|
                                IRE_indpres_1s$endroot=="LL"|
                                grepl("k$",IRE_indpres_1s$endroot)|
                                grepl("g$",IRE_indpres_1s$endroot)|
                                IRE_indpres_1s$endroot=="lg"|
                                IRE_indpres_1s$endroot=="ng",
                              IRE_indpres_1s$endroot, "-")
unique(IRE_indpres_1s$pal_end)
IRE_indpres_1s$pal_end=ifelse(IRE_indpres_1s$pal_end=="tS"|
                                IRE_indpres_1s$pal_end=="dG"|
                                IRE_indpres_1s$pal_end=="NN"|
                                IRE_indpres_1s$pal_end=="LL"|
                                IRE_indpres_1s$pal_end=="lg"|
                                IRE_indpres_1s$pal_end=="ng",
                              IRE_indpres_1s$pal_end,
                              str_sub(IRE_indpres_1s$pal_end, -1, -1)
)


# assign columns for next merging 
IRE_indpres_1s$form_1ps_ind=IRE_indpres_1s$form
IRE_indpres_1s$fonform_1ps_ind=IRE_indpres_1s$fonform
IRE_indpres_1s$fonroot_1ps_ind=IRE_indpres_1s$fonroot
IRE_indpres_1s$fonsuff_1ps_ind=IRE_indpres_1s$fonsuff
IRE_indpres_1s$pal_end_1ps_ind=IRE_indpres_1s$pal_end
#IRE_indpres_1s$formfreq_1ps_ind=IRE_indpres_1s$Freq


#
# 3rd conjugation
# present indicative 3 ps
#

IRE_indpres_3s=ire_verbs[ire_verbs$POS=="VER:ind+pres+3+s", ]

IRE_indpres_3s$fonroot=str_sub(IRE_indpres_3s$fonform, 1, -2)
IRE_indpres_3s$fonsuff=str_sub(IRE_indpres_3s$fonform, -1, -1)
unique(IRE_indpres_3s$fonsuff)

IRE_indpres_3s=merge(IRE_indpres_3s, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit",  all.x = F)

# does root end in palatal
IRE_indpres_3s$endroot=str_sub(IRE_indpres_3s$fonroot, -2, -1)
IRE_indpres_3s$pal_end=ifelse(IRE_indpres_3s$endroot=="tS"|
                                IRE_indpres_3s$endroot=="dG"|
                                IRE_indpres_3s$endroot=="NN"|
                                IRE_indpres_3s$endroot=="LL"|
                                grepl("k$",IRE_indpres_3s$endroot)|
                                grepl("g$",IRE_indpres_3s$endroot)|
                                IRE_indpres_3s$endroot=="lg"|
                                IRE_indpres_3s$endroot=="ng",
                              IRE_indpres_3s$endroot, "-")
unique(IRE_indpres_3s$pal_end)
IRE_indpres_3s$pal_end=ifelse(IRE_indpres_3s$pal_end=="tS"|
                                IRE_indpres_3s$pal_end=="dG"|
                                IRE_indpres_3s$pal_end=="NN"|
                                IRE_indpres_3s$pal_end=="LL"|
                                IRE_indpres_3s$pal_end=="lg"|
                                IRE_indpres_3s$pal_end=="ng",
                              IRE_indpres_3s$pal_end,
                              str_sub(IRE_indpres_3s$pal_end, -1, -1)
)

# assign columns
IRE_indpres_3s$form_3ps_ind=IRE_indpres_3s$form
IRE_indpres_3s$fonform_3ps_ind=IRE_indpres_3s$fonform
IRE_indpres_3s$fonroot_3ps_ind=IRE_indpres_3s$fonroot
IRE_indpres_3s$pal_end_3ps_ind=IRE_indpres_3s$pal_end
#IRE_indpres_3s$formfreq_3ps_ind=IRE_indpres_3s$Freq


#
# 3rd conjugation
# present indicative 1 pp
#

IRE_indpres_1p=ire_verbs[ire_verbs$POS=="VER:ind+pres+1+p", ]

IRE_indpres_1p=merge(IRE_indpres_1p, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit", all.x = F)

# strip morpheme 
IRE_indpres_1p$fonsuff= stringr::str_replace(IRE_indpres_1p$fonform, IRE_indpres_1p$inf_root, "")
IRE_indpres_1p$fonroot= stringr::str_replace(IRE_indpres_1p$fonform, IRE_indpres_1p$fonsuff, "")

# does root end in palatal
IRE_indpres_1p$endroot=str_sub(IRE_indpres_1p$fonroot, -2, -1)
IRE_indpres_1p$pal_end=ifelse(IRE_indpres_1p$endroot=="tS"|
                                IRE_indpres_1p$endroot=="dG"|
                                IRE_indpres_1p$endroot=="NN"|
                                IRE_indpres_1p$endroot=="LL"|
                                grepl("k$",IRE_indpres_1p$endroot)|
                                grepl("g$",IRE_indpres_1p$endroot)|
                                IRE_indpres_1p$endroot=="lg"|
                                IRE_indpres_1p$endroot=="ng",
                              IRE_indpres_1p$endroot, "-")
unique(IRE_indpres_1p$pal_end)
IRE_indpres_1p$pal_end=ifelse(IRE_indpres_1p$pal_end=="tS"|
                                IRE_indpres_1p$pal_end=="dG"|
                                IRE_indpres_1p$pal_end=="NN"|
                                IRE_indpres_1p$pal_end=="LL"|
                                IRE_indpres_1p$pal_end=="lg"|
                                IRE_indpres_1p$pal_end=="ng",
                              IRE_indpres_1p$pal_end,
                              str_sub(IRE_indpres_1p$pal_end, -1, -1)
)

# assign columns
IRE_indpres_1p$form_1pp_ind=IRE_indpres_1p$form
IRE_indpres_1p$fonform_1pp_ind=IRE_indpres_1p$fonform
IRE_indpres_1p$fonroot_1pp_ind=IRE_indpres_1p$fonroot
IRE_indpres_1p$pal_end_1pp_ind=IRE_indpres_1p$pal_end
#IRE_indpres_1p$formfreq_1pp_ind=IRE_indpres_1p$Freq


#
# 3rd conjugation
# present subjunctive 1 ps
#

IRE_subjpres_1s=ire_verbs[ire_verbs$POS=="VER:sub+pres+1+s", ]
unique(IRE_subjpres_1s$fonsuff)


IRE_subjpres_1s$fonroot=str_sub(IRE_subjpres_1s$fonform, 1, -2)
IRE_subjpres_1s$fonsuff=str_sub(IRE_subjpres_1s$fonform, -1, -1)
unique(IRE_subjpres_1s$fonsuff)


# does root end in palatal
IRE_subjpres_1s$endroot=str_sub(IRE_subjpres_1s$fonroot, -2, -1)
IRE_subjpres_1s$pal_end=ifelse(IRE_subjpres_1s$endroot=="tS"|
                                 IRE_subjpres_1s$endroot=="dG"|
                                 IRE_subjpres_1s$endroot=="NN"|
                                 IRE_subjpres_1s$endroot=="LL"|
                                 grepl("k$",IRE_subjpres_1s$endroot)|
                                 grepl("g$",IRE_subjpres_1s$endroot)|
                                 IRE_subjpres_1s$endroot=="lg"|
                                 IRE_subjpres_1s$endroot=="ng",
                               IRE_subjpres_1s$endroot, "-")
unique(IRE_subjpres_1s$pal_end)
IRE_subjpres_1s$pal_end=ifelse(IRE_subjpres_1s$pal_end=="tS"|
                                 IRE_subjpres_1s$pal_end=="dG"|
                                 IRE_subjpres_1s$pal_end=="NN"|
                                 IRE_subjpres_1s$pal_end=="LL"|
                                 IRE_subjpres_1s$pal_end=="lg"|
                                 IRE_subjpres_1s$pal_end=="ng",
                               IRE_subjpres_1s$pal_end,
                               str_sub(IRE_subjpres_1s$pal_end, -1, -1)
)


# assign columns
IRE_subjpres_1s$form_1ps_sub=IRE_subjpres_1s$form
IRE_subjpres_1s$fonform_1ps_sub=IRE_subjpres_1s$fonform
IRE_subjpres_1s$fonroot_1ps_sub=IRE_subjpres_1s$fonroot
IRE_subjpres_1s$pal_end_1ps_sub=IRE_subjpres_1s$pal_end
#IRE_subjpres_1s$formfreq_1ps_sub=IRE_subjpres_1s$Freq


# merge indicative singular forms
IRE_verbs_morph0=merge(IRE_indpres_1s[, c("form_1ps_ind", #"formfreq_1ps_ind", 
                                          "fonform_1ps_ind", "fonroot_1ps_ind", "fonroot_noinc_1ps_ind", "pal_end_1ps_ind", "lemma_morphit", "conj", "inf_root", "pal_end_inf")], 
                       IRE_indpres_3s[ , c("form_3ps_ind", #"formfreq_3ps_ind", 
                                           "fonform_3ps_ind", "fonroot_3ps_ind", "pal_end_3ps_ind",
                                           "lemma_morphit")], 
                       by.x="lemma_morphit", 
                       by.y="lemma_morphit", 
                       all.x = F)

IRE_verbs_morph1=merge(IRE_verbs_morph0, 
                       IRE_indpres_1p[ , c("form_1pp_ind", #"formfreq_1pp_ind", 
                                           "fonform_1pp_ind", "fonroot_1pp_ind", "pal_end_1pp_ind",
                                           "lemma_morphit")], 
                       by.x="lemma_morphit", 
                       by.y="lemma_morphit", 
                       all.x = F)

# merge subjunctive forms
IRE_verbs_morph=merge(IRE_verbs_morph1, 
                      IRE_subjpres_1s[ , c("form_1ps_sub", #"formfreq_1ps_sub", 
                                           "fonform_1ps_sub", "fonroot_1ps_sub", "pal_end_1ps_sub", 
                                           "lemma_morphit")], 
                      by.x="lemma_morphit", 
                      by.y="lemma_morphit", 
                      all.x = F)

# L-morphome
IRE_verbs_morph$L_morph=ifelse(IRE_verbs_morph$fonroot_1ps_ind==IRE_verbs_morph$fonroot_1ps_sub 
                               &!IRE_verbs_morph$fonroot_1ps_ind==IRE_verbs_morph$fonroot_3ps_ind
                               # &!IRE_verbs_morph$fonroot_noinc_1ps_ind==IRE_verbs_morph$fonroot_1pp_ind
                               , 1, 0)


# is root of indicative 1 pers singular == the root of infinitive 
IRE_verbs_morph$indpres_1s_eq_inf= ifelse(IRE_verbs_morph$fonroot_1ps_ind==IRE_verbs_morph$inf_root, 1, 0)

summary(IRE_verbs_morph$indpres_1s_eq_inf)
IRE_verbs_morph[IRE_verbs_morph$indpres_1s_eq_inf==0, ]

# drop duplicates
summary(duplicated(IRE_verbs_morph))
IRE_verbs_morph$dup=duplicated(IRE_verbs_morph)

IRE_verbs_morph=IRE_verbs_morph[IRE_verbs_morph$dup==F, ]
IRE_verbs_morph$dup=NULL


##
#
#      verbs in 
#              "-rre"     
#
##

#
# split cells 
#

#
# verbs in -rre
# present indicative 1 ps
#

RRE_indpres_1s=rre_verbs[rre_verbs$POS=="VER:ind+pres+1+s", ]

RRE_indpres_1s$fonroot=str_sub(RRE_indpres_1s$fonform, 1, -2)
RRE_indpres_1s$fonsuff=str_sub(RRE_indpres_1s$fonform, -1, -1)

unique(RRE_indpres_1s$fonsuff)

# merge with root of present infinitive
RRE_indpres_1s=merge(RRE_indpres_1s, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit",  all.x = F)

# does root end in palatal
RRE_indpres_1s$endroot=str_sub(RRE_indpres_1s$fonroot, -2, -1)
RRE_indpres_1s$pal_end=ifelse(RRE_indpres_1s$endroot=="tS"|
                                RRE_indpres_1s$endroot=="dG"|
                                RRE_indpres_1s$endroot=="NN"|
                                RRE_indpres_1s$endroot=="LL"|
                                grepl("k$",RRE_indpres_1s$endroot)|
                                grepl("g$",RRE_indpres_1s$endroot)|
                                RRE_indpres_1s$endroot=="lg"|
                                RRE_indpres_1s$endroot=="ng",
                              RRE_indpres_1s$endroot, "-")
unique(RRE_indpres_1s$pal_end)
RRE_indpres_1s$pal_end=ifelse(RRE_indpres_1s$pal_end=="tS"|
                                RRE_indpres_1s$pal_end=="dG"|
                                RRE_indpres_1s$pal_end=="NN"|
                                RRE_indpres_1s$pal_end=="LL"|
                                RRE_indpres_1s$pal_end=="lg"|
                                RRE_indpres_1s$pal_end=="ng",
                              RRE_indpres_1s$pal_end,
                              str_sub(RRE_indpres_1s$pal_end, -1, -1)
)


# assign columns for next merging 
RRE_indpres_1s$form_1ps_ind=RRE_indpres_1s$form
RRE_indpres_1s$fonform_1ps_ind=RRE_indpres_1s$fonform
RRE_indpres_1s$fonroot_1ps_ind=RRE_indpres_1s$fonroot
RRE_indpres_1s$fonsuff_1ps_ind=RRE_indpres_1s$fonsuff
RRE_indpres_1s$pal_end_1ps_ind=RRE_indpres_1s$pal_end
#RRE_indpres_1s$formfreq_1ps_ind=RRE_indpres_1s$Freq


#
# verbs in -rre
# present indicative 3 ps
#

RRE_indpres_3s=rre_verbs[rre_verbs$POS=="VER:ind+pres+3+s", ]

RRE_indpres_3s$fonroot=str_sub(RRE_indpres_3s$fonform, 1, -2)
RRE_indpres_3s$fonsuff=str_sub(RRE_indpres_3s$fonform, -1, -1)
unique(RRE_indpres_3s$fonsuff)

RRE_indpres_3s=merge(RRE_indpres_3s, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit",  all.x = F)

# does root end in palatal
RRE_indpres_3s$endroot=str_sub(RRE_indpres_3s$fonroot, -2, -1)
RRE_indpres_3s$pal_end=ifelse(RRE_indpres_3s$endroot=="tS"|
                                RRE_indpres_3s$endroot=="dG"|
                                RRE_indpres_3s$endroot=="NN"|
                                RRE_indpres_3s$endroot=="LL"|
                                grepl("k$",RRE_indpres_3s$endroot)|
                                grepl("g$",RRE_indpres_3s$endroot)|
                                RRE_indpres_3s$endroot=="lg"|
                                RRE_indpres_3s$endroot=="ng",
                              RRE_indpres_3s$endroot, "-")
unique(RRE_indpres_3s$pal_end)
RRE_indpres_3s$pal_end=ifelse(RRE_indpres_3s$pal_end=="tS"|
                                RRE_indpres_3s$pal_end=="dG"|
                                RRE_indpres_3s$pal_end=="NN"|
                                RRE_indpres_3s$pal_end=="LL"|
                                RRE_indpres_3s$pal_end=="lg"|
                                RRE_indpres_3s$pal_end=="ng",
                              RRE_indpres_3s$pal_end,
                              str_sub(RRE_indpres_3s$pal_end, -1, -1)
)

# assign columns
RRE_indpres_3s$form_3ps_ind=RRE_indpres_3s$form
RRE_indpres_3s$fonform_3ps_ind=RRE_indpres_3s$fonform
RRE_indpres_3s$fonroot_3ps_ind=RRE_indpres_3s$fonroot
RRE_indpres_3s$pal_end_3ps_ind=RRE_indpres_3s$pal_end
#RRE_indpres_3s$formfreq_3ps_ind=RRE_indpres_3s$Freq


#
# verbs in -rre
# present indicative 1 pp
#

RRE_indpres_1p=rre_verbs[rre_verbs$POS=="VER:ind+pres+1+p", ]

RRE_indpres_1p=merge(RRE_indpres_1p, verbs_inf[, c("lemma_morphit", "inf_root", "pal_end_inf")], by.x="lemma_morphit", by.y ="lemma_morphit", all.x = F)

# strip morpheme 
RRE_indpres_1p$fonsuff= stringr::str_replace(RRE_indpres_1p$fonform, RRE_indpres_1p$inf_root, "")
RRE_indpres_1p$fonroot= stringr::str_replace(RRE_indpres_1p$fonform, RRE_indpres_1p$fonsuff, "")

# does root end in palatal
RRE_indpres_1p$endroot=str_sub(RRE_indpres_1p$fonroot, -2, -1)
RRE_indpres_1p$pal_end=ifelse(RRE_indpres_1p$endroot=="tS"|
                                RRE_indpres_1p$endroot=="dG"|
                                RRE_indpres_1p$endroot=="NN"|
                                RRE_indpres_1p$endroot=="LL"|
                                grepl("k$",RRE_indpres_1p$endroot)|
                                grepl("g$",RRE_indpres_1p$endroot)|
                                RRE_indpres_1p$endroot=="lg"|
                                RRE_indpres_1p$endroot=="ng",
                              RRE_indpres_1p$endroot, "-")
unique(RRE_indpres_1p$pal_end)
RRE_indpres_1p$pal_end=ifelse(RRE_indpres_1p$pal_end=="tS"|
                                RRE_indpres_1p$pal_end=="dG"|
                                RRE_indpres_1p$pal_end=="NN"|
                                RRE_indpres_1p$pal_end=="LL"|
                                RRE_indpres_1p$pal_end=="lg"|
                                RRE_indpres_1p$pal_end=="ng",
                              RRE_indpres_1p$pal_end,
                              str_sub(RRE_indpres_1p$pal_end, -1, -1)
)

# assign columns
RRE_indpres_1p$form_1pp_ind=RRE_indpres_1p$form
RRE_indpres_1p$fonform_1pp_ind=RRE_indpres_1p$fonform
RRE_indpres_1p$fonroot_1pp_ind=RRE_indpres_1p$fonroot
RRE_indpres_1p$pal_end_1pp_ind=RRE_indpres_1p$pal_end
#RRE_indpres_1p$formfreq_1pp_ind=RRE_indpres_1p$Freq


#
# verbs in -rre
# present subjunctive 1 ps
#

RRE_subjpres_1s=rre_verbs[rre_verbs$POS=="VER:sub+pres+1+s", ]
unique(RRE_subjpres_1s$fonsuff)


RRE_subjpres_1s$fonroot=str_sub(RRE_subjpres_1s$fonform, 1, -2)
RRE_subjpres_1s$fonsuff=str_sub(RRE_subjpres_1s$fonform, -1, -1)
unique(RRE_subjpres_1s$fonsuff)


# does root end in palatal
RRE_subjpres_1s$endroot=str_sub(RRE_subjpres_1s$fonroot, -2, -1)
RRE_subjpres_1s$pal_end=ifelse(RRE_subjpres_1s$endroot=="tS"|
                                 RRE_subjpres_1s$endroot=="dG"|
                                 RRE_subjpres_1s$endroot=="NN"|
                                 RRE_subjpres_1s$endroot=="LL"|
                                 grepl("k$",RRE_subjpres_1s$endroot)|
                                 grepl("g$",RRE_subjpres_1s$endroot)|
                                 RRE_subjpres_1s$endroot=="lg"|
                                 RRE_subjpres_1s$endroot=="ng",
                               RRE_subjpres_1s$endroot, "-")
unique(RRE_subjpres_1s$pal_end)
RRE_subjpres_1s$pal_end=ifelse(RRE_subjpres_1s$pal_end=="tS"|
                                 RRE_subjpres_1s$pal_end=="dG"|
                                 RRE_subjpres_1s$pal_end=="NN"|
                                 RRE_subjpres_1s$pal_end=="LL"|
                                 RRE_subjpres_1s$pal_end=="lg"|
                                 RRE_subjpres_1s$pal_end=="ng",
                               RRE_subjpres_1s$pal_end,
                               str_sub(RRE_subjpres_1s$pal_end, -1, -1)
)


# assign columns
RRE_subjpres_1s$form_1ps_sub=RRE_subjpres_1s$form
RRE_subjpres_1s$fonform_1ps_sub=RRE_subjpres_1s$fonform
RRE_subjpres_1s$fonroot_1ps_sub=RRE_subjpres_1s$fonroot
RRE_subjpres_1s$pal_end_1ps_sub=RRE_subjpres_1s$pal_end
#RRE_subjpres_1s$formfreq_1ps_sub=RRE_subjpres_1s$Freq


# merge indicative singular forms
RRE_verbs_morph0=merge(RRE_indpres_1s[, c("form_1ps_ind",# "formfreq_1ps_ind", 
                                          "fonform_1ps_ind", "fonroot_1ps_ind", "pal_end_1ps_ind", "lemma_morphit", "conj", "inf_root", "pal_end_inf")], 
                       RRE_indpres_3s[ , c("form_3ps_ind",# "formfreq_3ps_ind", 
                                           "fonform_3ps_ind", "fonroot_3ps_ind", "pal_end_3ps_ind",
                                           "lemma_morphit")], 
                       by.x="lemma_morphit", 
                       by.y="lemma_morphit", 
                       all.x = F)

RRE_verbs_morph1=merge(RRE_verbs_morph0, 
                       RRE_indpres_1p[ , c("form_1pp_ind", #"formfreq_1pp_ind", 
                                           "fonform_1pp_ind", "fonroot_1pp_ind", "pal_end_1pp_ind",
                                           "lemma_morphit")], 
                       by.x="lemma_morphit", 
                       by.y="lemma_morphit", 
                       all.x = F)

# merge subjunctive forms
RRE_verbs_morph=merge(RRE_verbs_morph1, 
                      RRE_subjpres_1s[ , c("form_1ps_sub", #"formfreq_1ps_sub", 
                                           "fonform_1ps_sub", "fonroot_1ps_sub", "pal_end_1ps_sub", 
                                           "lemma_morphit")], 
                      by.x="lemma_morphit", 
                      by.y="lemma_morphit", 
                      all.x = F)

# L-morphome
RRE_verbs_morph$L_morph=ifelse(RRE_verbs_morph$fonroot_1ps_ind==RRE_verbs_morph$fonroot_1ps_sub 
                               &!RRE_verbs_morph$fonroot_1ps_ind==RRE_verbs_morph$fonroot_3ps_ind
                               &!RRE_verbs_morph$fonroot_1ps_ind==RRE_verbs_morph$fonroot_1pp_ind,
                               1, 0)


# is root of indicative 1 pers singular == the root of infinitive 
RRE_verbs_morph$indpres_1s_eq_inf= ifelse(RRE_verbs_morph$fonroot_1ps_ind==RRE_verbs_morph$inf_root, 1, 0)

summary(RRE_verbs_morph$indpres_1s_eq_inf)
RRE_verbs_morph[RRE_verbs_morph$indpres_1s_eq_inf==0, ]

# drop duplicates
summary(duplicated(RRE_verbs_morph))
RRE_verbs_morph$dup=duplicated(RRE_verbs_morph)

RRE_verbs_morph=RRE_verbs_morph[RRE_verbs_morph$dup==F, ]
RRE_verbs_morph$dup=NULL


# set for merging

ARE_verbs_morph$fonroot_noinc_1ps_ind=rep("-")
ERE_verbs_morph$fonroot_noinc_1ps_ind=rep("-")
RRE_verbs_morph$fonroot_noinc_1ps_ind=rep("-")


# clear dupes


all_verbs_morph=rbind(ARE_verbs_morph, ERE_verbs_morph, IRE_verbs_morph, RRE_verbs_morph)  

dim(RRE_verbs_morph)

# output csvs

setwd(outwd)

#write.csv(ARE_verbs_morph, "ARE_verbs_morpho_types.csv")
#write.csv(ERE_verbs_morph, "ERE_verbs_morpho_types.csv")
#write.csv(IRE_verbs_morph, "IRE_verbs_morpho_types.csv")
#write.csv(RRE_verbs_morph, "RRE_verbs_morpho_types.csv")

write.csv(all_verbs_morph, "all_verbs_morpho_types.csv")
