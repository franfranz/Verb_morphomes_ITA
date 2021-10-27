###
#
#     M O R P H O M E    P A T T E R N S 
#
#             +++ italian verbal morphology +++   
#
#      v.3.0.2
#
#     https://github.com/franfranz/Verb_morphomes_ITA
#
### 


#   Description
#   
#
#
#  Description
#  Description
#  The verb_lemma list of word frequencies is available here 


# REFERENCE LEGEND - regular latina graphemes ere used instead of IPA to ensure compatibility
#
# S = voiceless postalveolar fricative 
# tS= voiceless alveloar affricate (with postalveolar solution)
# dG= voiced alveloar affricate (with postalveolar solution)
# N = nasal palatal 
# L = lateral palatal


# clear ws
rm(list=ls())

# filename of the itwac list currently in use
thefilename_itwac="itwac_verbs_lemmas_notail_2_1_0.csv"

# required packages: stringr, pryr, xtable, viridis 


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

## input required: graphical parameters 

# palette in colors - viridis 
# resists grayscale transformation and is good for printing but lighter colors ere not that visible in presentations
col_a = viridisLite::viridis(8)[1]
col_b = viridisLite::viridis(8)[2]
col_c = viridisLite::viridis(8)[6]
col_d = viridisLite::viridis(8)[7]

# palette in greyscale 
# col_a = "#171717" #grey10
# col_b = "#303030" #grey20
# col_c = "#7D7D7D"  #grey50
# col_d = "#B3B3B3" #grey70

pal_01=c(col_a, col_b, col_c, col_d)


# bar borders 
bar_bor1="#FFFFFF" #white
bar_bor2= "#4D4D4D" #grey30 

# legend settings
#legendcontent=
#colcontent= 
myfavbty= "n"

# other constants
roundnum = 4


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


itwac_all = read.csv(thefilename_itwac, 
                     header = T,
                     sep=",",
                     enc="utf-8")

summary(itwac_all)
head(itwac_all, 20)

itwac_all$Freq <- as.integer(itwac_all$Freq)
#unique(itwac_all$POS)=="NOUN"

# drop itwac POS column
itwac_all$POS=NULL

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

# present subjunctive 1- 2- 3 ps
# present indicative 1 -2 ps
# present infinite 

verbs0=  morphit[morphit$POS=="VER:sub+pres+1+s"|
#          morphit$POS=="VER:sub+pres+2+s"|
 #         morphit$POS=="VER:sub+pres+3+s"|
          morphit$POS=="VER:ind+pres+1+s"|
          morphit$POS=="VER:ind+pres+3+s"|
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

head(verbs1)

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
 # list exceptions "sciere"

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
 
 
 # two-letter clusters tS/k
 verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ce", "tSe")
 verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ci", "tSi")
 verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ca", "ka")
 verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "co", "ko")
 verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "cu", "ku")
 
 
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
 
 
 ## voiceless velar stop, geminate
 verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gghe", "gge")
 verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gghi", "ggi")
 #verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gga", "gga")
 #verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ggo", "ggo")
 #verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ggu", "ggu")
 summary(verbs2$fonform==verbs2$form)
 head(verbs2)
 
 
 # two-letter clusters dG/g
 verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ge", "dGe")
 verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gi", "dGi")
 #verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "ga", "ga")
 #verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "go", "go")
 #verbs2$fonform=stringr::str_replace_all(verbs2$fonform, "gu", "gu")
 summary(verbs2$fonform==verbs2$form)
 head(verbs2)
 
 
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
 
 
# strip root
verbs3= verbs2
verbs3$fonroot=str_sub(verbs3$fonform, 1, -2)
verbs3$fonsuff=str_sub(verbs3$fonform, -1, -1)


 

#
# split conjugations 
#
#

# 1st conjugation ere
are_verbs=verbs3[verbs3$conj=="are", ]

# 2nd conjugation ere
ere_verbs=verbs3[verbs3$conj=="ere", ]

# 3rd conjugation ire
ire_verbs=verbs3[verbs3$conj=="ire", ]

# rre_verbs - think about it 
rre_verbs=verbs3[verbs3$conj=="rre", ]

#
#
#     1st conjugation
#                   " -are "
#

#
# split cells 
#

# 1st conjugation
# present indicative 1 ps

ARE_indpres_1s=are_verbs[are_verbs$POS=="VER:ind+pres+1+s", ]
unique(ARE_indpres_1s$fonsuff)

# forms in [-io] 
ARE_indpres_1s_jo=ARE_indpres_1s[grep("i$", ARE_indpres_1s$fonroot), ]
unique(ARE_indpres_1s_jo$fonsuff)

# subtract forms in -io
#ARE_indpres_1s=ARE_indpres_1s[ARE_indpres_1s$form %in% ARE_indpres_1s_jo$form==F , ]

# assign columns 
ARE_indpres_1s$form_1ps_ind=ARE_indpres_1s$form
ARE_indpres_1s$fonroot_1ps_ind=ARE_indpres_1s$fonroot

# assign columns -io
ARE_indpres_1s_jo$form_1ps_ind=ARE_indpres_1s_jo$form
ARE_indpres_1s_jo$fonroot_1ps_ind=ARE_indpres_1s_jo$fonroot

# 1st conjugation
# present indicative 3 ps

ARE_indpres_3s=are_verbs[are_verbs$POS=="VER:ind+pres+3+s", ]
unique(ARE_indpres_3s$fonsuff)

# forms in [-io] 
ARE_indpres_3s_i=ARE_indpres_3s[grep("i$", ARE_indpres_3s$fonroot), ]
unique(ARE_indpres_3s_i$fonsuff)

# subtract forms in -ia
ARE_indpres_3s=ARE_indpres_3s[ARE_indpres_3s$form %in% ARE_indpres_3s_i==F , ]

# assign columns
ARE_indpres_3s$form_3ps_ind=ARE_indpres_3s$form
ARE_indpres_3s$fonroot_3ps_ind=ARE_indpres_3s$fonroot

# 1st conjugation
# present subjunctive 1 ps

ARE_subjpres_1s=are_verbs[are_verbs$POS=="VER:sub+pres+1+s", ]
unique(ARE_subjpres_1s$fonsuff)

# forms in [-io] 
#ARE_subjpres_1s_i=ARE_subjpres_1s[grep("i$", ARE_subjpres_1s$fonroot), ]
ARE_subjpres_1s_i=ARE_subjpres_1s[ARE_subjpres_1s$lemma_morphit %in% ARE_indpres_1s_jo$lemma_morphit==T, ]
ARE_subjpres_1s_i$fonroot=ARE_subjpres_1s_i$fonform

# subtract
ARE_subjpres_1s=ARE_subjpres_1s[ARE_subjpres_1s$lemma_morphit %in%ARE_subjpres_1s_i==F, ]

#remerge
ARE_subjpres_1s=rbind(ARE_subjpres_1s, ARE_subjpres_1s_i)

ARE_subjpres_1s_ii=ARE_subjpres_1s[grep("ii$", ARE_subjpres_1s$fonform), ]
unique(ARE_subjpres_1s_ii$fonsuff)

# assign columns
ARE_subjpres_1s$form_1ps_sub=ARE_subjpres_1s$form
ARE_subjpres_1s$fonroot_1ps_sub=ARE_subjpres_1s$fonroot


ARE_verbs_morph0=merge(ARE_indpres_1s[, c("form_1ps_ind", "fonroot_1ps_ind", "lemma_morphit", "conj")], 
                       ARE_indpres_3s[ , c("form_3ps_ind", "fonroot_3ps_ind", "lemma_morphit")], 
                       by.x="lemma_morphit", 
                       by.y="lemma_morphit", 
                       all.x = F)

ARE_verbs_morph=merge(ARE_verbs_morph0, 
                      ARE_subjpres_1s[ , c("form_1ps_sub", "fonroot_1ps_sub", "lemma_morphit")], 
                      by.x="lemma_morphit", 
                      by.y="lemma_morphit", 
                      all.x = F)


ARE_verbs_morph$L_morph=ifelse(ARE_verbs_morph$fonroot_1ps_ind==ARE_verbs_morph$fonroot_1ps_sub 
                               &!ARE_verbs_morph$fonroot_1ps_ind==ARE_verbs_morph$fonroot_3ps_ind,
                               1, 0)

#
#
#     2nd conjugation
#                   " -ere "
#

#
# split cells 
#

# 2nd conjugation
# present indicative 1 ps

ERE_indpres_1s=ere_verbs[ere_verbs$POS=="VER:ind+pres+1+s", ]
unique(ERE_indpres_1s$fonsuff)

# assign columns
ERE_indpres_1s$form_1ps_ind=ERE_indpres_1s$form
ERE_indpres_1s$fonroot_1ps_ind=ERE_indpres_1s$fonroot

# forms in [-io] 
ERE_indpres_1s_jo=ERE_indpres_1s[grep("i$", ERE_indpres_1s$fonroot), ]
unique(ERE_indpres_1s_jo$fonsuff)

# 2nd conjugation
# present indicative 3 ps

ERE_indpres_3s=ere_verbs[ere_verbs$POS=="VER:ind+pres+3+s", ]
unique(ERE_indpres_3s$fonsuff)
ERE_indpres_3s[ERE_indpres_3s$fonsuff=="a", ]

# discard verbs in "a" ("sa"/"ha")
ERE_indpres_3s=ERE_indpres_3s[ERE_indpres_3s$fonsuff!="a", ]

# forms in [-io] 
ERE_indpres_3s_i=ERE_indpres_3s[grep("i$", ERE_indpres_3s$fonroot), ]

# assign columns
ERE_indpres_3s$form_3ps_ind=ERE_indpres_3s$form
ERE_indpres_3s$fonroot_3ps_ind=ERE_indpres_3s$fonroot


#
# 2nd conjugation
# present subjunctive 1 ps

ERE_subjpres_1s=ere_verbs[ere_verbs$POS=="VER:sub+pres+1+s", ]
unique(ERE_subjpres_1s$fonsuff)

ERE_subjpres_1s$form_1ps_sub=ERE_subjpres_1s$form
ERE_subjpres_1s$fonroot_1ps_sub=ERE_subjpres_1s$fonroot

ERE_verbs_morph0=merge(ERE_indpres_1s[, c("form_1ps_ind", "fonroot_1ps_ind", "lemma_morphit", "conj")], 
                       ERE_indpres_3s[ , c("form_3ps_ind", "fonroot_3ps_ind", "lemma_morphit")], 
                       by.x="lemma_morphit", 
                       by.y="lemma_morphit", 
                       all.x = F)

ERE_verbs_morph=merge(ERE_verbs_morph0, 
                      ERE_subjpres_1s[ , c("form_1ps_sub", "fonroot_1ps_sub", "lemma_morphit")], 
                      by.x="lemma_morphit", 
                      by.y="lemma_morphit", 
                      all.x = F)

ERE_verbs_morph$L_morph=ifelse(ERE_verbs_morph$fonroot_1ps_ind==ERE_verbs_morph$fonroot_1ps_sub 
                               &!ERE_verbs_morph$fonroot_1ps_ind==ERE_verbs_morph$fonroot_3ps_ind,
                               1, 0)

#
#
#     3rd conjugation
#                   " -ire "
#

#
# split cells 
#

# 3rd conjugation
# present indicative 1 ps

IRE_indpres_1s=ire_verbs[ire_verbs$POS=="VER:ind+pres+1+s", ]
IRE_indpres_1s$form_1ps_ind=IRE_indpres_1s$form
IRE_indpres_1s$fonroot_1ps_ind=IRE_indpres_1s$fonroot


# forms in [-io] 
IRE_indpres_1s_jo=IRE_indpres_1s[grep("i$", IRE_indpres_1s$fonroot), ]
unique(IRE_indpres_1s_jo$fonsuff)

# 3rd conjugation
# present indicative 3 ps

IRE_indpres_3s=ire_verbs[ire_verbs$POS=="VER:ind+pres+3+s", ]

# assign columns
IRE_indpres_3s$form_3ps_ind=IRE_indpres_3s$form
IRE_indpres_3s$fonroot_3ps_ind=IRE_indpres_3s$fonroot

# forms in [-io] 
IRE_indpres_3s_i=IRE_indpres_3s[grep("i$", IRE_indpres_3s$fonroot), ]
unique(IRE_indpres_3s_i$fonsuff)

# 3rd conjugation
# present subjunctive 1 ps

IRE_subjpres_1s=ire_verbs[ire_verbs$POS=="VER:sub+pres+1+s", ]
unique(IRE_subjpres_1s$fonsuff)

# assign columns
IRE_subjpres_1s$form_1ps_sub=IRE_subjpres_1s$form
IRE_subjpres_1s$fonroot_1ps_sub=IRE_subjpres_1s$fonroot

IRE_verbs_morph0=merge(IRE_indpres_1s[, c("form_1ps_ind", "fonroot_1ps_ind", "lemma_morphit", "conj")], 
                       IRE_indpres_3s[ , c("form_3ps_ind", "fonroot_3ps_ind", "lemma_morphit")], 
                       by.x="lemma_morphit", 
                       by.y="lemma_morphit", 
                       all.x = F)

IRE_verbs_morph=merge(IRE_verbs_morph0, 
                      IRE_subjpres_1s[ , c("form_1ps_sub", "fonroot_1ps_sub", "lemma_morphit")], 
                      by.x="lemma_morphit", 
                      by.y="lemma_morphit", 
                      all.x = F)

IRE_verbs_morph$L_morph=ifelse(IRE_verbs_morph$fonroot_1ps_ind==IRE_verbs_morph$fonroot_1ps_sub 
                               &!IRE_verbs_morph$fonroot_1ps_ind==IRE_verbs_morph$fonroot_3ps_ind,
                               1, 0)



#
#
#     verbs in 
#              "-rre"     #

#
# split cells 
#

# -rre
# present indicative 1 ps

RRE_indpres_1s=rre_verbs[rre_verbs$POS=="VER:ind+pres+1+s", ]
unique(RRE_indpres_1s$fonsuff)
RRE_indpres_1s$form_1ps_ind=RRE_indpres_1s$form
RRE_indpres_1s$fonroot_1ps_ind=RRE_indpres_1s$fonroot


# -rre
# present indicative 3 ps

RRE_indpres_3s=rre_verbs[rre_verbs$POS=="VER:ind+pres+3+s", ]
unique(RRE_indpres_3s$fonsuff)

RRE_indpres_3s$form_3ps_ind=RRE_indpres_3s$form
RRE_indpres_3s$fonroot_3ps_ind=RRE_indpres_3s$fonroot


# -rre
# present subjunctive 1 ps
RRE_subjpres_1s=rre_verbs[rre_verbs$POS=="VER:sub+pres+1+s", ]
unique(RRE_subjpres_1s$fonsuff)

RRE_subjpres_1s$form_1ps_sub=RRE_subjpres_1s$form
RRE_subjpres_1s$fonroot_1ps_sub=RRE_subjpres_1s$fonroot


RRE_verbs_morph0=merge(RRE_indpres_1s[, c("form_1ps_ind", "fonroot_1ps_ind", "lemma_morphit", "conj")], 
                      RRE_indpres_3s[ , c("form_3ps_ind", "fonroot_3ps_ind", "lemma_morphit")], 
                      by.x="lemma_morphit", 
                      by.y="lemma_morphit", 
                      all.x = F)

RRE_verbs_morph=merge(RRE_verbs_morph0, 
                      RRE_subjpres_1s[ , c("form_1ps_sub", "fonroot_1ps_sub", "lemma_morphit")], 
                      by.x="lemma_morphit", 
                      by.y="lemma_morphit", 
                      all.x = F)

RRE_verbs_morph$L_morph=ifelse(RRE_verbs_morph$fonroot_1ps_ind==RRE_verbs_morph$fonroot_1ps_sub 
                               &!RRE_verbs_morph$fonroot_1ps_ind==RRE_verbs_morph$fonroot_3ps_ind,
                              1, 0)


all_verbs_morph=rbind(ARE_verbs_morph, ERE_verbs_morph, IRE_verbs_morph, RRE_verbs_morph)  

# output csvs
write.csv(ARE_verbs_morph, "ARE_verbs_morpho.csv")
write.csv(ARE_verbs_morph, "ERE_verbs_morpho.csv")
write.csv(ARE_verbs_morph, "IRE_verbs_morpho.csv")
write.csv(ARE_verbs_morph, "RRE_verbs_morpho.csv")

write.csv(all_verbs_morph, "all_verbs_morpho.csv")
