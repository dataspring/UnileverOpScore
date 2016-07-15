
set.seed(17)


load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\20141211130801-Training.RData")
uni.raw <- Training_Data[,colSums(is.na(Training_Data)) != nrow(Training_Data)]

# //test data prep loading
load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\20141211130759-Submission.RData")
unitest1.raw <- Submission_Data[,colSums(is.na(Submission_Data)) != nrow(Submission_Data)]

#find missing columns
missing <- setdiff(colnames(uni.raw), colnames(unitest1.raw))
#fill in the missing columns, ensure they are of proper data type
unitest1.raw[missing] <- as.numeric(NA)
unitest1.raw$Opinon.on.color.of.the.detergent.powder = as.character(unitest1.raw$Opinon.on.color.of.the.detergent.powder)
unitest1.raw$Dissolution.problems  = as.character(unitest1.raw$Dissolution.problems)


#reorder the missing columns accordingly to the train data
unitest.raw <- unitest1.raw[colnames(uni.raw)]
#something spurious
missing.puz <- setdiff(colnames(unitest.raw), colnames(unitest1.raw))
missing.puz


## ----- an Learning Vector Quantization (LVQ) model. The varImp is then used to estimate the variable importance---------------
## -- http://machinelearningmastery.com/feature-selection-with-the-caret-r-package/ 

### --------------------not done ------------------------------
## ---- remove 1st set of duplicate columns "Smell..While.in.the.pack" to "Smell..When.wearing.clothes.at.the.end.of.the.day" 
## --- with "Strength.of.Smell..While.in.the.pack" to "Strength.of.Smell..When.wearing.clothes.at.the.end.of.the.day"  
## --- remove columns 170 to 179   ---- retain this though 180 to 189
### --------------------not done ------------------------------



for(i in 204:228) {
  unitest.raw[,i] = sapply(unitest.raw[,i], function(v) {
    if (!is.na(str_locate(v, ignore.case("no "))[1])) 
      return ("No")
    else return (v)
  })
}

for(i in 204:228) {
  
  unitest.raw[,i] = sapply(unitest.raw[,i], function(v) {
    vtrim <- toupper(str_trim(v))
    ifelse (vtrim != "YES" && vtrim != "NO" && vtrim != "NA", "YES", v)
  })
  
}

#-- Removing Whitespace From a Whole Data Frame in R
unitest.raw <- data.frame(lapply(unitest.raw, function(v) {
  if (is.character(v)) return(gsub('\\s+', '',v))
  else return(v)
}), stringsAsFactors = FALSE)

#-- convert all chr to lowercase 
unitest.raw <- data.frame(lapply(unitest.raw, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}), stringsAsFactors = FALSE)

#-- assign all numbers intact removing explantory prefixes
unitest.raw <- data.frame(lapply(unitest.raw, function(v) {
  if (is.character(v) & !is.na(str_locate(v, "\\(\\d\\)")[1])) 
  {
    strloc <- str_locate(v,"\\(\\d\\)")
    return(str_sub(v, start=strloc[,1], end=strloc[,2]))
  }
  else return(v)
}), stringsAsFactors = FALSE)

## -- for some columns that don't get worked from the previous logic -----------------

for(i in 253:275) {
    unitest.raw[,i] = sapply(unitest.raw[,i], function(v) {
      if (is.character(v) & !is.na(str_locate(v, "\\(\\d\\)")[1])) 
      {
        strloc <- str_locate(v,"\\(\\d\\)")
        return(str_sub(v, start=strloc[,1], end=strloc[,2]))
      }
      else return(v)
    })
}

## -------------------------------------------------------------------------------------------------
## - check point to save the interim result so that can be replayed

save(unitest.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unileverTest-Prep1.RData" )
#write.table(x=unitest.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-prep.csv", sep="\t" )

##--load train data for string handling stuff
load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-Prep1.RData")

load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unileverTest-Prep1.RData")
#write.table(x=unitest.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-prep-csv.csv", sep="," )
## -------------------------------------------------------------------------------------------------


## ----- column level data munging to reduce the factor levels of some features ------------------------

## -------------------------------------------------------------------------------------------------
recoderFunc <- function(data, oldvalue, newvalue) {
  # convert any factors to characters
  # create the return vector
  newvec <- data
  # put recoded values into the correct position in the return vector
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  newvec
}


## ------------------- Column Education.level ----------------------------------------------------------
oldvalue <- as.character(na.omit(unique(unitest.raw$Education.level))) 
# [1] "finishedhighschool/vocational/diploma"               "finishedprimaryschool"                              
# [3] "finishedsecondaryschool"                             "unfinished/studyingsecondaryschool"                 
# [5] "*unfinished/studyinghighschool/vocational/diploma"   "*unfinished/studyinguniversity"                     
# [7] "finisheduniversityorhigher"                          "unfinished/studyingprimaryschool"                   
# [9] "illiterate"                                          "junior/middleschoolincomplete"                      
# [11] "*junior/middleschoolcomplete"                        "*secondary/senior/highschoolcomplete"               
# [13] "*college/universitycomplete(graduate)"               "primarycomplete"                                    
# [15] "literatebutnoformal/schooleducation"                 "postgraduatestudiesincomplete(nopostgraduatedegree)"
# [17] "*postgraduatestudiescomplete(postgraduatedegree)"    "primaryincomplete"                                  
# [19] "secondary/senior/highschoolincomplete"               "completedsecondary/seniorhighschool"                
# [21] "completedcollege/university"                         "completedprimary/junior"                            
# [23] "completedapostgraduatecourse"                        "college/universityincomplete"                       
# [25] "completedsecondary/middleschool(approx11-15)"        "completedprimary/juniorschool(approxages6-11)"      
# [27] "completedcollege/university(approx19years+)"         "completedpostgraduatecourse"                        
# [29] "noformalorschooleducation"                           "completedupper/highschool/6thformcollege"           
# [31] "someprimary/juniorschoolbutnotcompleted"             "donætknow"                                          
# [33] "someprimary/junior,butnotcompleted"                  "someprimary/junior,butnotcomplete"                  
# [35] "noformaleducation"
newvalue <- c("diploma","primary",
              "secondary","secondary",
              "diploma","graduate",
              "graduate","primary",
              "illiterate","primary",
              "primary", "secondary",
              "graduate","primary",
              "primary","graduate",
              "graduate","primary",
              "secondary","secondary",                                    
              "graduate","primary",
              "graduate", "graduate",
              "secondary","primary",
              "graduate","graduate",
              "illiterate","secondary",
              "primary","primary",
              "primary","primary",
              "illiterate")  

as.character(na.omit(unique(unitest.raw$Education.level)))
unitest.raw$Education.level <- recoderFunc(unitest.raw$Education.level, oldvalue, newvalue)
as.character(na.omit(unique(unitest.raw$Education.level)))
## ------------------- Column Education.level ****ends***------------------------------------------------


## ------------------- Column Amount.used  ----------------------------------------------------------
oldvalue <- as.character(na.omit(unique(unitest.raw$Amount.used ))) 
# [1] "aboutthree-fourth(+)"          "fullyused"                     "abouthalf(+)"                 
# [4] "aboutthree-quartersofapack"    "onewholepack"                  "abouthalfofapack"             
# [7] "lessthanaquarterofapack"       "abouthalfofpack"               "aboutthreequartersofapack"    
# [10] "abouthalfapack"                "aboutaquarterofapack"          "aboutthree-quartersofapack..."
# [13] "onewholepack..."               "abouthalfofapack..."           "afourth(+)"    

newvalue <- c("3by4","all","1by2",
              "3by4","all","1by2",
              "1by4","1by2","3by4",
              "1by2","1by4","3by4",
              "all","1by2","1by4")  

as.character(na.omit(unique(unitest.raw$Amount.used )))
unitest.raw$Amount.used  <- recoderFunc(unitest.raw$Amount.used , oldvalue, newvalue)
as.character(na.omit(unique(unitest.raw$Amount.used )))
## ------------------- Column Education.level ****ends***------------------------------------------------

# cn <- data.frame(A=as.character(na.omit(unique(uni.raw$Times.used ))), B = newvalue )
# process <- function(x) {
#   return(paste(cn[cn$A == x,][1,2], ",", sep=""))  
# }           
# apply(cbind(oldvalue), 1, process)


## ------------------- Column Times.used  ----------------------------------------------------------
oldvalue <- as.character(na.omit(unique(unitest.raw$Times.used ))) 
# [1] "11+"                       "6to10"                     "7-10"                      "6-10"                     
# [5] "10-14"                     "15+"                       "8-11"                      "12+"                      
# [9] "nooftimes24-27(25.5)"      "nooftimes12-15(13.5)"      "nooftimes20-23(21.5)"      "nooftimes16-19(17.5)"     
# [13] "nooftimes28(28)"           "morethan12times"           "12times"                   "10times"                  
# [17] "13ormoretimes"             "7-10timesinthelast14days"  "10-12timesinthelast14days" "6timesinthelast14days"    
# [21] "10-14(12)"                 "15+(15)"                   "everyday"                  "10+timesinthelast14days"  
# [25] "15plus"

# [1] "11+"                       "6to10"                     "7-10"                      "10-14"                    
# [5] "15+"                       "nooftimes24-27(25.5)"      "nooftimes16-19(17.5)"      "nooftimes20-23(21.5)"     
# [9] "nooftimes28(28)"           "nooftimes12-15(13.5)"      "8-11"                      "12+"                      
# [13] "morethan12times"           "10times"                   "12times"                   "13ormoretimes"            
# [17] "10-12timesinthelast14days" "7-10timesinthelast14days"  "6timesinthelast14days"     "6-10"                     
# [21] "10+timesinthelast14days"   "everyday"  

newvalue <- c("11to13","lt8","lt8","11to13",
              "14to16","gt16","gt16","gt16",
              "gt16","14to16","9to10","11to13",
              "14to16","9to10","11to13","14to16",
              "9to10","lt8","lt8","lt8",
              "11to13","14to16")  

as.character(na.omit(unique(unitest.raw$Times.used )))
unitest.raw$Times.used  <- recoderFunc(unitest.raw$Times.used , oldvalue, newvalue)
as.character(na.omit(unique(unitest.raw$Times.used )))
## ------------------- Column Times.used  ****ends***------------------------------------------------


# cn <- data.frame(A=as.character(na.omit(unique(uni.raw$Dry.method ))), B = newvalue )
# process <- function(x) {
#   return(paste(cn[cn$A == x,][1,2], ",", sep=""))  
# }           
# apply(cbind(oldvalue), 1, process)

## ------------------- Column Times.used  ----------------------------------------------------------
oldvalue <- as.character(na.omit(unique(unitest.raw$Dry.method ))) 
# "drymostclothesoutsideinthesun"             "dryallclothesoutsideinthesun"             
# [3] "*dryallclothesoutsideintheshade"           "*drymostclothesoutsideintheshade"         
# [5] "oudoorsinsun"                              "outdoorsinsunandshade"                    
#    "outdoorsinshade"                          
# [9] "tumbledried"                               "indoors,withoutheat(e.g.onaclothesmaiden)"
# [11] "***drymostclothesinadryer"                 "**dryallclothesinsidethehouse"            
# [13] "driedmostclothesoutsideinthesun"           "driedallclothesoutsideinthesun"           
# [15] "*driedallclothesinsidethehouse"            "*driedmostclothesinsidethehouse"          
# [17] "driedallclothesoutsideintheshade"          "driedmostclothesoutsideintheshade"        
# [19] "indoors,withheat(e.g.overaradiator)"       "**drymostclothesinsidethehouse"           
# [21] "indoors,withoutheat"

# [1] "drymostclothesoutsideinthesun"             "dryallclothesoutsideinthesun"             
# [3] "***drymostclothesinadryer"                 "*dryallclothesoutsideintheshade"          
# [5] "*drymostclothesoutsideintheshade"          "oudoorsinsun"                             
# [7] "outdoorsinshade"                           "outdoorsinsunandshade"                    
# [9] "driedallclothesoutsideinthesun"            "driedmostclothesoutsideinthesun"          
# [11] "*driedallclothesinsidethehouse"            "driedallclothesoutsideintheshade"         
# [13] "driedmostclothesoutsideintheshade"         "*driedmostclothesinsidethehouse"          
# [15] "**driedallclothesinadryer"                 "indoors,withheat(e.g.overaradiator)"      
# [17] "indoors,withoutheat(e.g.onaclothesmaiden)" "**drymostclothesinsidethehouse"           
# [19] "**dryallclothesinsidethehouse"

newvalue <- c("outdoors","outdoors",
              "indoors","outdoors",
              "outdoors","outdoors",
              "outdoors","outdoors",
              "outdoors","outdoors",
              "indoors","indoors",
              "outdoors","indoors",
              "indoors","indoors",
              "indoors","indoors",
              "indoors")  

as.character(na.omit(unique(unitest.raw$Dry.method )))
unitest.raw$Dry.method  <- recoderFunc(unitest.raw$Dry.method , oldvalue, newvalue)
as.character(na.omit(unique(unitest.raw$Dry.method )))
## ------------------- Column Times.used  ****ends***------------------------------------------------

# cn <- data.frame(A=as.character(na.omit(unique(uni.raw$Annual.household.income ))), B = newvalue )
# process <- function(x) {
#   return(paste(cn[cn$A == x,][1,2], ",", sep=""))  
# }           
# apply(cbind(oldvalue), 1, process)

## ------------------- Column Annual.household.income  ----------------------------------------------------------
oldvalue <- as.character(na.omit(unique(unitest.raw$Annual.household.income ))) 

# [1] "25001-99999"         "0-0"                 "15001-20000"         "8001-10000"          "20001-25000"        
# [6] "10001-15000"         "7001-8000"           NA                    "rs.8001-rs.10,000"   "rs.25,001+"         
# [11] "rs.10,001-rs.15,000" "rs.20,001-rs.25,000" "rs.5001-rs.7000"     "rs.2001-rs.3000"     "iprefernottosay"    
# [16] "rs.4001-rs.5000"     "rs.15,001-rs.20,000" "rs.7001-rs.8000"     "rs.3001-rs.4000"     "uptors.2000"        
# [21] "rs5000-10,000"       "rs20,000to30,000"    "rs10,001to20,000"    "30,001-40,000"       "40,001-50,000"      
# [26] "50,001-60,000"       "lessthanrs.5000"     "above80,000"         "4001-5000"           "1-2000"  

# [1] "25001-99999"         "10001-15000"         "15001-20000"         "8001-10000"          "5001-7000"          
# [6] "0-0"                 "7001-8000"           "20001-25000"         "rs10,001to20,000"    "rs5000-10,000"      
# [11] "rs20,000to30,000"    "30,001-40,000"       "lessthanrs.5000"     "40,001-50,000"       "rs.10,001-rs.15,000"
# [16] "rs.25,001+"          "rs.7001-rs.8000"     "rs.4001-rs.5000"     "rs.5001-rs.7000"     "rs.3001-rs.4000"    
# [21] "rs.2001-rs.3000"     "rs.8001-rs.10,000"   "iprefernottosay"     "rs.15,001-rs.20,000" "rs.20,001-rs.25,000"

newvalue <- c("gt25k","lt25k","lt25k","lt25k","lt25k",
              "lt25k","lt25k","lt25k","lt25k","lt25k",
              "lt25k","gt25k","lt25k","gt25k","lt25k",
              "gt25k","lt25k","lt25k","lt25k","lt25k",
              "lt25k","lt25k","lt25k","lt25k","lt25k")  

as.character(na.omit(unique(unitest.raw$Annual.household.income)))
unitest.raw$Annual.household.income  <- recoderFunc(unitest.raw$Annual.household.income , oldvalue, newvalue)
as.character(na.omit(unique(unitest.raw$Annual.household.income )))

## ------------------- Column Annual.household.income ends ----------------------------------------------------------

## --- consolidate handle foam's 7 columns into 1 -------------------------------------------------------------------

# remove these duplicate columns 
# 
# Additional.Others..Difficult.to.remove.odours
# Additional.Others..Difficult.to.remove.stains
# Additional.Others..Powder.left.in.bucket
# Additional.Others..Powder.left.on.clothes
# Additional.Others..Yellowing.of.clothes 
# ### ---------------------------------------------------------------------------------------------
# 
# ### ----- consolidate foam column from these columns
# 
# Additional.Others..Foam.is.too.much  
# Additional.Others..Foam.is.too.less  
# Additional.Others..Foam.is.less.long.lasting  
# Additional.Others..More.time.to.lather

require(data.table)

process <- function(x) {
  
  if (is.na(x["Amount.of.form"])) 
  {
    if (!is.na(x["Amount.of.form..while.mixing.with.water"]) ||
          !is.na(x["Amount.of.form..while.soaking.clothes"]) ||
          !is.na(x["Amount.of.form..while.washing.clothes"]) ||
          !is.na(x["Amount.of.form..while.rising.clothes"]))
    {
      dtl <- data.table(Aa = c(x["Amount.of.form..while.mixing.with.water"],
                               x["Amount.of.form..while.soaking.clothes"],
                               x["Amount.of.form..while.washing.clothes"],
                               x["Amount.of.form..while.rising.clothes"] ), 
                        Cc = c(1,1,1,1))
      dtlsort <- dtl[, length(Cc), by=Aa]
      
      
      return (dtlsort[order(-dtlsort$V1)][1]$Aa)
    }
    else
    {
      if (!is.na(x["Additional.Others..Foam.is.too.much"]))
      {
        vtrim <- toupper(x["Additional.Others..Foam.is.too.much"])
        ifelse (vtrim == "NO", "(3)", (ifelse (vtrim == "YES", "(1)", NA)))
      }
      else
      {
        if (!is.na(x["Additional.Others..Foam.is.too.less"]))
        {
          vtrim <- toupper(x["Additional.Others..Foam.is.too.less"])
          ifelse (vtrim == "NO", "(3)", (ifelse (vtrim == "YES", "(1)", NA)))
        }
        else
          return(x["Amount.of.form"])
      }
        
    }
  }
  else
    return(x["Amount.of.form"])
  
}           

unitest.raw$Amount.of.form <- cbind(apply(unitest.raw, 1, process))


## ------------------------------------------------------------------------------------------------------------------

## ----------------- combine 4 columns that is used to handle powder - hand/cup/scoop/plastic --------
unitest.raw$Cup = sapply(unitest.raw$Cup, function(v) {ifelse (v != "no", "Cup", v)})
unitest.raw$Hands = sapply(unitest.raw$Hands, function(v) {ifelse (v != "no" && v != "none", "Hands", v)})
unitest.raw$Teaspoon = sapply(unitest.raw$Teaspoon, function(v) {ifelse (v != "no", "Teaspoon", v)})
unitest.raw$Plastic.Scoop = sapply(unitest.raw$Plastic.Scoop, function(v) {ifelse (v != "no", "Scoop", v)})
unitest.raw$Table.Spoon = sapply(unitest.raw$Table.Spoon, function(v) {ifelse (v != "no", "Table", v)})

unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop <- unitest.raw$Cup
unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop[is.na(unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop)] <- unitest.raw$Hands[is.na(unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop)]
unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop[is.na(unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop)] <- unitest.raw$Teaspoon[is.na(unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop)]
unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop[is.na(unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop)] <- unitest.raw$Plastic.Scoop[is.na(unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop)]
unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop[is.na(unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop)] <- unitest.raw$Table.Spoon[is.na(unitest.raw$Cup.Hands.Teaspoon.Plastic.Scoop)]


## ----------------- combine 4 columns that is used to handle powder - hand/cup/scoop/plastic --------


# unitest.raw <- as.data.frame(sapply(unitest.raw, tolower))

# unitest.raw1 <- unitest.raw
unitest.raw[c(3:129)][is.na(unitest.raw[c(3:129)])] <- 0

unitest.raw[c(3:129)] <- unitest.raw[c(3:129)] /100

## -------------------------------------------------------------------------------------------------
## - check point to save the interim result so that can be replayed

save(unitest.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unileverTest-Prep2.RData" )
#write.table(x=unitest.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-prep.csv", sep="\t" )

load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unileverTest-Prep2.RData")
#write.table(x=unitest.raw, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-prep-csv.csv", sep="," )
## -------------------------------------------------------------------------------------------------


#---------- column "Smell..when.you.wear.them" to be prepared from teh following columns ---------------
# Smell..once.they.are.dry 42
# Smell..Whilst.ironing 72
# Smell..When.wearing.clothes.for.first.time.after.washing 68
# Smell..When.wearing.clothes.at.the.start.of.the.day 84.30
# Smell..When.wearing.clothes.at.the.end.of.the.day 68.86
#-----------------------------------------------------------
process <- function(x) {
  
  if (is.na(x["Smell..when.you.wear.them"])) 
  {
    if (!is.na(x["Smell..once.they.are.dry"]) ||
          !is.na(x["Smell..Whilst.ironing"]) ||
          !is.na(x["Smell..When.wearing.clothes.for.first.time.after.washing"]) ||
          !is.na(x["Smell..When.wearing.clothes.at.the.start.of.the.day"]) ||
          !is.na(x["Smell..When.wearing.clothes.at.the.end.of.the.day"]))
    {
      dtl <- data.table(Aa = c(x["Smell..once.they.are.dry"],
                               x["Smell..Whilst.ironing"],
                               x["Smell..When.wearing.clothes.for.first.time.after.washing"],
                               x["Smell..When.wearing.clothes.at.the.start.of.the.day"],
                               x["Smell..When.wearing.clothes.at.the.end.of.the.day"] ), 
                        Cc = c(1,1,1,1,1))
      dtlsort <- dtl[, length(Cc), by=Aa]
      
      
      return (dtlsort[order(-dtlsort$V1)][1]$Aa)
    }
    else
    {
      return(x["Smell..when.you.wear.them"])
    }
  }
  else
    return(x["Smell..when.you.wear.them"])
  
}           
unitest.raw$Smell..when.you.wear.them <- cbind(apply(unitest.raw, 1, process))
#------------------------------------------------------------------------------------------------------------


#---------- column "Smell..While.washing.the.clothes" to be prepared from teh following columns ---------------
# Smell..While.mixing.with.water.and.soaking 76.27
# Smell..Puttng.the.wet.clothes.on.the.line.for.drying 58.38
# Smell..When.drying.clothes.outdoors.on.the.line 75.60
#-----------------------------------------------------------
process <- function(x) {
  
  if (is.na(x["Smell..While.washing.the.clothes"])) 
  {
    if (!is.na(x["Smell..While.mixing.with.water.and.soaking"]) ||
          !is.na(x["Smell..Puttng.the.wet.clothes.on.the.line.for.drying"]) ||
          !is.na(x["Smell..When.drying.clothes.outdoors.on.the.line"]))
    {
      dtl <- data.table(Aa = c(x["Smell..While.mixing.with.water.and.soaking"],
                               x["Smell..Puttng.the.wet.clothes.on.the.line.for.drying"],
                               x["Smell..When.drying.clothes.outdoors.on.the.line"] ), 
                        Cc = c(1,1,1))
      dtlsort <- dtl[, length(Cc), by=Aa]
      
      
      return (dtlsort[order(-dtlsort$V1)][1]$Aa)
    }
    else
    {
      return(x["Smell..While.washing.the.clothes"])
    }
  }
  else
    return(x["Smell..While.washing.the.clothes"])
  
}           
unitest.raw$Smell..While.washing.the.clothes <- cbind(apply(unitest.raw, 1, process))
#------------------------------------------------------------------------------------------------------------


#-------------------- original contribution of data ------------------------------------
contrib.raw <- data.frame(cbind(colnames(unitest.raw), NAs(unitest.raw)), stringsAsFactors=FALSE)
colnames(contrib.raw) <- c("ColName", "NAs")

contrib.raw$NAs <- as.numeric(contrib.raw$NAs)
unitest.rowcount <- nrow(unitest.raw)
contrib.raw$NonNAs <-  unitest.rowcount - contrib.raw$NAs
contrib.raw$PercentNAs  <- round(contrib.raw$NAs/unitest.rowcount * 100, digits=2)
contrib.raw$PercentData <- round(contrib.raw$NonNAs/unitest.rowcount * 100, digits=2)
#---------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------- Update data for additional fields from their counterparts ------
for(i in 205:216) {
  unitest.raw[,i] <- ifelse(is.na(unitest.raw[,i]), unitest.raw[,i+12], unitest.raw[,i])
}
##------------------------------------------------------------------------------------------------------------------



## ---- "Powder.left.on.clothes" to "Skin.Sensitive.Issues" to be skiped for their Additional counterparts (204 to 216) and retain 217 229
unitest.raw1 <- unitest.raw[, -c(205:216)]

## ----- remove all "strength.of.smell" columns for thier "smell" columns which are well calibrated
unitest.raw1 <- unitest.raw1[,-grep(toupper("Strength.of.smell"),cbind(toupper(colnames(unitest.raw1))))]



## remove these columns as they are consolidted
unitest.raw1$Cup <- NULL
unitest.raw1$Hands <- NULL
unitest.raw1$Teaspoon <- NULL
unitest.raw1$Plastic.Scoop <- NULL
unitest.raw1$Table.Spoon <- NULL


save(unitest.raw1, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unileverTest-Raw1.RData" )

#-----30% NAs data used for training - from training data provided by unilever
## --- loads <unitest.raw2>
load("D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-NA30.RData")

unitest.raw2 <- unitest.raw1[,c(colnames(uni.raw2))]
dim(unitest.raw2)
#-------------------------- final data for prediction 
save(unitest.raw2, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unileverTest-NA30.RData" )

options(scipen=10)
write.table(x=unitest.raw2, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unileverTest-NA30.csv", sep=",", quote=FALSE)
options(scipen=0)
## -------------------------------------------------------------------------------------------------



unitest.raw2[sapply(unitest.raw2, is.character)] <- as.data.frame(lapply(unitest.raw2[sapply(unitest.raw2, is.character)], 
                                       as.factor))
unitest.raw2[,1] <- as.character(unitest.raw2[,1])
unitest.raw2[,2] <- as.character(unitest.raw2[,2])
str(unitest.raw2, list.len=400)

##Feature Selection based on the contribution of data points vs NAs in that particluar column
NAs <- function(x) {
  as.vector(apply(x, 2, function(x) length(which(is.na(x)))))
}
contrib <- data.frame(cbind(colnames(unitest.raw1), NAs(unitest.raw1)), stringsAsFactors=FALSE)
colnames(contrib) <- c("ColName", "NAs")

contrib$NAs <- as.numeric(contrib$NAs)
uni.rowcount <- nrow(unitest.raw1)
contrib$NonNAs <-  unitest.rowcount - contrib$NAs
contrib$PercentNAs  <- round(contrib$NAs/unitest.rowcount * 100, digits=2)
summary(contrib$PercentNAs)


## --- further investigation of data of training data ---------------------

contrib2 <- data.frame(cbind(colnames(uni.raw2), NAs(uni.raw2)), stringsAsFactors=FALSE)
colnames(contrib2) <- c("ColName", "NAs")

contrib2$NAs <- as.numeric(contrib2$NAs)
uni.rowcount <- nrow(uni.raw2)
contrib2$NonNAs <-  uni.rowcount - contrib2$NAs
contrib2$PercentNAs  <- round(contrib2$NAs/uni.rowcount * 100, digits=2)


library(plyr)
cb1 <- contrib2[contrib2$PercentNAs < 10 & contrib2$PercentNAs != 0 ,]
colnames(cb1)[2] <- "10NAs"; colnames(cb1)[3] <- "10Data"; colnames(cb1)[4] <- "10NAs%"
cb2 <- contrib2[contrib2$PercentNAs < 20 & contrib2$PercentNAs != 0 ,]
colnames(cb2)[2] <- "20NAs"; colnames(cb2)[3] <- "20Data"; colnames(cb2)[4] <- "20NAs%"
cb3 <- contrib2[contrib2$PercentNAs < 30 & contrib2$PercentNAs != 0 ,]
colnames(cb3)[2] <- "30NAs"; colnames(cb3)[3] <- "30Data"; colnames(cb3)[4] <- "30NAs%"
cb4 <- contrib2[contrib2$PercentNAs < 40 & contrib2$PercentNAs != 0 ,]
colnames(cb4)[2] <- "40NAs"; colnames(cb4)[3] <- "40Data"; colnames(cb4)[4] <- "40NAs%"
cb5 <- contrib2[contrib2$PercentNAs < 50 & contrib2$PercentNAs != 0 ,]
colnames(cb5)[2] <- "50NAs"; colnames(cb5)[3] <- "50Data"; colnames(cb5)[4] <- "50NAs%"

cbb <- merge(cb1, cb2, all=TRUE, by="ColName")
cbb <- merge(cbb, cb3, all=TRUE, by="ColName")
cbb <- merge(cbb, cb4, all=TRUE, by="ColName")
cbb <- merge(cbb, cb5, all=TRUE, by="ColName")

contribu <- data.frame(cbb[c("ColName")], 100-cbb[c("10NAs%", "20NAs%", "30NAs%", "40NAs%","50NAs%")])

## --- further investigation of data of test data for submission ---------------------

contrib2 <- data.frame(cbind(colnames(unitest.raw2), NAs(unitest.raw2)), stringsAsFactors=FALSE)
colnames(contrib2) <- c("ColName", "NAs")

contrib2$NAs <- as.numeric(contrib2$NAs)
unitest.rowcount <- nrow(unitest.raw2)
contrib2$NonNAs <-  unitest.rowcount - contrib2$NAs
contrib2$PercentNAs  <- round(contrib2$NAs/unitest.rowcount * 100, digits=2)


library(plyr)
cb1 <- contrib2[contrib2$PercentNAs < 10 & contrib2$PercentNAs != 0 ,]
colnames(cb1)[2] <- "10NAs"; colnames(cb1)[3] <- "10Data"; colnames(cb1)[4] <- "10NAs%"
cb2 <- contrib2[contrib2$PercentNAs < 20 & contrib2$PercentNAs != 0 ,]
colnames(cb2)[2] <- "20NAs"; colnames(cb2)[3] <- "20Data"; colnames(cb2)[4] <- "20NAs%"
cb3 <- contrib2[contrib2$PercentNAs < 30 & contrib2$PercentNAs != 0 ,]
colnames(cb3)[2] <- "30NAs"; colnames(cb3)[3] <- "30Data"; colnames(cb3)[4] <- "30NAs%"
cb4 <- contrib2[contrib2$PercentNAs < 40 & contrib2$PercentNAs != 0 ,]
colnames(cb4)[2] <- "40NAs"; colnames(cb4)[3] <- "40Data"; colnames(cb4)[4] <- "40NAs%"
cb5 <- contrib2[contrib2$PercentNAs < 50 & contrib2$PercentNAs != 0 ,]
colnames(cb5)[2] <- "50NAs"; colnames(cb5)[3] <- "50Data"; colnames(cb5)[4] <- "50NAs%"

cbb <- merge(cb1, cb2, all=TRUE, by="ColName")
cbb <- merge(cbb, cb3, all=TRUE, by="ColName")
cbb <- merge(cbb, cb4, all=TRUE, by="ColName")
cbb <- merge(cbb, cb5, all=TRUE, by="ColName")

contribuT <- data.frame(cbb[c("ColName")], 100-cbb[c("10NAs%", "20NAs%", "30NAs%", "40NAs%","50NAs%")])

merge(contribu, contribuT, by="ColName", all=TRUE)


### ------------------------------------Imputation using missForest-----------------------------------


## -- let us start imputing this final data set and see how it goes - use missForest as KNN impute is only linux
## -- http://stackoverflow.com/questions/23288668/can-i-use-package-imputation-in-r-version-3-0-3

library("doParallel")
library("missForest")
library(randomForest)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

##---cannot impute chr data types and we ignore all numeric data types too
unitest.raw1.imp <- missForest(unitest.raw2[,c(130:172)], verbose=TRUE, parallelize = "forests", maxiter = 8, ntree = 100)

df.submit <- unitest.raw2
df.submit[,130:172] <- unitest.raw1.imp$ximp

sum(is.na(df.submit))
save(df.submit, file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unileverSubmit.RData" )


### ------------------------ Prediction part starts -------------------------------------------

require(caret)  # for modelling


## split training data into train batch and test batch
set.seed(78615)

submit.batch <- df.submit[,-c(1,2)]


#---------------------- fix the factor anomalies in some columns compared to the one used to train ------------

#--get the old train data to factors
uni.raw2[sapply(uni.raw2, is.character)] <- as.data.frame(lapply(uni.raw2[sapply(uni.raw2, is.character)], 
                                                                 as.factor))
uni.raw2[,1] <- as.character(uni.raw2[,1])
uni.raw2[,2] <- as.character(uni.raw2[,2])
str(uni.raw2, list.len=400)


process <- function(x) {
    return(nlevels(x))  
  }           

unitr <- lapply(  uni.raw2[sapply(uni.raw2,is.factor)], process)
unisub <-  lapply(  unitest.raw2[sapply(unitest.raw2,is.factor)], process)
as.numeric(unitr) - as.numeric(unisub)

#--- this doesn't work, hence next 3 statements are used
# levels(unitest.raw2$Amount.of.form) <- levels(uni.raw2$Amount.of.form) 

unitest.raw2[2251, "Amount.of.form"] <- NA
unitest.raw2$Amount.of.form <- as.character(unitest.raw2$Amount.of.form)
unitest.raw2$Amount.of.form <- as.factor(unitest.raw2$Amount.of.form)

levels(unitest.raw2$Overall.Opinion) <- levels(uni.raw2$Overall.Opinion) 
#----------------------------------------------------------------------------------------------------------------


# process <- function(x,y) {
#   if (nlevels(x) != nlevels(y)) {
#     levels(x) <- levels(y)
#     retrun (colnames(x))
#   }
#   else
#     return (ok)  
# }           
# unitr <- lapply(  uni.raw2[sapply(uni.raw2,is.factor)], unitest.raw2[sapply(unitest.raw2,is.factor)], process)



#--------- loads random forest model for prediction
load(file="D:\\Data Mining\\02-Dextra-Unilever\\05012015-Sun-Run\\rftune.RData" )
## Random Forest model
rf.pred <- predict(rf.tune, submit.batch)
## cannot do confusion matrix - as we don't have the values though of the prediction vs original held by unilever
# confusionMatrix(rf.pred, submit.batch$Overall.Opinion)


df.submit.xl <- df.submit
df.submit.xl$Overall.Opinion <- rf.pred


options(scipen=10)
write.table(x=df.submit.xl[,c("ID","Overall.Opinion" )], file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-upload.csv", sep=",", quote=FALSE, row.names = FALSE)
options(scipen=0)
#

#--------- loads random forest model for prediction
load(file="D:\\Data Mining\\02-Dextra-Unilever\\05012015-Sun-Run\\rparttune.RData" )
## Random Forest model
rpart.pred <- predict(rpart.tune, submit.batch)
## cannot do confusion matrix - as we don't have the values though of the prediction vs original held by unilever
# confusionMatrix(rf.pred, submit.batch$Overall.Opinion)


df.submit.rpart.xl <- df.submit
df.submit.rpart.xl$Overall.Opinion <- rf.pred


options(scipen=10)
write.table(x=df.submit.xl[,c("ID","Overall.Opinion" )], file="D:\\Data Mining\\02-Dextra-Unilever\\01-InputsReadOnly\\unilever-upload.csv", sep=",", quote=FALSE, row.names = FALSE)
options(scipen=0)
#








