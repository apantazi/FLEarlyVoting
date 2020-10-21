library(erer)
library(tidyverse)
library(lubridate)
library(janitor)
library(naniar)
library(forcats)
library(scales)

memory.limit(size=80000)
setwd("C:/Users/Andrew/Downloads/EVL_10866/")

## Reading in ---------------------------------

#read in in-person early voter file (https://countyballotfiles.floridados.gov/VoteByMailEarlyVotingReports/PublicStats)
early_voters <- read_delim("EVL_10866_10212020_082114752.txt",  delim = '\t',escape_double = FALSE, trim_ws = TRUE, col_names=c("RecordType",	"CountyId",	"FvrsElectionNumber",	"ElectionDate",	"ElectionName",	"Voter_ID",	"VoterName",	"AbsPrecinct",	"PrecinctSplit",	"CongressionalDistrict",	"SenateDistrict",	"HouseDistrict",	"CountyCommissionDistrict",	"SchoolBoardDistrict",	"OtherDistricts",	"AbsParty",	"Location",	"DateofEarlyVote"))

#create list of voters
voter_id <- early_voters$Voter_ID

#read in voter history
voterhistory <- read_delim("C:/Users/Andrew/Desktop/20201013_VoterHistory/combine.txt",  delim = '\t',escape_double = FALSE, trim_ws = TRUE, col_names=c("County", "Voter_ID", "Elex_Date", "Election_Type", "ballot"))

#filter voter history
voterhistory <- voterhistory %>% 
  filter(Voter_ID %in% voter_id)



#read in voter file
voters <- read_delim("C:/Users/Andrew/Desktop/20201013_VoterDetail/combine.txt",  delim = '\t',escape_double = FALSE, trim_ws = TRUE, col_names=c("County", "Voter_ID", "Name_Last", "Name_Suffix", "Name_First", "Name_Middle", "PRR_exempt", "Residence_1", "Residence_2", "Residence_City", "Residence_State", "Residence_Zip", "Mailing_Address_1", "Mailing_Address_2", "Mailing_Address_3", "Mailing_City", "Mailing_State", "Mailing_Zip", "Mailing_Country", "Sex", "Race", "DOB", "Reg_Date", "Party", "Precinct", "Precinct_Group", "Precinct_Split", "Precinct_Suffix", "Voter_Status", "Congressional_District", "House_District", "Senate_District", "County_Commission_District", "School_Board_District", "Area_Code", "Phone", "Phone_ext", "Email"))

#filter voter file
voters <- voters %>% 
  filter(Voter_ID %in% voter_id)

## transforming =================================
#voterhistory elex date
voterhistory <- voterhistory %>% mutate(Elex_Date = mdy(Elex_Date))

#transform reg-date and DOB into date formats
voters <- voters %>% mutate(Reg_Date = mdy(Reg_Date)) 
voters <- voters %>% mutate(DOB = mdy(DOB))

#Create age as of registration
voters$reg_age <- interval(voters$DOB, voters$Reg_Date) / years(1)
#create age
voters$age <- interval(voters$DOB,Sys.Date()) / years(1)

#Create four party categories
voters$simple_party = ""
voters$simple_party[voters$Party == "REP"] <- "REP"
voters$simple_party[voters$Party =="DEM"] <- "DEM"
voters$simple_party[voters$simple_party == ""] <- "Other"

#create common language race categories
voters$Race[voters$Race == "1"] <- "American Indian or Alaskan Native"
voters$Race[voters$Race == "2"] <- "Asian or Pacific Islander"
voters$Race[voters$Race == "3"] <- "Black, Not Hispanic"
voters$Race[voters$Race == "4"] <- "Hispanic"
voters$Race[voters$Race == "5"] <- "White, Not Hispanic"
voters$Race[voters$Race == "6"] <- "Other"
voters$Race[voters$Race == "7"] <- "Multi-racial"
voters$Race[voters$Race == "9"] <- "Unknown"

#generations
voters$Generation = ""
voters$Generation[voters$DOB <= as.Date("1928-12-31")] <- "Greatest Generation"
voters$Generation[voters$DOB <= as.Date("1945-12-31") & voters$DOB >= as.Date("1929-01-01")] <- "Silent Generation"
voters$Generation[voters$DOB <= as.Date("1964-12-31") & voters$DOB >= as.Date("1946-01-01")] <- "Baby Boomers"
voters$Generation[voters$DOB <= as.Date("1980-12-31") & voters$DOB >= as.Date("1965-01-01")] <- "Gen X"
voters$Generation[voters$DOB <= as.Date("1996-12-31") & voters$DOB >= as.Date("1981-01-01")] <- "Millenial"
voters$Generation[voters$DOB >= as.Date("1997-01-01")] <- "Gen Z"

#white v nonwhite
voters$simplerace = ""
voters$simplerace[voters$Race == "White, Not Hispanic"] <- "White, Not Hispanic"
voters$simplerace[voters$simplerace == ""] <- "Non-White"

#registration year
voters$Reg_Year <- year(voters$Reg_Date)

#Transform history code into plain english
voterhistory$ballot[voterhistory$ballot =="A"] <- "VbM"
voterhistory$ballot[voterhistory$ballot =="B"] <- "VbM Not Counted"
voterhistory$ballot[voterhistory$ballot =="E"] <- "Voted Early"
voterhistory$ballot[voterhistory$ballot =="N"] <- "Did Not Vote"
voterhistory$ballot[voterhistory$ballot =="P"] <- "Provisional Not Counted"
voterhistory$ballot[voterhistory$ballot =="Y"] <- "Voted at Polls"
voterhistory$ballot[is.na(voterhistory$ballot)] <- "Did Not Vote"

#Transform history code into simple_ballot
voterhistory$simple_ballot = ""
voterhistory$simple_ballot[voterhistory$ballot =="VbM"] <- as.numeric(1)
voterhistory$simple_ballot[voterhistory$ballot =="VbM Not Counted"] <- as.numeric(0)
voterhistory$simple_ballot[voterhistory$ballot =="Voted Early"] <- as.numeric(1)
voterhistory$simple_ballot[voterhistory$ballot =="Did Not Vote"] <- as.numeric(0)
voterhistory$simple_ballot[voterhistory$ballot =="Provisional Not Counted"] <- as.numeric(0)
voterhistory$simple_ballot[voterhistory$ballot =="Voted at Polls"] <- as.numeric(1)
voterhistory$simple_ballot[is.na(voterhistory$ballot)] <- as.numeric(0)

voterhistory <- voterhistory %>% 
  mutate(simple_ballot = as.numeric(simple_ballot))

#age in whole integers
voters$age_floor <- floor(voters$age)


## joining dataframes ############################# 

voterhistory <- left_join(early_voters,voterhistory,by="Voter_ID")
voterhistory <- left_join(voterhistory,voters,by="Voter_ID")
rm(early_voters)
voterhistory$Generation[is.na(voterhistory$Generation)] <- ""


### alternative filters for smaller DFs ############################# 
#only recent elex
voterhistory <- voterhistory %>% 
  filter(Elex_Date == as.Date("2020-08-18"))

#just major elections
voterhistory <- voterhistory %>% 
  filter(Elex_Date == as.Date("2020-08-18")| Elex_Date == as.Date("2020-03-17") | Elex_Date == as.Date("2018-11-06")| Elex_Date == as.Date("2018-08-28")| Elex_Date == as.Date("2016-11-08")| Elex_Date == as.Date("2016-08-30")| Elex_Date == as.Date("2016-03-15")| Elex_Date == as.Date("2014-11-04")| Elex_Date == as.Date("2014-08-26")| Elex_Date == as.Date("2012-11-06")| Elex_Date == as.Date("2012-08-14")| Elex_Date == as.Date("2012-01-31")| Elex_Date == as.Date("2010-11-02")| Elex_Date == as.Date("2010-08-24")| Elex_Date == as.Date("2008-11-04")| Elex_Date == as.Date("2008-08-26")| Elex_Date == as.Date("2008-01-29"))

## create grouped DFs for charting ############################# 

voterhistoryrace <- voterhistory %>% 
  group_by(Voter_ID,Race) %>% 
  summarise(total = sum(simple_ballot)) %>%
  arrange(desc(total))

voterhistoryparty <- voterhistory %>% 
  group_by(Voter_ID,simple_party) %>% 
  summarise(total = sum(simple_ballot)) %>%
  arrange(desc(total))

voterhistorygeneration <- voterhistory %>% 
  group_by(Voter_ID,Generation) %>% 
  summarise(total = sum(simple_ballot)) %>%
  arrange(desc(total))

voterhistorygroup$firsttime = "First Time"
voterhistorygroup$firsttime[voterhistorygroup$total > 0] <- "Not First Time"
voterhistorygroup$firsttime[voterhistorygroup$total == 0] <- "First Time"

voterhistoryrace$firsttime = "First Time"
voterhistoryrace$firsttime[voterhistoryrace$total > 0] <- "Not First Time"
voterhistoryrace$firsttime[voterhistoryrace$total == 0] <- "First Time"

voterhistoryparty$firsttime = "First Time"
voterhistoryparty$firsttime[voterhistoryparty$total > 0] <- "Not First Time"
voterhistoryparty$firsttime[voterhistoryparty$total == 0] <- "First Time"

voterhistorygeneration$firsttime = "First Time"
voterhistorygeneration$firsttime[voterhistorygeneration$total > 0] <- "Not First Time"
voterhistorygeneration$firsttime[voterhistorygeneration$total == 0] <- "First Time"

voterhistory_first <- voterhistorygroup %>% 
  group_by(firsttime,CountyId) %>% 
  summarise(total= n()) %>% 
  arrange(desc(total))

voterhistory_first_race <- voterhistoryrace %>% 
  group_by(firsttime,Race) %>% 
  summarise(total= n()) %>% 
  arrange(desc(total))

voterhistory_first_party <- voterhistoryparty %>% 
  group_by(firsttime,simple_party) %>% 
  summarise(total= n()) %>% 
  arrange(desc(total))

voterhistory_first_generation <- voterhistorygeneration %>% 
  group_by(firsttime,Generation) %>% 
  summarise(total=n()) %>% 
  arrange(desc(total))


## Creating Charts #############################
#GGPLOT creator
esquisse::esquisser()

#County Charts
voterhistory_first %>% 
  mutate(CountyId = fct_reorder(CountyId, desc(total))) %>%
  ggplot(aes(x = CountyId, fill = firsttime, weight = total)) +
  geom_bar() +
  scale_fill_hue() +
  labs(title = "New voters make up 1 in 11 early voters") +
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),legend.position = c(0.8, 0.35),legend.title=element_blank(),
        legend.background = element_rect(fill="aliceblue",size=.75, linetype="solid",colour ="darkgray"),
        axis.text.x=element_text(color = "grey20", family="sans", size = 9, hjust = .5, vjust = .5, face = "plain"),axis.title.x=element_blank(),plot.title=element_text(hjust=.5,size=16))

#Race Charts
voterhistory_first_race %>% 
  mutate(Race = fct_reorder(Race, desc(total))) %>%
  ggplot(aes(x = Race, fill = firsttime, weight = total)) +
  geom_bar() +
  scale_fill_hue() +
  scale_y_continuous(label=comma)+
  labs(title = "New voters make up 1 in 11 early voters") +
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),legend.position = c(0.8, 0.4),legend.title=element_blank(),
        legend.background = element_rect(fill="aliceblue",size=.75, linetype="solid",colour ="darkgray"),
        axis.text.x=element_text(color = "grey20", family="sans", size = 9, hjust = .5, vjust = .5, face = "plain"),axis.title.x=element_blank(),plot.title=element_text(hjust=.5,size=16))

#Party Charts
voterhistory_first_party %>% 
  mutate(simple_party = fct_reorder(simple_party, desc(total))) %>%
  ggplot(aes(x = simple_party, fill = firsttime, weight = total)) +
  geom_bar() +
  scale_fill_hue() +
  scale_y_continuous(label=comma)+
  labs(title = "New voters make up 1 in 11 early voters") +
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),legend.position = c(0.85, 0.35),legend.title=element_blank(),
        legend.background = element_rect(fill="aliceblue",size=.75, linetype="solid",colour ="darkgray"),
        axis.text.x=element_text(color = "grey20", family="sans", size = 9, hjust = .5, vjust = .5, face = "plain"),axis.title.x=element_blank(),plot.title=element_text(hjust=.5,size=16))


#create sorted generations
voterhistory_first_generation$Generation <- factor(voterhistory_first_generation$Generation,levels=c("Greatest Generation","Silent Generation","Baby Boomers","Gen X","Millenial","Gen Z",""))

#Generations charts
voterhistory_first_generation %>% 
  ggplot(aes(x = Generation, fill = firsttime, weight = total)) +
  geom_bar() +
  scale_fill_hue() +
  scale_y_continuous(label=comma)+
  labs(title = "New voters make up 1 in 11 early voters") +
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),legend.position = c(0.85, 0.35),legend.title=element_blank(),
        legend.background = element_rect(fill="aliceblue",size=.75, linetype="solid",colour ="darkgray"),
        axis.text.x=element_text(color = "grey20", family="sans", size = 9, hjust = .5, vjust = .5, face = "plain"),axis.title.x=element_blank(),plot.title=element_text(hjust=.5,size=16))
