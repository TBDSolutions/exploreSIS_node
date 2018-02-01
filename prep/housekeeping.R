
# Cleaning and recoding of local, PIHP-specific variables
# Anything that's not accomplished by 'readSIS.R' script from exploreSIS repo

library(stringdist); library(tidyr); library(magrittr); library(dplyr)

##### INTERVIEWER FIELDS #####

# Recode interviewers using fuzzy string matching

interviewer_codes <-
c("A Macandog","A Koch","E Purdey","T Stuckey","T Perez","C Blanton","J Widzinski",
"T Jones","L Wright","C Osborne","S Castle","N Muta","M Teall","P Primel",
"B Phillips","R Moore","S Goodlock","R Sandborn","C Demski","S ONeill",
"G Olson","C Dombrowski","S Fry","A Anderson","F Taylor","N Holtz",
"C Jankwietz","S Olson","C Patterson","K Heltzel","S Wyzgoski","L Nichols",
"K Macco","L Burlingame","R Misiak","L Good","P Dever","R Leininger",
"J Whitcher","J Strong","S Oliver","K Kramer","C Grounds","C Macrae",
"E Brouwer","A Giuffre","J Tambs","L Wisneski","K McDonnell","L Wood",
"T Haase","D Maat","S Crayne","A Fuhrman","S Roy","J Folkins","C Marger",
"S Johnson","N Barkey","C Davis","D Morgan","L OHare","G Nelson","A Freshour",
"E DeLeon","H DiMatteo","T Taylor","M Afariogun","A Berryhill","V Gabby",
"M Mellin","A Halsted","L Sobol","H Hawkins","K Lentz","J Bohne",
"L Baumgart","G Miller","J Werth","C Wallace","D Evans","J Morrill","P TenBrink",
"A Brink","J Dils","B Huben","S Steinberg","E Podsiadly","P Taylor",
"C Crawford","T Lewicki","J McGhee","A Crissman","D Akridge","A Pfaendtner",
"L Bowman","J Chrivia","C Coachman","K Lord","C Kuyava","L Griffin",
"E Chester","H Harrison","R Freitag", "R Sporer","A Adamski","E Nelson","F Nekola",
"A Hockstad","C Hull","K Parsell","N Martinez","T Rutgers","M Wilhelm",
"S Viskantes","S Alston","R Thompkins","J Myers","S Gordon","O Chojnacki",
"V Lesauvage","M Bloomfield","J Frei","M Johnson","L Brink","S Biondo",
"R Mack","A Pollard","J Krizan", "K Smith")

# interviewer_codes <-
#   c("Anna Macandog","Amanda Koch","Emily Purdey","T'Challa Stuckey","T Perez","Cheri Blanton","Jessica Widzinski",
#     "Tim Jones","Lauren Wright","Carolyn Osborne","Sandy Castle","Najla Muta","Megan Teall","Paul Primel",
#     "Brande Phillips","Ralynda Moore","Shirley Goodlock","Rosemary Sandborn","Corie Demski","Sandy ONeill",
#     "Grace Olson","Caitlin Dombrowski","Sarah Fry","Alicia Anderson","Frank Taylor","Natalie Holtz",
#     "Connie Jankwietz","Sally Olson","Carolyn Patterson","Kristin Heltzel","Scott Wyzgoski","Leslie Nichols",
#     "Kelly Macco","Lisa Burlingame","Rachel Misiak","Lindsey Good","Patrick Dever","Ryan Leininger",
#     "Julia Whitcher","Jovan Strong","Shannon Oliver","Katie Kramer","Chase Grounds","Carol Macrae",
#     "Elizabeth Brouwer","Anthony Giuffre","James Tambs","Lauren Wisneski","Katy McDonnell","Lynda Wood",
#     "Tamara Haase","David Maat","Sarah Crayne","Adam Fuhrman","Stephanie Roy","Janet Folkins","Christina Marger",
#     "Sherrice White-Johnson","Nora Barkey","Christine Davis","Donna Morgan","Lori OHare","Grace Nelson","Austin Freshour",
#     "Ellie DeLeon","Hannah DiMatteo","Tradina Taylor","Meta Afariogun","Amie Berryhill","Virginia Gabby",
#     "Mary Beth Mellin","Alyson Halsted","Linda Sobol","Heather Hawkins","Kathy Lentz","Jennifer Bohne",
#     "Lori Baumgart","Gregory Miller","Jade Werth","Carol Wallace","Diana Evans","John Morrill","Pam TenBrink",
#     "Adam Brink","Jennifer Dils","Bill Huben","Shanna Steinberg","Erica Podsiadly","Priscilla Taylor",
#     "Cherlyn Crawford","Todd Lewicki","James McGhee","Aubrie Crissman","Denise Akridge","Alexandria Pfaendtner",
#     "Lorraine Bowman","Joshua Chrivia","Charlotte Coachman","Katie Lord","Carol Kuyava","Lena Griffin",
#     "Elizabeth Chester","Holly Harrison","Rhea Freitag", "Renee Sporer","Amy Adamski","Emma Nelson","Faith Nekola",
#     "Abbie Hockstad","Chelsey Hull","Kristin Parsell","Natalie Martinez","Theresa Rutgers","Marcia Wilhelm",
#     "Shawna Viskantes","Stephanie Alston","R Thompkins","J Myers","S Gordon","O Chojnacki","V Lesauvage","M Bloomfield",
#     "J Frei","M Johnson","L Brink","S Biondo","R Mack","A Pollard","J Krizan")


# Remove e-mail address suffixes from interviewer fields

remove_these <- c(
  "@.*","ausable","BABHA","babha","CEI","cls","cmhcm","copper","gccmha","gihn","gogebic",
  "hbh","hiawatha","ICCMH","lifeways","macomb","mcbh","morc","NCMH","network180","newaygo",
  "northpointe","northeast","oakland","pathways","SCCMH","scmha","sisonline","tbh","wash",
  "pce","txp","mshn","[[:punct:]]","[0-9]"
)

sub_sis$interviewer_orig_regex <- 
gsub(
  paste(remove_these,collapse = "|"),"", 
  sub_sis$interviewer_orig, ignore.case = T
)

sub_sis$interviewer_regex <- 
gsub(
  paste(remove_these,collapse = "|"),"", 
  sub_sis$interviewer, ignore.case = T
)

interviewer_raw <- unique(c(as.character(sub_sis$interviewer_orig_regex),
                            as.character(sub_sis$interviewer_regex)))

interviewer_ref_jw <- 
  # Create distance matrix computing string diff
  stringdistmatrix(
    interviewer_raw, interviewer_codes, 
    method = 'jw', p = 0.1, useNames = T
  )  %>%
  as.data.frame() %>%
  mutate(rawtext = row.names(.)) %>%
  group_by(rawtext) %>%
  gather(coded_jw,score_jw,-rawtext) %>%
  mutate(rank = row_number(score_jw)) %>%
  # select match with lowest (best) score
  filter(rank == min(rank)) %>%
  select(-rank)

interviewer_ref_lcs <- 
  # Create distance matrix computing string diff
  stringdistmatrix(
    interviewer_raw, interviewer_codes, 
    method = 'lcs', useNames = T
  )  %>%
  as.data.frame() %>%
  mutate(rawtext = row.names(.)) %>%
  group_by(rawtext) %>%
  gather(coded_lcs,score_lcs,-rawtext) %>%
  mutate(rank = row_number(score_lcs)) %>%
  # select match with lowest (best) score
  filter(rank == min(rank)) %>%
  select(-rank)

interviewer_ref_osa <- 
  # Create distance matrix computing string diff
  stringdistmatrix(
    interviewer_raw, interviewer_codes, 
    method = 'osa', useNames = T
  )  %>%
  as.data.frame() %>%
  mutate(rawtext = row.names(.)) %>%
  group_by(rawtext) %>%
  gather(coded_osa,score_osa,-rawtext) %>%
  mutate(rank = row_number(score_osa)) %>%
  # select match with lowest (best) score
  filter(rank == min(rank)) %>%
  select(-rank)

interviewer_ref <-
  interviewer_ref_jw %>%
  left_join(interviewer_ref_lcs, by = "rawtext") %>%
  left_join(interviewer_ref_osa, by = "rawtext") %>%
  mutate(
    all_match = coded_jw == coded_lcs & coded_lcs == coded_osa,
    # Keep only output where the top-ranking match 
    # from all three algorithms match each other
    interviewer_coded = case_when(
      all_match == T ~ coded_jw
    )
  ) %>%
  filter(is.na(interviewer_coded) == F) %>%
  select(rawtext,interviewer_coded) %>%
  mutate(interviewer_coded = ifelse(rawtext == 'olson' | rawtext == 'cjcourtney',
                                    'Unable to map',
                                    ifelse(rawtext == 'solson',
                                           'S Olson',
                                           interviewer_coded)
        )
  )

  ## identify instances when fuzzy matching needs manual review
interviewer_review <-
  interviewer_ref_jw %>%
  left_join(interviewer_ref_lcs, by = "rawtext") %>%
  left_join(interviewer_ref_osa, by = "rawtext") %>%
  mutate(
    all_match = coded_jw == coded_lcs & coded_lcs == coded_osa,
    # Keep only output where the top-ranking match 
    # from all three algorithms match each other
    interviewer_coded = case_when(
      all_match == T ~ coded_jw
    )
  ) %>%
  filter(is.na(interviewer_coded) == T) %>%
  # manually map
  mutate(
    interviewer_coded = case_when(
      rawtext %in% c("","mi","nelson","sis") ~ "Unable to map",
      rawtext == "caitlindombrowski" ~ "C Dombrowski",
      rawtext == "amandakoch" ~ "A Koch",
      rawtext == "renees" ~ "R Sporer",             
      rawtext == "emilyp" ~ "E Purdey",             
      rawtext %in% c("lohare","      lohare","lohare ") ~ "L OHare",           
      rawtext == "woodl" ~ "L Wood",      
      rawtext == "alexandriapfaendtner" ~ "A Pfaendtner",
      rawtext %in% c("oneill","sonelil","soneill") ~ "S ONeill",
      rawtext == "chasegrounds" ~ "C Grounds",
      rawtext == "katiekramer" ~ "K Kramer",
      rawtext == "lisab" ~ "L Burlingame",
      rawtext == "swhite" ~ "S Johnson", #Sherrice White-Johnson       
      rawtext == "kmcdonne" ~ "K McDonnell",
      rawtext == "sandycastle" ~ "S Castle",
      rawtext == "tradinat" ~ "T Taylor",
      rawtext == "carolynosborne" ~ "C Osborne",
      rawtext == "sarahcrayne" ~ "S Crayne",    
      rawtext == "gnelson" ~ "G Nelson",
      rawtext == "enelson" ~ "E Nelson",
      rawtext == "elliedeleon" ~ "E DeLeon",
      rawtext %in% c("kellym","cccccckellymacco","kmaccogenhsorg") ~ "K Macco",
      rawtext %in% c("lindseygood","lwlindseygood") ~ "L Good",
      rawtext == "megant" ~ "M Teall",
      rawtext %in% c("bohnejennifer","jenniferbohne") ~ "J Bohne",
      rawtext == "stephanieroy" ~ "S Roy",
      rawtext == "rosemarysandborn" ~ "R Sandborn",
      rawtext %in% c("chrisdavis","cdigiacintodavis") ~ "C Davis",
      rawtext %in% c("coriedemski","coriedemsi") ~ "C Demski",
      rawtext == "kristinheltzel" ~ "K Heltzel",
      rawtext == "faithn" ~ "F Nekola",
      rawtext == "janetfolkins" ~ "J Folkins",
      rawtext == "metaa" ~ "M Afariogun",
      rawtext == "solivericareallianceorg" ~ "S Oliver",
      rawtext == "rachaelmack" ~ "R Mack",
      rawtext == "troyperez" ~ "T Perez",
      rawtext == "gregorymiller" ~ "G Miller",
      rawtext == "najlamuta" ~ "N Muta",
      rawtext == "deniseakridge" ~ "D Akridge",
      rawtext == "christinamarger" ~ "C Marger",
      rawtext == "taswhitestuckey" ~ "T Stuckey",
      rawtext == "kristiesmith" ~ "K Smith"
    )
  ) %>%
  select(rawtext, interviewer_coded)

# identify new names to be manually mapped
if(sum(is.na(interviewer_review$interviewer_coded)==T)>0) {
paste("The names below require manual review")
interviewer_review %>%
  filter(is.na(interviewer_coded)==T) %>%
  select(rawtext) 
} else paste("All names have been mapped")

# join interviwer_review back to interviewer_ref
interviewer_ref <- rbind(interviewer_ref,interviewer_review)


rm(interviewer_ref_jw); rm(interviewer_ref_lcs); rm(interviewer_ref_osa)


## Clean interviewer values in SIS

sub_sis <-
  sub_sis %>%
  # Map orig interviewer values to fuzzy-matched values (filtered for high similarity)
  left_join(interviewer_ref, by = c("interviewer_orig_regex" = "rawtext")) %>%
  rename("interviewer_orig_coded" = "interviewer_coded") %>% # rename joined field to avoid duplicates below
  # Map orig interviewer values to fuzzy-matched values (filtered for high similarity)
  left_join(interviewer_ref, by = c("interviewer_regex" = "rawtext")) %>%
  rename(
    "interviewer_raw" = "interviewer",
    "interviewer_orig_raw" = "interviewer_orig"
  ) %>%
  mutate(interviewer = ifelse(is.na(interviewer_orig_coded),
                              yes = interviewer_coded,
                              no = interviewer_orig_coded))
  

##### AGENCY FIELD #####

# Map Agency variants using fuzzy string matching

# Remove common sub-strings
sub_sis$agency_mod <- gsub(" CMH| County| Behavioral Health| Community| Network",
                           "", sub_sis$agency, ignore.case = TRUE)
# Remove prefixes with ' in ', such as 'CLS in Oakland'
sub_sis$agency_mod <- gsub(".+? in ","",sub_sis$agency_mod, ignore.case = TRUE)
sub_sis$agency_mod <- tolower(sub_sis$agency_mod)

agency_codes <- unique(totals$agency)

agency_raw <- unique(as.character(sub_sis$agency_mod))

agency_ref <- 
  # Create distance matrix computing string diff
  stringdistmatrix(agency_raw, agency_codes, 
                   method='jw', p=0.1, useNames = T)  %>%
  as.data.frame() %>%
  mutate(rawtext = row.names(.)) %>%
  group_by(rawtext) %>%
  gather(coded,score,-rawtext) %>%
  # select match with lowest (best) score
  filter(score == min(score)) %>%
  # keep only the most accurate
  filter(score <= 0.30) %>%
  select(rawtext,coded)

## Combine reference sets for joins

ref <- rbind(interviewer_ref,agency_ref)

## Clean agency values in SIS

sub_sis <-
sub_sis %>%
  # Rename 'agency' field as 'sub_agency', since we'll group these at CMH level
  rename(sub_agency = agency) %>%
  mutate(sub_agency = as.character(sub_agency)) %>%
  # Map agency values to fuzzy-matched values (filtered for high similarity)
  left_join(ref, by = c("agency_mod" = "rawtext")) %>%
  rename(agency = coded) %>%
  mutate(agency = ifelse(is.na(agency) == T,
                         sub_agency,
                         agency)) %>%
  # recode remaining variants
  mutate(agency = dplyr::recode(agency,
                                `Community Living Services` = "Detroit-Wayne",
                                `Muskegon County CMH` = "HealthWest",
                                `CEI CMH` = "CEI",
                                `Manistee-Benzie CMH` = "Centra Wellness",
                                `Pines CMH` = "Pines (Branch)",
                                `Montcalm Center for Behavioral Health ` = "Montcalm")) %>%
  # Remove agencies not in master list
  mutate(
    agency = ifelse(agency %in% unique(totals$agency),agency,NA),
    agency = as.factor(agency)
  ) %>%
  select(-agency_mod)

##### PIHP FIELDS #####

# Map PIHP variants by recoding

sub_sis %<>%
  mutate(PIHP = dplyr::recode(PIHP,
                       `Region 1 PIHP (NorthCare Network PIHP)` = "NorthCare",
                       `Region 2 PIHP (Northern MI Regional Entity PIHP)` = "Northern MI",
                       `Region 3 PIHP (Lakeshore Regional Entity PIHP)` = "Lakeshore",
                       `Region 4 PIHP (Southwest MI Behavioral Health PIHP)` = "Southwest",
                       `Region 5 PIHP (Mid-State Health Network PIHP)` = "Mid-State",
                       `Region 6 PIHP (CMH Partnership of SE MI PIHP)` = "CMH SE MI",
                       `Region 7 PIHP (Detroit Wayne Mental Health Authority)` = "Detroit-Wayne",
                       `Region 8 PIHP (Oakland County CMH Authority)` = "Oakland",
                       `Region 9 PIHP (Macomb County CMH Services)` = "Macomb",
                       `Region 10 PIHP (Region 10 PIHP)` = "Region 10"))

##### REMOVE TEMP TABLES from WRKSPACE

rm(ref)
rm(agency_ref); rm(agency_codes); rm(agency_raw)
rm(interviewer_ref); rm(interviewer_codes); rm(interviewer_raw)

##### FILTER OUT ASSESSMENTS W/ MISSING FIELDS

print(
  paste0(
    sum(is.na(sub_sis$mcaid_id) == T),
    " assessments will be removed due to missing Medicaid IDs"
  )
)

print(
  paste0(
    sum(is.na(sub_sis$agency) == T),
    " assessments will be removed due to missing agency fields"
  )
)

print(
  paste0(
    sum(is.na(sub_sis$interviewer) == T),
    " assessments will be removed due to missing interviewer IDs"
  )
)

exclusions <-
  sub_sis %>%
  filter(
    is.na(agency) == T 
    | is.na(interviewer) == T
    | is.na(mcaid_id) == T
  ) %>%
  select(sis_id,mcaid_id,agency,interviewer,sis_date)
  

sub_sis %>%
  filter(
    is.na(agency) == F 
    & is.na(interviewer) == F
    & is.na(mcaid_id) == F
  )