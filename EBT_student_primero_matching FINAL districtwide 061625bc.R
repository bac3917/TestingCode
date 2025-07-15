rm(list=ls())

library(readxl);library(tidyverse); library(tidyr); library(dplyr)
library(janitor);library(expss); library(stringdist); library(writexl)


# File import/prep --------------------------------------------------------------------

# List all CSV files in the folder
primerofiles <- list.files(path = '//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/Primero', pattern = "\\.csv$", full.names = TRUE)
studentfiles <- list.files(path = '//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/Student upload/batch2', pattern = "\\.csv$", full.names = TRUE)

# Function to read CSV and convert all columns to character

standard_names<-c("sfa_name",   "aun9_3",  'school_name',  "school_code4",  "pasecureid",
                  "student_first_name",  "student_middle_name",  "student_last_name",  "student_dob",
                  "student_address1",  "student_address2",  "student_apt_no",  "student_city",
                  "student_state",  "student_zip",  "eligibility",  "county",
                  "case_number",  "parent_first_name",  "parent_last_name",  "parent_phone",  "parent_email",
                  "enrollment_begin",  "enrollment_end",  "validation_output")

# Function to read, convert to character, and rename columns by position
read_and_align <- function(file, standard_names) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  df[] <- lapply(df, as.character)
  
#see if aun still looks right (in same column)  
  
  # Rename columns by position only if counts match
  if (ncol(df) == length(standard_names)) {
    names(df) <- standard_names
  } else {
    stop(paste("Column count mismatch in file:", file))
  }
  
  return(df)
}


# start merge -------------------------------------------------------------

# Read and combine files using standard column names
#setwd("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/Primero")
#p <- lapply(primerofiles, function(f) read_and_align(f, standard_names)) %>%
 # bind_rows() 

#length(unique(p$aun9_3))  # number of unique AUNs

setwd("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/Student upload")
s <- lapply(studentfiles, function(f) read_and_align(f, standard_names)) %>%
  bind_rows()


# Joining/matching --------------------------------------------------------

#rename with suffix for clarity
s2 <- s %>%
  rename_with(~ paste0("sebt_", .x))

#p2 <- p %>%
  #rename_with(~ paste0("primero_", .x))


# Helper to clean and lowercase variables
clean_keys <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ iconv(as.character(.), from = "", to = "UTF-8", sub = "byte") %>%
                    trimws() %>%
                    tolower()))
}

# Clean both datasets
s2 <- clean_keys(s2, c("sebt_aun9_3", "sebt_pasecureid", "sebt_student_dob", "sebt_student_last_name", "sebt_student_first_name", "sebt_parent_first_name", "sebt_parent_last_name"))
#p2 <- clean_keys(p2, c("primero_aun9_3", "primero_pasecureid", "primero_student_dob", "primero_student_last_name", "primero_student_first_name", "primero_parent_first_name", "primero_parent_last_name"))


#drop any with missing AUN and/or missing secureID
s2 <- s2 %>%
  filter(!is.na(sebt_aun9_3), sebt_aun9_3 != "",
         !is.na(sebt_pasecureid), sebt_pasecureid != "")

p2 <- p2 %>%
 filter(!is.na(primero_aun9_3), primero_aun9_3 != "",
         !is.na(primero_pasecureid), primero_pasecureid != "")

#save the primero files to rds
#saveRDS(p2, file = "//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSprimerofiles/primerofiles.rds")

###load the primero files in
p2 <- readRDS("//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher Input/RDSprimerofiles/primerofiles.rds")

#filter primerofiles to only the ones in the current batch
p2 <- p2 %>% 
  filter(primero_aun9_3 %in% s2$sebt_aun9_3)

# find unmatchable IDs -----------------------------------------------------

# Extract PAsecureID + AUN from Primero and SEBT files
p_ids <- p2 %>%
  filter(!is.na(primero_pasecureid), primero_pasecureid != "",
         !is.na(primero_aun9_3), primero_aun9_3 != "") %>%
  select(primero_aun9_3, primero_pasecureid, primero_student_first_name, primero_student_last_name) %>%
  distinct() %>%
  rename(AUN = primero_aun9_3, PAsecureID = primero_pasecureid, FirstName = primero_student_first_name,
         LastName = primero_student_last_name)

s_ids <- s2 %>%
  filter(!is.na(sebt_pasecureid) & sebt_pasecureid != "" &
         !is.na(sebt_aun9_3) & sebt_aun9_3 != "") %>%
  select(sebt_aun9_3, sebt_pasecureid, sebt_student_first_name, sebt_student_last_name) %>%
  distinct() %>%
  rename(AUN = sebt_aun9_3, PAsecureID = sebt_pasecureid, FirstName = sebt_student_first_name,
         LastName = sebt_student_last_name)

# Compare within matching AUNs
only_in_primero <- anti_join(p_ids, s_ids, by = c("AUN", "PAsecureID")) %>%
  mutate(Source = "Primero")

only_in_sebt <- anti_join(s_ids, p_ids, by = c("AUN", "PAsecureID")) %>%
  mutate(Source = "SEBT")

# Combine and print
unmatched_ids <- bind_rows(only_in_primero, only_in_sebt)  #almost 1.4 million cases?


# perform the join --------------------------------------------------------

#Create join keys
p2 <- p2 %>%
  mutate(join_key1 = paste0(primero_aun9_3, "_",primero_pasecureid),
         join_key2 = paste0(primero_aun9_3, "_", primero_student_first_name, "_", primero_student_last_name, "_", primero_student_dob))

s2 <- s2 %>%
  mutate(join_key1 = paste0(sebt_aun9_3, "_", sebt_pasecureid ),
         join_key2 = paste0(sebt_aun9_3, "_", sebt_student_first_name, "_", sebt_student_last_name, "_", sebt_student_dob))

#decide which key to use
p2 <- p2 %>%
  mutate(join_key = if_else(join_key1 %in% s2$join_key1, join_key1, join_key2))

s2 <- s2 %>%
  mutate(join_key = if_else(join_key1 %in% p2$join_key1, join_key1, join_key2))

#before joining, fix all the sh*t that isnt found in both or is a many-to-many

#unmatched
# Students in p2 not matched in s2
unmatched_p2 <- anti_join(p2, s2, by = "join_key")

# Students in s2 not matched in p2
unmatched_s2 <- anti_join(s2, p2, by = "join_key")

#manytomany
# Count duplicates in each dataset
dups_p2 <- p2 %>% count(join_key) %>% filter(n > 1)
dups_s2 <- s2 %>% count(join_key) %>% filter(n > 1)

# Check overlapping duplicate keys
many_to_many_keys <- inner_join(dups_p2, s2, by = "join_key")






match1 <- inner_join(p2, s2, by = "join_key")



#AUN first: all matches/comparisons should be grouped by AUN (district) before anything! Matching p and s variables for each district -------


#Total student count difference between each district’s file A vs B (indicate which file has more/number missing cases) 
#Print() missing student info [List student detail (all column variables) for students included in district’s file A but not in file B, and vice versa.] --------

# Count students per AUN in each dataset
p_counts <- p2 %>%
  group_by(primero_aun9_3) %>%
  summarise(p_total = n(), .groups = "drop")

s_counts <- s2 %>%
  group_by(sebt_aun9_3) %>%
  summarise(s_total = n(), .groups = "drop")

# Merge and compute difference
student_counts_by_aun <- full_join(p_counts, s_counts,
                                   by = c("primero_aun9_3" = "sebt_aun9_3")) %>%
  rename(AUN = primero_aun9_3) %>%
  mutate(across(c(p_total, s_total), ~replace_na(., 0)),
         difference = p_total - s_total)



#Student eligibility: Number of ‘free’, paid’, and ‘reduced’ for each district for both p and s files 
#Total Count diff for each eligibility type between p and s --------

# Function: count all elig by AUN
count_eligibility <- function(df, aun_col, elig_col, prefix) {
  df %>%
    mutate(elig = tolower(.data[[elig_col]])) %>%
    filter(elig %in% c("free", "reduced", "paid")) %>%
    group_by(.data[[aun_col]], elig) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = elig, values_from = count, values_fill = 0) %>%
    rename_with(~ paste0(prefix, "_", .x), -all_of(aun_col)) %>%
    rename(AUN = all_of(aun_col))
}

# Count in both types
sebt_counts <- count_eligibility(match1, "sebt_aun9_3", "sebt_eligibility", "sebt")
primero_counts <- count_eligibility(match1, "sebt_aun9_3", "primero_eligibility", "primero")

# Merge and get diff
eligibility_counts <- full_join(sebt_counts, primero_counts, by = "AUN") %>%
  mutate(
    diff_free = sebt_free - primero_free,
    diff_paid = sebt_paid - primero_paid,
    diff_reduced = sebt_reduced - primero_reduced,
  )


#PAsecureID?: print() student info for each case where PAsecureID < 10 !!!!! -------------------------------------

#new variable: s10digitID and p10digitID

sIDcounts <- s2 %>%
  mutate(
    s10digitID = case_when(
      is.na(sebt_pasecureid) ~ NA_character_,
      nchar(sebt_pasecureid) == 10 ~ "TRUE",
      TRUE ~ as.character(nchar(sebt_pasecureid))
    )
  )

pIDcounts <- p2 %>%
  mutate(
    p10digitID = case_when(
      is.na(primero_pasecureid) ~ NA_character_,
      nchar(primero_pasecureid) == 10 ~ "TRUE",
      TRUE ~ as.character(nchar(primero_pasecureid))
    )
  )

"sebt10digitID_summary" <- sIDcounts %>%
  group_by(sebt_aun9_3) %>%
  summarise(
    "s10digit_pasecureIDs" = sum(s10digitID == TRUE, na.rm = TRUE),
    s_not_pasecure = sum(!s10digitID %in% c(TRUE, NA), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(AUN = sebt_aun9_3)

"primero10digitID_summary" <- pIDcounts %>%
  group_by(primero_aun9_3) %>%
  summarise(
    "p10digit_pasecureIDs" = sum(p10digitID == TRUE, na.rm = TRUE),
    p_not_pasecure = sum(!p10digitID %in% c(TRUE, NA), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(AUN = primero_aun9_3)

#print students that don’t have the following variables match between s and p: --------

# siteID -----------------------------------------------------------------

school_code_mismatches <- match1 %>%
  filter(!is.na(sebt_school_code4), !is.na(primero_school_code4)) %>%
  filter(sebt_school_code4 != primero_school_code4) %>%
  group_by(sebt_aun9_3) %>%
  summarise(school_code_mismatch_count = n(), .groups = "drop") %>%
  rename(AUN = sebt_aun9_3)

# parent guardian first and last ------------------------------------------

parent_name_mismatches <- match1 %>%
  filter(
    !is.na(sebt_parent_first_name), !is.na(primero_parent_first_name),
    !is.na(sebt_parent_last_name), !is.na(primero_parent_last_name)
  ) %>%
  filter(
    tolower(trimws(sebt_parent_first_name)) != tolower(trimws(primero_parent_first_name)) |
      tolower(trimws(sebt_parent_last_name)) != tolower(trimws(primero_parent_last_name))
  ) %>%
  group_by(sebt_aun9_3) %>%
  summarise(parent_name_mismatch_count = n(), .groups = "drop") %>%
  rename(AUN = sebt_aun9_3)



# student address ---------------------------------------------------------

standardize_address <- function(x) {
  x %>%
    tolower() %>%
    trimws() %>%
    str_replace_all("\\bstreet\\b", "st") %>%
    str_replace_all("\\bavenue\\b", "ave") %>%
    str_replace_all("\\bdrive\\b", "dr") %>%
    str_replace_all("\\broad\\b", "rd") %>%
    str_replace_all("\\blane\\b", "ln") %>%
    str_replace_all("\\bcourt\\b", "ct") %>%
    str_replace_all("\\bplace\\b", "pl") %>%
    str_replace_all("\\bboulevard\\b", "blvd") %>%
    str_replace_all("\\.", "")
}

#similarity score
match1 <- match1 %>%
  mutate(
    addr_similarity = ifelse(
      !is.na(sebt_student_address1) & sebt_student_address1 != "" &
        !is.na(primero_student_address1) & primero_student_address1 != "",
      stringdist::stringsim(
        standardize_address(sebt_student_address1),
        standardize_address(primero_student_address1),
        method = "jw" #jarowinkler
      ), NA_real_))

address_mismatches <- match1 %>%
  filter(!is.na(addr_similarity)) %>%
  filter(addr_similarity < 0.9) %>%
  group_by(sebt_aun9_3) %>%
  summarise(address_mismatch_count = n(), .groups = "drop") %>%
  rename(AUN = sebt_aun9_3)


# case number -------------------------------------------------------------

case_number_mismatches <- match1 %>%
  filter(!is.na(sebt_case_number), !is.na(primero_case_number)) %>%
  filter(sebt_case_number != primero_case_number) %>%
  group_by(sebt_aun9_3) %>%
  summarise(case_number_mismatch_count = n(), .groups = "drop") %>%
  rename(AUN = sebt_aun9_3)


# print summary for masterdatasheet ---------------------------------------

summarydata_all <- student_counts_by_aun %>%
  full_join(eligibility_counts, by = "AUN") %>%
  full_join(primero10digitID_summary, by = "AUN") %>%
  full_join(sebt10digitID_summary, by = "AUN") %>%
  full_join(school_code_mismatches, by = "AUN") %>%
  full_join(parent_name_mismatches, by = "AUN") %>%
  full_join(address_mismatches, by = "AUN") %>%
  full_join(case_number_mismatches, by = "AUN") %>%
  arrange(AUN) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))


#2 main outputs so far:
view(summarydata_all) #summary data to be put in master sheet
view(unmatched_ids) #list of students that could not be analyzed due to mismatched IDs


# export as excel file ----------------------------------------------------

output_folder_path <- "//192.168.1.68/Research_and_Evaluation_Group/CSC_Initiatives/NKH/data_and_analysis/data/Filematcher output"  # Change this to your actual path

## CHANGE BASED ON BATCH
filename <- "batch1_districtdata.xlsx" #change based on batch range (default should be soemthing like "district_data") MAKE SURE ITS .XLSX
## CHANGE BASED ON BATCH

output_folder <- file.path(output_folder_path, filename)
write_xlsx(summarydata_all, path = output_folder)
cat("File saved to:", output_folder, "\n")

# added this now 1049am

#### check with enrollment data

#paleas <- read_excel("//csc-profiles/Research_and_Evaluation_Group/GENERAL/Data/PA_Data/PDE/202425/pde_lea_enrollment_202425.xlsx")
#paleas<-paleas %>% select(AUN,County,Total)

#temp<-s %>% 
  #mutate(AUN=str_replace_all(aun9_3,"-","")) %>% 
 # select(AUN,sfa_name,pasecureid,county) %>%
 # left_join(paleas,by=c("AUN")) %>%
 # group_by(sfa_name) %>% summarise(SEBT_Count=n(), Official_Enrollment=first(Total))
#temp %>% select(sfa_name,SEBT_Count,Official_Enrollment) %>% htmlTable()
