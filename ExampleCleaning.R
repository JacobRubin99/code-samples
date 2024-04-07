# filename: Cleaning.R
# author: Jacob Rubin
# description: Create Clean Dataset from hd and sfa files

####################################################
## Part 1 - Load Datafiles and Create Panel Data ##
####################################################

# If not already installed, call install.packages("tidyverse")
library(tidyverse)

# modify location of your directory as needed
setwd("/Users/jacobrubin/Desktop/Spring 2024 MIT Blueprint Labs Data Task/") 

### HD DATA PREPARATION

# Get file paths for HD
hd_file_paths <- list.files("Data/schools", pattern = "\\.csv$", full.names = TRUE)

# Function to add "year" column to each dataset
read_and_label <- function(file_name) {
  year <- sub(".*?(\\d{4})\\.csv", "\\1", file_name)
  read_csv(file_name) %>%
    mutate(year = year)
}

# Apply the function to each file and create a panel dataset
hd_panel_data <- bind_rows(lapply(hd_file_paths, read_and_label))

### SFA DATA PREPARATION

# Read sfa file
sfa1015 <- read.csv("Data/students/sfa1015.csv")

# Convert sfa1015 to long format
sfa_long <- sfa1015 %>% 
  pivot_longer(
    cols = -unitid, 
    names_to = c(".value", "year"), 
    names_pattern = "(scugrad|scugffn|scugffp|fgrnt_p|fgrnt_a|sgrnt_p|sgrnt_a)(\\d+)"
  )

### Join HD and SFA Data files on unitid and year
panel_data <- inner_join(hd_panel_data, sfa_long, by = c("unitid", "year"))


#######################################
## Part 2 - Clean Panel Data ##
#######################################


# NOTE: For the purpose of this strongly balanced panel exercise, we remove incomplete rows
# rather than filling in missing observations. For another analysis, we may choose to estimate missing data.

selected_panel_data <- panel_data %>%
  group_by(unitid) %>%
  filter(
    # Only include Tennessee colleges
    stabbr == "TN" &
    # Check if each group (unitid) has exactly 6 rows (one for each year from 2010 to 2015)
    n() == 6 &
    # Ensure these groups have no NA in the enrollment and grant aid information columns
    all(!is.na(c(scugrad,scugffn,scugffp,fgrnt_p,fgrnt_a,sgrnt_p,sgrnt_a))) &
    # Restrict the sample to undergraduate institutions
    ugoffer == 1
  ) %>%
  ungroup() %>%
  
  mutate(
    # dummy variable that identifies bachelor's degree-granting institutions
    # Determined by: (1) ensuring that the institution offers an undergraduate degree (ugoffer == 1)
    # (2) ensuring that the institution offers at least a Bachelor's degree instcat == 2 or 3
    degree_bach = as.integer(ugoffer == 1 & 
                              (instcat == 2 |
                                instcat == 3)
                             ),
    # dummy variable that identifies public institutions
    public = as.integer(control == 1),
    # total amt of aid = avg. aid * total number of students
    grant_state = sgrnt_a * scugffn,
    grant_federal = fgrnt_a * scugffn
    ) %>%
  rename("ID_IPEDS" = unitid,
         "enroll_ftug" = scugffn
  ) %>%
  select(ID_IPEDS, year, degree_bach, public, enroll_ftug, grant_state, grant_federal) 


# Export the selected data to a CSV file
write_csv(selected_panel_data, "clean.csv")

