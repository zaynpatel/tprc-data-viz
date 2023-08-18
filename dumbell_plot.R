library(tidyverse)
library(ggforce)
library(ggalt)
library(RColorBrewer)
library(forcats)

df <- loc_freq_NGSO_files

consolidate_company_names <- function(name) {
  if (grepl("^Iridium", name)) {
    return("Iridium LLC")
  } else if (grepl("^Raytheon", name)) {
    return("Raytheon")
  } else if (grepl("^Orbital ATK", name)) {
    return("Orbital ATK")
  } else if (grepl("^Northrop Grumman", name)) {
    return("Northrop Grumman")
  } else if (grepl("^Astro", name)) {
    return("Astro")
  } else if (grepl("^Blue Origin", name)) {
    return("Blue Origin")
  } else if (grepl("^MIT", name)) {
    return("MIT")
  } else if (grepl("^Boston University", name)) {
    return("Boston University")
  } else if (grepl("^Space Exploration", ignore.case = TRUE, name)) {
    return("SpaceX")
  } else if (grepl("Worldvu Satellites Limited Debtor-In-Possession", name, ignore.case = TRUE)) {
    return("Worldvu Satellites Limited")
  } else {
    return(name)
  }
}

# Function to consolidate the actions
consolidate_last_actions <- function(action) {
  case_when(
    action %in% c("Grant Expired Due to New License", "Grant of Authority", "Granted") ~ "Granted",
    action %in% c("Denied/Dismissed", "Dismissed by Delegated Authority") ~ "Dismissed",
    action %in% c("Granted in Part / Dismissed in Part", "Granted in Part / Denied in Part", "Granted in Part/ Deferred in Part") ~ "Granted in Part/Denied, Dismissed, Deferred in Part",
    TRUE ~ action
  )
}

# Function to consolidate type_TABLE
consolidate_type <- function(type) {
  case_when (
    type == "EX" ~ "Experimental",
    TRUE ~ "IBFS"
  )
}

df$consolidated_company_name <- sapply(df$Applicant.Name_TABLE, consolidate_company_names)
df$consolidated_action <- sapply(df$Last.Action_TABLE, consolidate_last_actions)
df$consolidated_type <- sapply(df$type_TABLE, consolidate_type)

df_ibfs <- df_ibfs

# We then determine the top 10 companies based on unique combinations
top_10_companies <- df_ibfs %>% 
  count(consolidated_company_name) %>% 
  top_n(10, n) %>% 
  pull(consolidated_company_name)
print(top_10_companies)

radial_plots <- function(df_ibfs, top_10_companies) {
  df_ibfs <- df_ibfs %>%
    select(consolidated_type, apogee, perigee, consolidated_company_name, consolidated_action, File.Number) %>%
    filter(!is.na(consolidated_type), !is.na(apogee), !is.na(perigee), !is.na(consolidated_company_name), !is.na(consolidated_action), !is.na(File.Number)) %>%
    filter(apogee >= 100, perigee >= 100) %>%
    mutate(
      a = (apogee + perigee) / 2,  
      e = ifelse(apogee > perigee, 1 - (perigee / apogee), 0),  
      b = ifelse(e < 1, a * sqrt(1 - e^2), NA), 
      consolidated_company_top10 = ifelse(consolidated_company_name %in% top_10_companies, consolidated_company_name, 'Other')
    ) %>%
    group_by(consolidated_company_top10)
  return(df_ibfs)
}

df_ibfs <- radial_plots(df_ibfs, top_10_companies)

df_ibfs <- df_ibfs %>%
  left_join(unique_combinations, by = "consolidated_company_name")

df_ibfs$consolidated_company_top10 <- fct_relevel(df_ibfs$consolidated_company_top10, "Other", after = Inf)

ggplot(df_ibfs, aes(y = forcats::fct_rev(consolidated_company_top10), x = perigee, xend = apogee, size = n)) +  
  geom_dumbbell(aes(colour = consolidated_action), size = 1.5, alpha = 0.5) +
  theme_minimal(base_family = "Times New Roman", base_size = 18) +
  coord_cartesian(xlim = c(0, 1500)) + 
  labs(
    y = "Company",
    x = "Altitude (km)"
  ) +
  scale_size_continuous(range = c(0.5, 2)) +
  scale_color_brewer(palette = "Set1", name = "Outcome")
