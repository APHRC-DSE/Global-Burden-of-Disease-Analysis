

#------------------------------------ Cause specific mortality rates ------------------------------
#count the number of deaths from each cause per year and country
num_per_cause <- df2 %>% 
     group_by(country, death_year, icd_10_cause_of_death) %>%
     summarise(deaths = n()) %>%
  arrange(desc(deaths))

#population data
pop_dataset <- pop_data %>%
  rename(country = Country)%>%
  filter(!country %in% c("Gambia"))

pop_dataset$death_year <- as.character(pop_dataset$death_year)
num_per_cause$death_year <- as.character(num_per_cause$death_year)

#merge the causes of death data and pop data
mortality_data <- left_join(
  pop_dataset,
  num_per_cause,
  by = c("country", "death_year")
)

#total pop per year
pop_year <- pop_dataset %>%
  group_by(death_year) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

#total death per year
deaths_cause <- mortality_data %>%
  group_by(death_year, icd_10_cause_of_death) %>%
  summarise(
    total_deaths = sum(deaths, na.rm = TRUE),
    .groups = "drop"
  )

#mortality rate per year per 100000
cs_rate_year_global <- deaths_cause %>%
  left_join(pop_year, by = "death_year") %>%
  mutate(
    cs_mortality_rate = (total_deaths / total_population) * 100000
  )

#pick top 6 per year 
top5_per_year <- cs_rate_year_global %>%
  group_by(death_year) %>%
  mutate(rank = dense_rank(desc(cs_mortality_rate))) %>%
  filter(rank <= 6) %>%
  ungroup()

#plot top six causes of mortality per year
CSMR <- ggplot(top5_per_year, 
       aes(x = death_year, y = cs_mortality_rate, color = icd_10_cause_of_death,group = icd_10_cause_of_death)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0,90))+
  scale_color_manual(values = disease_colors) +
  labs(
    title = "Cause-Specific Mortality Rates Over Time",
    x = "Year",
    y = "CSMR (per 100,000)",
    color = "Cause of Death"
  ) +
  #scale_y_continuous(limits = c(0,90))+
  #scale_x_continuous(breaks = 2015:2021) +
  theme_plot()

#save_plot(CSMR, "CSMR.png")



#---------------------------------------------- CSMR per country ------------------------------
 
csmr_country <-  mortality_data %>%
  mutate(
    csm_rate = (deaths / population) * 100000
  )

top5_per_year_country <- csmr_country %>%
  group_by(country,death_year) %>%
  mutate(rank = dense_rank(desc(csm_rate))) %>%
  filter(rank <= 5) %>%
  ungroup()

# plot CSMR per country, top 5
csmr_country_top <- ggplot(top5_per_year_country,
                           aes(x = death_year,
                               y = csm_rate,
                               color = icd_10_cause_of_death, group = icd_10_cause_of_death)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ country) +
  scale_color_manual(values = disease_colors) +
  labs(
    title = "Cause-specific mortality rates, country stratified",
    x = "Year",
    y = "CSMR (per 100,000)",
    color = "Cause of Death"
  ) +
  theme_plot()

#save_plot(csmr_country_top, "CSMR_per_country.png")


#------------------------------------------- YLL, Premature mortality ------------------------------------------

# add age groups present in life table
df3 <- df2 %>%
  mutate(
    age_group_table = cut(
      age_at_death,
      breaks = c(-Inf, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50,
                 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, Inf),
      right = FALSE,
      labels = c("<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
                 "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                 "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                 "75 to 79", "80 to 84", "85 to 89", "90 to 94", "95 to 99",
                 "100 to 104", "105 to 109", "110+")
    )
  )

#number deaths per age group, gender and year
result <- df3 %>%
  group_by(death_year, age_group_table,gender, icd_10_cause_of_death) %>%
  summarise(deaths = n(), .groups = "drop")

# read the life expectancy data files from the life table folder
life_tables <- file.path(mainDir, "life table")

table_data_files <- list.files(
  path = life_tables,
  pattern = "\\.csv$", #all csv files
  full.names = T,
  ignore.case = TRUE
)

#df_list <- lapply(table_data_files, read.csv) #each file to separate dataframe

df <- table_data_files %>%
  map_dfr(read_csv) #all files to one dataframe

#filter 2015 to 2019, since latest year in life table is 2019, use Africa life expectancies
df_filtered <- df %>%
  filter(
    year_id %in% 2015:2019,
    sex_name %in% c("male", "female"),
    measure_name == "Life expectancy",
    location_name == "Africa"
  )%>%
  select(sex_name,age_group_name,year_id, measure_name,val)%>%
  rename(gender = sex_name, age_group_table = age_group_name,death_year = year_id )%>%
  mutate(gender = case_when(gender == "male" ~ "Male",
                            gender == "female" ~"Female"))%>%
  mutate(death_year = as.character(death_year))


#merge life expectancy data with mortality dataset
df_merged <- result %>%
  left_join(df_filtered, by= c("gender","age_group_table", "death_year"))


#2019 life expectancy values to be used in 2020/2021
lifeexp_2019 <- df_merged %>%
  filter(death_year == 2019) %>%
  select(gender, age_group_table, val) %>%
  distinct()

#join these values to data
df_merged <- df_merged %>%
  left_join(lifeexp_2019, 
            by = c("gender", "age_group_table"), 
            suffix = c("", "_2019"))

#replace missing values in 2020 and 2021 with 2019 values
df_merged <- df_merged %>%
  mutate(
    val = ifelse(is.na(val) & death_year %in% c(2020, 2021),
                 val_2019,
                 val)
  )%>%
  select(-val_2019)


# 2) Compute YLL per group
df_merged <- df_merged %>%
  mutate(
    YLL = deaths * val,
    
  )

# Aggregate YLL per disease × year (sum across age groups and sexes)
YLL_per_disease_year <- df_merged %>%
  group_by(death_year, icd_10_cause_of_death) %>%
  summarise(
    total_deaths = sum(deaths, na.rm = TRUE),
    total_YLL = sum(YLL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(death_year, icd_10_cause_of_death)

#Select specific causes
selected_causes <- c("Malaria/dengue", "Injury","Cardiovascular diseases" ,"Tuberculosis", "Neonatal disorders", "HIV/AIDS")

YLL_selected <- YLL_per_disease_year %>%
  filter(icd_10_cause_of_death %in% selected_causes)%>%
  mutate(death_year = as.numeric(death_year)) %>%
  arrange(icd_10_cause_of_death, death_year)

#Plot YLL
plot_YLL <- ggplot(YLL_selected, aes(x = death_year, y = total_YLL, fill = icd_10_cause_of_death)) +
  geom_area(position = "stack", alpha = 0.8, color = "black", size = 0.2) +
  labs(
    title = "",
    x = "Year",
    y = "Years of life lost due \
    to premature death",
    fill = "Cause of Death"
  ) +
  scale_y_continuous(labels = function(x) ifelse(x == 0, "0", paste0(x / 1000, "k"))) +
  scale_x_continuous(breaks = 2015:2021) +
  scale_fill_manual(values = disease_colors) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )


#save_plot(plot_YLL,"YLL2.png" ) 

#--------------------------------------------- END ----------------------------------------------
