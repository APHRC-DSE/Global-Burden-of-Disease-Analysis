library(ggplot2)
library(RColorBrewer)
library(ggalluvial)

#------------------------------- descriptive table -------------------------------------------------------------

var_include <- c("country", "gender", "age_group_at_death", "death_year")

descr_table <- create_descr_table(
  data = df2,
  vars = var_include,
  by_var = death_year
)

#save table
#gtsave(descr_table, filename = file.path(output_Dir, "des_table.docx"))

#---------------------------- Mortality rate ---------------------------------------------------------------------

pop_data <- readxl::read_xlsx("./Other_Datasets/Deaths_population_Site.xlsx")


yearly_deaths <- pop_data %>%
  mutate(mortality = (deaths_number/population)*100000)%>%
  filter(Country != "Gambia")
 

p_mortality <- ggplot(yearly_deaths, aes(x = death_year, y = mortality, color = Country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Mortality Rate per 100,000 Population by Country and Year",
    x = "Year",
    y = "Mortality rate per 100,000",
    color = "Country"
  ) +
  scale_y_continuous(limits = c(0,900))+
  scale_x_continuous(breaks = 2015:2021) +
 theme_plot()


#save_plot(p_mortality, "mortality_rate.png")


#--------------------------deaths per hdss --------------------------------------------------------------------
       
deaths_hdss <- df2 %>%
  group_by(site_name, gender) %>%
  summarise(total_deaths = n(), .groups = "drop")

set2_colors <- brewer.pal(n = 8, name = "Set2")

gender_colors <- c("Male" = set2_colors[1],
                   "Female" = set2_colors[2])


plot_gender <- ggplot(deaths_hdss, aes(x = site_name, 
                                       y = total_deaths, 
                                       fill = gender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = gender_colors) +  
  labs(
    title = "Total Deaths by Country, Stratified by Gender",
    x = "HDSS",
    y = "Number of Deaths",
    fill = "Gender"
  ) +
  theme_plot()
  

#save_plot(plot_gender,"Total_deaths_country_gender.png" )  

#------------------------------ deaths per gender per hdss per year----------------------------------------------------

df_sex_site_time <- df2 %>%
  group_by(site_name, gender, death_year) %>%
  summarise(total_deaths = n(), .groups = "drop")


p_sex_site <- ggplot(df_sex_site_time,
                     aes(x = death_year, 
                         y = total_deaths, 
                         color = gender,
                         group = gender)) +
  geom_line(size = 1.1) +
  geom_point(size = 1.8) +
  facet_wrap(~site_name, nrow = 2, ncol = 3, scales = "free_y") +
  scale_color_manual(values = gender_colors) +  # specific Set1 colors
  labs(
    title = "Deaths by Sex Over Time Across HDSS Sites",
    x = "Year",
    y = "Number of deaths",
    color = "Sex"
  ) +
  theme_plot()

#save_plot(p_sex_site,"deaths_sex_gender_hdss.png",  width = 10, height = 6)

#-------------------------------------- Pyramid ---------------------------------------------------------
df_pyramid <- df2 %>%
  group_by(site_name, age_group_at_death, gender) %>%
  summarise(total_deaths = n(), .groups = "drop") %>%
  mutate(
    age_group_at_death = factor(age_group_at_death,
                                levels = c("Under 5", "5-14", "15-49", "50+")),
    total_deaths = ifelse(gender == "Male", -total_deaths, total_deaths)  # negative for males
  )

p_pyramid <- ggplot(df_pyramid,
                    aes(x = age_group_at_death,
                        y = total_deaths,
                        fill = gender)) +
  geom_col(width = 0.7) +
  coord_flip() +
  facet_wrap(~site_name, nrow = 2, ncol = 3, scales = "free_x") +
  scale_fill_manual(values = gender_colors) +
  scale_y_continuous(labels = abs) +  # show positive values on both sides
  labs(
    title = "Age Pyramid of Deaths by HDSS Site",
    x = "Age Group",
    y = "Number of Deaths",
    fill = "Gender"
  ) +
  theme_plot()

#save_plot(p_pyramid, "age_pyramid_of_deaths2.png", width=10)

#--------------------------------------- Top Causes of death 2015 and 2021 -----------------------------

cause_year_rank <- df2 %>%
  group_by(death_year, icd_10_cause_of_death) %>%
  summarise(total_deaths = n(), .groups = "drop") %>%
  group_by(death_year) %>%
  arrange(desc(total_deaths), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top_2015_2020 <- cause_year_rank %>%
  dplyr::filter(death_year %in% c(2015,2021))%>%
  group_by(death_year) %>%
  slice_max(total_deaths, n = 10) %>%
  ungroup()%>%
  dplyr::mutate(death_year= as.factor(death_year))
  
#---------------------------------------------- top 5 causes of death per gender -----------------------
#diseases colors
set1_colors <- brewer.pal(8, "Set1")
set3_colors <- brewer.pal(12, "Set3")
disease_colors <- c("Acute respiratory infections" = set2_colors[1],
                    "Cardiovascular diseases"  =  set2_colors[2],
                    "Diarrhoeal diseases" = set2_colors[3],
                    "HIV/AIDS" = set2_colors[7],
                    "Injury" = set2_colors[5],
                    "Malaria/dengue" = set2_colors[6],
                    "Neoplasms" = set2_colors[4],
                    "Neonatal disorders" = set2_colors[8],
                    "Tuberculosis" = set3_colors[10],
                    "Neuropsychiatric conditions" = set3_colors[8],
                    "Other infectious diseases" = set3_colors[11],
                    "Digestive diseases" = set3_colors[3],
                    "Diabetes mellitus" = set3_colors[6])


cause_gender_rank <- df2 %>%
  filter(!is.na(gender))%>%
  group_by(death_year, gender, icd_10_cause_of_death) %>%
  summarise(total_deaths = n(), .groups = "drop") %>%
  group_by(death_year,gender) %>%
  mutate(
    proportion = total_deaths / sum(total_deaths) * 100
  ) %>%
  arrange(death_year, gender, desc(proportion)) %>%
  slice_head(n = 5) %>%
  ungroup()


plot_gender_top <- ggplot(cause_gender_rank, aes(x = death_year, y = proportion, color = icd_10_cause_of_death, group = icd_10_cause_of_death)) +
              geom_line(size = 1.2) +
              geom_point(size = 3) +
              facet_wrap(~ gender) +
             scale_color_manual(values = disease_colors) +
              scale_y_continuous(limits = c(5,35))+
              #scale_x_continuous(breaks = min(cause_gender_rank$death_year):max(cause_gender_rank$death_year)) +
                   labs(
           title = "Leading 5 Causes of Death, gender stratified",
            x = "Year",
           y = "Proportion of Deaths (%)",
         color = "Cause of Death"
                     ) +
  theme_plot()

#save_plot(plot_gender_top, "top_gender_year.png", width = 10)

#---------------------------------- top 5 per year and country --------------------------------------------------------

cause_country_rank <- df2 %>%
  group_by(death_year, country, icd_10_cause_of_death) %>%
  summarise(total_deaths = n(), .groups = "drop") %>%
  group_by(death_year,country) %>%
  mutate(
    proportion = total_deaths / sum(total_deaths) * 100
  ) %>%
  arrange(death_year, country, desc(proportion)) %>%
  slice_head(n = 3) %>%
  ungroup()


plot_country_top <- ggplot(cause_country_rank,
                           aes(x = death_year,
                               y = proportion,
                               fill = icd_10_cause_of_death)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ country) +
  scale_fill_manual(values = disease_colors) +
  labs(
    title = "Leading Causes of Death, country stratified",
    x = "Year",
    y = "Proportion of Deaths (%)",
    fill = "Cause of Death"
  ) +
  theme_plot()

#save_plot(plot_country_top, "top_country_year.png", width = 10)


#------------------------------------------- top per age group -------------------------
top5_agegroup <- df2 %>%
  group_by(age_group_at_death, icd_10_cause_of_death)%>%
  summarise(total_deaths = n(), .groups = "drop") %>%
  group_by(age_group_at_death)%>%
  arrange(age_group_at_death, desc(total_deaths)) %>%
  slice_head(n = 5) 


p_sankey <- ggplot(top5_agegroup,
                   aes(axis1 = age_group_at_death, axis2 = icd_10_cause_of_death, y = total_deaths)) +
  geom_alluvium(aes(fill = icd_10_cause_of_death), width = 0.25, alpha = 0.9) +
  geom_stratum(width = 0.25, fill = "grey95", color = "grey30") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.8) +
  scale_fill_manual(values = disease_colors)+
  scale_x_discrete(limits = c("Age group", "Cause of death"),
                   expand = c(0.15, 0.15)) +
  labs(
    title = "Flow of Deaths by Age Group and Cause (Top 5 Causes)",
    x = NULL,
    y = "Number of deaths",
    fill = "Cause of death"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    panel.grid = element_blank()
  )


#save_plot(p_sankey, "top_agegroup.png", 10,5)

#----------------------------------------------------- top per age group per country -------------------------------

top5_agegroup_country <- df2 %>%
  group_by(country, age_group_at_death, icd_10_cause_of_death) %>%
  summarise(total_deaths = n(), .groups = "drop") %>%
  group_by(country, age_group_at_death) %>%
  arrange(desc(total_deaths), .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  ungroup()


p_sankey <- ggplot(top5_agegroup_country,
                   aes(axis1 = age_group_at_death,
                       axis2 = icd_10_cause_of_death,
                       y = total_deaths)) +
  
  geom_alluvium(aes(fill = icd_10_cause_of_death),
                width = 0.25, alpha = 0.9) +
  
  geom_stratum(width = 0.25,
               fill = "grey95",
               color = "grey30") +
  
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3.5) +
  
  facet_wrap(~ country, scales = "free_y") +  # allow independent scaling per country
  
  scale_fill_manual(values = disease_colors) +
  
  scale_x_discrete(limits = c("Age group", "Cause of death"),
                   expand = c(0.15, 0.15)) +
  
  labs(
    title = "Flow of Deaths by Age Group and Cause per Country",
    x = NULL,
    y = NULL,   #remove y-axis meaning
    fill = "Cause of death"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_blank(),   #hide y-axis text
    axis.ticks.y = element_blank(),  #hide y-axis ticks
    panel.grid = element_blank(),
    legend.position = "bottom"
  )





#----------------------------------------------------------------- END ---------------------------------------------------------------

