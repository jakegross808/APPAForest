library(tidyverse)
library(modelr)
library(ggplot2)
library(units)
library(gghighlight)

devtools::load_all('C:/Users/JJGross/Documents/R_projects/rFIA-master/rFIA-master/R')

# all plots ----
# Using tpa to hopefully get all plots available

all_plots_lat_long <- tpa(at_FIA, byPlot= TRUE, grpBy = c(STATECD, ECOSUBCD, LAT, LON), method = "annual")

all_plots_lat_long <- all_plots_lat_long %>%
  distinct(pltID, .keep_all = TRUE)

#________________________________________________divisions
# Get Better ECOSUBCD abbreviated names
# Distinct SUBSECTIONS:
n_distinct(eco$SUBSECTION)
levels(as.factor(eco$SUBSECTION))

# Distinct SUBSECTI_1:
n_distinct(eco$SUBSECTI_1)
levels(as.factor(eco$SUBSECTI_1))

# Distinct SECTION_NA:
n_distinct(eco$SECTION_NA)
levels(as.factor(eco$SECTION_NA))

# Distinct PROVINCE_N:
n_distinct(eco$PROVINCE_N)
levels(as.factor(eco$PROVINCE_N))
#_______________________________________________________

# ECOSUB and STATE names -------------------------------------------------------

state_codes <- read_csv("us-state-ansi-fips.csv") %>%
  mutate(st = as.integer(st))

state_eco <- all_plots_lat_long %>%
  select(STATECD, ECOSUBCD) %>%
  distinct() %>%
  left_join(state_codes, by = join_by(STATECD == st))

# Thinking something like State code plus 4 letter subsection abbr

eco_sub_names <- eco %>%
  select(SECTION, SUBSECTION, SUBSECTI_1) %>%
  st_drop_geometry()

state_eco2 <- state_eco %>%
  left_join(eco_sub_names, by = join_by(ECOSUBCD == SUBSECTION)) 

replace_list <- (c("Mountain" = "Mt", 
                  "Central" = "C.", 
                  "Northern" = "N.",
                  "Southern" = "S.",
                  "Eastern" = "E.",
                  "New Brunswick" = "NB",
                  "Lowlands" = "Lowl.",
                  "Lowland" = "Lowl.",
                  "Uplands" = "Upl.",
                  "Upland" = "Upl.",
                  "Plateau" = "Plat.",
                  "Piedmont" = "Pdmt",
                  "Blue" = "Bl.",
                  "Ridge" = "Rdg",
                  "Valley" = "Vly",
                  "Maine" = "ME",
                  "Connecticut" = "CT",
                  "Vermont" = "VT",
                  "Virginia" = "VA",
                  "Western" = "W.",
                  "Glacial Lake and Marine Plains" = "GL/MP",
                  "Hills and Plains" = "",
                  "Kittatinny-Shawangunk" = "Kitt-Shaw",
                  "Metasedimentary" = "Meta",
                  "Mahoosic Rangely Lakes" = "Mahoo/Rng Lks",
                  "Highlands" = "Highl.",
                  "Hudson" = "Hud.",
                  "Limestone" = "Limstn",
                  "Gettysburg" = "Getty",
                  "Sebago-Ossipee" = "Seb-Ossipee",
                  "Berkshire" = "Berk",
                  "Embayment" = "Embaymt"
                  ))

state_eco2_custom <- state_eco2 %>%
  mutate(cust_abbr = str_replace_all(SUBSECTI_1, replace_list))

eco_names <- state_eco2_custom %>%
  select(-STATECD, -stname) %>%
  arrange(stusps) %>%
  pivot_wider(names_from = stusps, values_from = stusps) %>%
  unite("states", CT:VT, remove = FALSE, na.rm = TRUE, sep = " ") %>%
  mutate(states = paste0("(",states,")")) %>%
  unite("cust_abb_st", cust_abbr:states, remove = FALSE, na.rm = TRUE, sep = " ") %>%
  select(ECOSUBCD, SECTION, ECOSUB_FULL = SUBSECTI_1, ECOSUB_ST = cust_abb_st, ECOSUB= cust_abbr, STATES = states)


# Dist from AT start  ----------------------------------------------------------

all_plots_spatial <- tpa(at_FIA, byPlot= TRUE, grpBy = c(STATECD, ECOSUBCD, ELEV), method = "annual", returnSpatial = TRUE)

all_plots_eco <- all_plots_spatial %>%
  dplyr::select(ECOSUBCD, geometry) %>%
  dplyr::distinct() 

#AT_nothern_terminus <- data.frame(longitude = c(-68.92130), latitude = c(45.90440))

plots_map <- ggplot() +
  geom_sf(data = all_plots_eco, aes(geometry = geometry, color = ECOSUBCD), shape = 21) +
  theme(legend.position = "none")

plots_map

# Select Southern-most plot
most_S_plot <- all_plots_lat_long %>%
  filter(pltID == "5_13_85_5")
most_S_plot
#most_NE_plot <- data.frame(longitude = c(-68.93607), latitude = c(46.49820))

plots_map +
  geom_point(data = most_S_plot, aes(x = LON, y = LAT), size = 4, 
             shape = 21, fill = "green") 

# Calculate distance between S point and each plot
most_S_plot_sf <- most_S_plot %>%
  sf::st_as_sf(coords = c("LON", "LAT"))

all_plots_spatial_dist <- mutate(.data = all_plots_spatial, 
                                 S_plot = most_S_plot_sf$geometry)

#Update S_plot cordinate ref system to same as other plots to calculate distance
st_crs(all_plots_spatial_dist$S_plot) <- st_crs(all_plots_spatial_dist$geometry)

all_plots_spatial_dist2 <- all_plots_spatial_dist %>%
  distinct(pltID, .keep_all = TRUE) %>%
  mutate(dist = st_distance(S_plot, geometry, by_element = TRUE))

plot_dist <- all_plots_spatial_dist2 %>%
  select(pltID, ECOSUBCD, ELEV, dist) %>%
  #left_join(eco_names, by = join_by(ECOSUBCD)) %>%
  st_drop_geometry()


# Add new ECOSUB and dist info to datasets before graphing ------------

# > All Plots ------------

# BAA Boxplot graphs by ECOSUBCD
BAA_plots_spatial <- tpa(at_FIA, 
                         byPlot= TRUE, 
                         #grpBy = c(STATECD, ECOSUBCD, ELEV), 
                         method = "annual", 
                         returnSpatial = TRUE) 

BAA_graph <- BAA_plots_spatial %>%
  left_join(plot_dist, by = join_by(pltID), relationship = "many-to-one", keep = FALSE) %>%
  left_join(eco_names, by = join_by(ECOSUBCD), relationship = "many-to-one")

BAA_graph %>%
  ggplot(aes(x=dist, y=BAA, color = ECOSUB_ST)) +
  geom_point()  


first_quantile <- quantile(BAA_graph$BAA, 0.25)

BAA_graph %>%
  st_drop_geometry() %>%
  mutate(ECOSUB_ST = forcats::fct_reorder(ECOSUB_ST, dist)) %>%
  ggplot() +
  geom_boxplot(aes(x=ECOSUB_ST, y=BAA), color = "red") +
  gghighlight(median(BAA) < first_quantile) +
  #theme(legend.position = "none") +
  geom_hline(yintercept = first_quantile, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Ecosubsection (from SW to NE)") + ylab("Basal area (sq. ft.) per acre") +
  geom_text(aes(1.5,first_quantile,label = "Q1", vjust = -0.5))
 
# could add n for each ECOSUB_ST to see how many plots are contributing to median


# Look at TPA + BAA of specific ECOSUB on dist/elev plot
unique(BAA_graph$ECOSUB_FULL)

eco_select <- "Gettysburg Piedmont Lowland"
BAA_graph %>%
  mutate(highlight = case_when(ECOSUB_FULL == eco_select ~ TRUE, 
                               .default = FALSE)) %>%
  ggplot(aes(x=dist, y=ELEV, color = TPA, size = BAA)) +
  geom_point(shape = 21) +
  gghighlight(highlight == TRUE) +
  ggtitle(eco_select) +
  xlab("Distance from SW to NE") + ylab("Elevation")

# Look at TPA + BAA of specific ECOSUB on TPA v. BAA plot
unique(BAA_graph$ECOSUB_FULL)

eco_select <- "Gettysburg Piedmont Lowland"
BAA_graph %>%
  mutate(highlight = case_when(ECOSUB_FULL == eco_select ~ TRUE, 
                               .default = FALSE)) %>%
  ggplot(aes(x=TPA, y=BAA, color = TPA, size = BAA)) +
  geom_point(shape = 21) +
  gghighlight(highlight == TRUE) +
  ggtitle(eco_select) +
  xlab("TPA") + ylab("BAA")

# Compare years + intensity of FIA monitoring
ECO_n <- BAA_graph %>%
  mutate(ECOSUB_ST = forcats::fct_reorder(ECOSUB_ST, dist)) %>%
  group_by(YEAR, ECOSUB, ECOSUBCD, ECOSUB_FULL, ECOSUB_ST) %>%
  summarise(n_plots = n()) 

levels(ECO_n$ECOSUB_ST)

# Run this again with most recent:
ECO_n %>%
  mutate(highlight = case_when(n_plots == 1 ~ "1 plot",
                               n_plots == 2 ~ "2 plots",
                               .default = NA)) %>%
  ggplot(aes(x=ECOSUB_ST, y=YEAR, size = n_plots, color = highlight)) +
  geom_count() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_size_area() +
  scale_color_manual(values = c("red2", "cyan3", "grey50"),
                     breaks = c("1 plot", "2 plots"))



# > Most Recent Plots ------------
   
# BAA Boxplot graphs by ECOSUBCD
BAA_plots_spatial_MR <- tpa(at_FIAMR, 
                         byPlot= TRUE, 
                         #grpBy = c(STATECD, ECOSUBCD, ELEV), 
                         method = "annual", 
                         returnSpatial = TRUE) 

BAA_graph_MR <- BAA_plots_spatial_MR %>%
  left_join(plot_dist, by = join_by(pltID), relationship = "many-to-one", keep = FALSE) %>%
  left_join(eco_names, by = join_by(ECOSUBCD), relationship = "many-to-one")

BAA_graph_MR %>%
  ggplot(aes(x=dist, y=BAA, color = ECOSUB_ST)) +
  geom_point()  


first_quantile <- quantile(BAA_graph_MR$BAA, 0.25)

BAA_graph_MR %>%
  st_drop_geometry() %>%
  mutate(ECOSUB_ST = forcats::fct_reorder(ECOSUB_ST, dist)) %>%
  ggplot() +
  geom_boxplot(aes(x=ECOSUB_ST, y=BAA), color = "red") +
  gghighlight(median(BAA) < first_quantile) +
  #theme(legend.position = "none") +
  geom_hline(yintercept = first_quantile, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Ecosubsection (from SW to NE)") + ylab("Basal area (sq. ft.) per acre") +
  geom_text(aes(1.5,first_quantile,label = "Q1", vjust = -0.5))


# Invasive Cover by Plot ----
inv_cover_plots <- invasive(at_FIA, byPlot = TRUE, grpBy = c(STATECD, ECOSUBCD), nCores = cores, method = 'annual')

inv_cover_plots_xy <- invasive(at_FIA, byPlot = TRUE, grpBy = c(STATECD, ECOSUBCD, LAT, LON), nCores = cores, method = 'annual')

# Invasive Presence by Plot
inv_presence_plots <- inv_cover_plots |>
  select(pltID, SCIENTIFIC_NAME, YEAR) |>
  distinct(pltID, SCIENTIFIC_NAME, YEAR) |> # species should be counted only 1 time per plot
  group_by(pltID, SCIENTIFIC_NAME, YEAR) |>
  summarise(inv_presence = n())

# Invasive Plots present
inv_n_plots <- inv_presence_plots |>
  group_by(SCIENTIFIC_NAME, YEAR) |>
  summarise(plots_present = n(), .groups = 'drop') 

inv_n_plots %>% 
  ggplot(aes(YEAR, plots_present, group = SCIENTIFIC_NAME)) +
  geom_line(alpha = 1/3)

lj <- inv_n_plots %>%
  filter(SCIENTIFIC_NAME == "Lonicera japonica")

lj %>% 
  ggplot(aes(YEAR, plots_present)) + 
  geom_point() + 
  ggtitle("Full data = ")

lj_mod <- lm(plots_present ~ YEAR, data = lj)
lj_mod

chk <- lj_mod[["coefficients"]][["YEAR"]]
str(chk)
chk

lj %>% 
  add_predictions(lj_mod) #|>
  ggplot(aes(YEAR, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

lj %>% 
  add_residuals(lj_mod) |> 
  ggplot(aes(YEAR, resid)) + 
  geom_hline(yintercept = 0, colour = "white", linewidth = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")



# Nest by Species
by_species <- inv_n_plots %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  nest()

by_species$SCIENTIFIC_NAME[[1]]
by_species$data[[1]]

# Models
plots_present_model <- function(df) {
  lm(plots_present ~ YEAR, data = df)
}

plots_present_model_return_slope <- function(df) {
  m <- lm(plots_present ~ YEAR, data = df)
  slope <- m[["coefficients"]][["YEAR"]]
  return(slope)
}




models <- map(by_species$data, plots_present_model)
models[[22]] # same as Lonicera japonica model above

models <- map(by_species$data, plots_present_model_return_slope)
models[[22]] # same as Lonicera japonica model above

models <- map(by_species$data, plots_present_model_fitted)
models[[22]] # same as Lonicera japonica model above

# add model to nested dataframe
by_species_m <- by_species %>% 
  mutate(model = map(data, plots_present_model),
         slope = map(data, plots_present_model_return_slope))


by_species_m2 <- by_species_m %>% 
  mutate(
    slope = as.numeric(slope),
    resids = map2(data, model, add_residuals)
  ) %>%
  arrange(-slope)

resids <- unnest(by_species_m2, resids)
resids

# Use glance to look at model quality metrics
broom::glance(lj_mod)

glance <- by_species %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE) %>%
  arrange(-slope)

glance %>% 
  ggplot(aes(SCIENTIFIC_NAME, r.squared)) + 
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)

inv_n_plots %>% 
  semi_join(bad_fit, by = "SCIENTIFIC_NAME") %>% 
  ggplot(aes(YEAR, plots_present, colour = SCIENTIFIC_NAME)) +
  geom_line()

test <- glance[1,]

# This adds column to nested dataframe:
test_new <- test %>%
  mutate(data = map(data, 
                    ~ mutate(.x, 
                             test = "did this work")))

test_new2 <- test %>%
  mutate(data = map(add_predictions(model = plots_present_model)))




test1 <- test %>% 
  mutate(plot1 = map(data, ~ ggplot(., aes(x = plots_present, y = YEAR)) +
                       geom_point() +
                       labs(title= SCIENTIFIC_NAME)
  ) 
  )

test2 <- test %>% 
  mutate(plot1 = map(data, ~ ggplot(., aes(x = plots_present, y = YEAR)) +
                       geom_point() +
                       labs(title= SCIENTIFIC_NAME)
  ) 
  )



test1$plot1

iris_species_figs <- iris_species_figs %>% 
  mutate(plot2 = pmap(list(data,`(Intercept)`,Sepal.Width), 
                      function(a,b,c) ggplot(a, aes(x = Sepal.Width, y = Sepal.Length)) +
                        geom_point() +
                        geom_abline(intercept = b, slope = c, color = 'blue')
  )
  ) 

# map models to the tibble
map_models <- inv_n_plots_sp1 %>% 
  mutate(model = map(data, model)) %>%
  
  

  nest(.by = SCIENTIFIC_NAME) |>
  dplyr::mutate(models = lapply(data, function(df) lm(plots_present ~ YEAR, data = df)))


                        
mutate(slope = map(data, ~coef(lm(value ~ points, data = .x))[["points"]])) 

look <- mtcars %>%
    nest(.by = cyl) %>%
    dplyr::mutate(models = lapply(data, function(df) lm(mpg ~ wt, data = df)))    
  
    


inv_n_plots |>
  #filter(SCIENTIFIC_NAME == "Elaeagnus umbellata") |>
  ggplot2::ggplot(ggplot2::aes(x = YEAR, y = plots_present, color = SCIENTIFIC_NAME)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_grid(cols = vars(SCIENTIFIC_NAME))

# Invasive Richness by Plot
inv_richness_plots <- inv_presence_plots |>
  group_by(pltID, YEAR) |>
  summarize(inv_richness = sum(inv_presence))

hist(inv_richness_plots$inv_richness)






#invasive <- invasive(atMatch, polys = eco, nCores = 6, totals = TRUE) # This is what was in 03make.R
#invasives <- readRDS("summary_data/invasive.rds") # This gets called in index.Rmd

inv <- invasive(at_FIA, polys = eco, nCores = cores, method = 'annual')
invasive_TI_var <- invasive(at_FIA, polys = eco, nCores = cores, totals = TRUE, variance = TRUE) # This uses method = TI as default
invasive_annual_var <- invasive(at_FIA, polys = eco, nCores = cores, totals = TRUE, variance = TRUE, method =  "annual") # See if method = "annual" is different
str(invasive_annual_var)

invasive_TI_M221Dd <- invasive_TI_var |>
  filter(SUBSECTION == "M221Dd") |>
  filter(COMMON_NAME == "Japanese honeysuckle") |>
  select(YEAR, SUBSECTION, SUBSECTI_1, SCIENTIFIC_NAME, COMMON_NAME, COVER_PCT, COVER_PCT_VAR, nPlots_INV, N)

table(invasive_TI_M221Dd$COMMON_NAME)
plot(invasive_TI_M221Dd$YEAR, invasive_TI_M221Dd$COVER_PCT)

invasive_annual_M221Dd <- invasive_annual_var |> 
  filter(SUBSECTION == "M221Dd") |>
  filter(COMMON_NAME == "Japanese honeysuckle") |>
  select(YEAR, SUBSECTION, SUBSECTI_1, SCIENTIFIC_NAME, COMMON_NAME, COVER_PCT, COVER_PCT_VAR, nPlots_INV, N)

table(invasive_annual_M221Dd$COMMON_NAME)
plot(invasive_annual_M221Dd$YEAR, invasive_annual_M221Dd$COVER_PCT)

# I like summarizing by method = "annual"

#invasives %>% as_tibble() %>%  #original code
invasive_annual_var %>% as_tibble() %>% 
  filter(SUBSECTION == "M221Dd") |>
  #filter(SECTION_NA %in% params$section) %>% # filter out data by Section scale # original code
  mutate(SE= round(sqrt(COVER_PCT_VAR)/sqrt(N),3), # var used here but not called in original 03make.R
         COMMON_NAME =  stringr::str_to_sentence(COMMON_NAME))%>% 
  select(SUBSECTI_1, YEAR ,COMMON_NAME,  COVER_PCT, SE)%>% # in original: "Year= MEASYEAR"
  ggplot(aes(x = YEAR, y = COVER_PCT, color= SUBSECTI_1 )) +
  geom_point() + geom_errorbar(aes(ymin= COVER_PCT-SE, ymax= COVER_PCT+SE))+
  labs(x = "", y = "Average Percent Cover + SE", color = "") +
  #scale_color_viridis_d()+
  theme_classic() +
  facet_wrap(~stringr::str_to_title(COMMON_NAME))+
  theme(legend.position = "top", axis.text = element_text(size = 14), axis.title = element_text(size = 14),
        axis.text.x= element_text(angle = 0), strip.text = element_text(size =  14, face ="bold"),legend.text= element_text(size = 14),legend.title= element_text(size = 14))

library(kableExtra)

invasive_annual_var %>% as_tibble() %>% 
  #filter(SECTION_NA %in% params$section) %>% # filter out data by Section scale
  mutate(`Percent Cover (SE)`= paste(round(COVER_PCT,2),"(", round(sqrt(COVER_PCT_VAR)/sqrt(N),3),")"),
         COMMON_NAME =  stringr::str_to_sentence(COMMON_NAME),
         `Percent Plots Present` = (nPlots_INV/N)*100) %>% arrange(SUBSECTI_1,COMMON_NAME) %>% 
  select(`Ecol. Subsection` = SUBSECTI_1, YEAR,`Common Name` = COMMON_NAME, `Latin Name`= SCIENTIFIC_NAME, `Percent Cover (SE)`, `Plots Present`= nPlots_INV, `Percent Plots Present`) %>% 
  kableExtra::kbl(.,digits = 2, escape = FALSE, align= "l", longtable= T, caption= paste0("Average annual percent cover and plot frequency of invasive plants detected within each subsection of the , params$section, during each measurement year.")) %>% column_spec(.,column= 4,italic=T) %>% collapse_rows(.,columns = 1, valign = "top") %>% 
  kable_styling(fixed_thead = TRUE, bootstrap_options = c("striped", "hover", "condensed", "responsive")) 


ts.plot$data$xVar<-as.numeric(ts.plot$data$YEAR)
ts.plot

plotFIA(tpaEcots, y = TPA, grp = SUBSECTI_1, plot.title = 'TPA per ecoregion')

plotFIA(tpaEco, y = BAA, legend.title = 'Basal area (sq.ft/acre)')

