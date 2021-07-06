# Get the data from the database
csales <- dbload("sales")
cfprod <- dbload("products")
stores <- dbload("stores") %>% 
  filter(sls(bundle, acc_type)) %>% 
  rename(store_id_id = id) %>% 
  select(store_id_id, state, 
         tobacco_purchases, current_account_start_date, acc_type)


#################################################################################
### Flavor vs CPM
# finding if different flavor has any vary sales volume
## Poisson
# Prepare data
csales1 <- csales %>%
  left_join(cfprod[,1:3], by = c("product_id_id"= "id")) %>%
  rename(item = name,
         qty = quantity) %>%
  mutate(date = floor_date(date, "month")) %>%
  filter(qty != 0,
         !is.na(qty),
         # only get the data for 2019
         date >= "2019-01-01",
         date <= "2019-12-01",
         sls(case, category)) %>%
  group_by(store_id_id) %>%
  summarise(n_flavor = n_distinct(item)) 


ss1 <- ss %>%
  filter(f.date >= "2019-01-01",
         f.date <= "2019-12-01",
         f.date >= st.date) %>%
  filter(all(machines == first(machines)),
         all(sls(yes, data)),
         all(sls(bundle, program)),
         machines > 0) %>%
  filter(f.date == "2019-12-01",
         cpm > 0,
         cpm <= 70) %>%
  left_join(csales1, by = "store_id_id") %>%
  select(store_id_id, machines, cpm, n_flavor)

flavor_CPM <- ss1 %>% 
  # 4 or more flavors for 2 machines
  filter(n_flavor >= 4 & cpm >= 15) %>% 
  # only for 2 machines (or change to 1 machine)
  filter(machines == 2)

## EDA
flavor_CPM$n_flavor <- as.factor(flavor_CPM$n_flavor)
flavor_CPM$cpm <- as.integer(round(flavor_CPM$cpm))
summary(flavor_CPM)

# Histogram
ggplot(data = flavor_CPM, mapping = aes(x = cpm)) + 
  geom_histogram(mapping = aes(y = ..density..), binwidth = 1) +
  theme_bw() + 
  theme(aspect.ratio = 1)
# Boxplot
ggplot(data = flavor_CPM, mapping = aes(x = n_flavor, y = cpm)) +
  geom_boxplot() +
  theme_bw() +
  theme(aspect.ratio = 1)

## Regression Model
flavor_CPM_poisson <- glm(cpm ~ n_flavor,
                          data = flavor_CPM,
                          family = poisson(link = "log")) 
summary(flavor_CPM_poisson)

## Assumptions
# DFFITS
flavor_CPM_dffits <- data.frame("dffits" = dffits(flavor_CPM_poisson))
flavor_CPM_dffits$obs <- 1:length(flavor_CPM$cpm)

ggplot(data = flavor_CPM_dffits) + 
  geom_point(mapping = aes(x = obs, y = abs(dffits))) +
  geom_hline(mapping = aes(yintercept = 2 * sqrt(7 / length(obs))), 
             color = "red", linetype = "dashed") +  # for n > 30
  theme_bw() +
  theme(aspect.ratio = 1)

# Compare the mean and variance
mean(flavor_CPM$cpm)
var(flavor_CPM$cpm)
# Conduct the formal test 
pchisq(q = flavor_CPM_poisson$deviance,df = flavor_CPM_poisson$df.residual,
       lower.tail = FALSE)  # small p-value -> overdispersion
# Fit a Quasi-Poisson model and look at the dispersion parameter estimate 
flavor_CPM_poisson_quasi <- glm(cpm ~ n_flavor,
                                data = flavor_CPM,
                                family = quasipoisson(link = "log")) 
summary(flavor_CPM_poisson_quasi)  # Dispersion parameter greater than 1

## Model Performence
# Likelihood ratio test statistic
(like_rat <- flavor_CPM_poisson$null.deviance - flavor_CPM_poisson$deviance)

# Likelihood ratio p-value
pchisq(q = like_rat, df = length(flavor_CPM_poisson$coefficients) - 1, 
       lower.tail = FALSE)  # At lease one variable is significant



### lm or anova (if needed)

# csales1 <- csales %>%
#   left_join(cfprod[,1:3], by = c("product_id_id"= "id")) %>%
#   rename(item = name,
#          qty = quantity) %>%
#   mutate(date = floor_date(date, "month")) %>%
#   filter(qty != 0,
#          !is.na(qty),
#          date >= "2019-01-01",
#          date <= "2019-12-01",
#          sls(case, category)) %>%
#   group_by(store_id_id) %>%
#   summarise(n_flavor = n_distinct(item)) 
# 
# 
# ss1 <- ss %>%
#   filter(f.date >= "2019-01-01",
#          f.date <= "2019-12-01",
#          f.date >= st.date) %>%
#   filter(all(machines == first(machines)),
#          all(sls(yes, data)),
#          all(sls(bundle, program)),
#          machines > 0) %>%
#   filter(f.date == "2019-12-01",
#          cpm > 0,
#          cpm <= 70) %>%
#   left_join(csales1, by = "store_id_id") %>%
#   select(store_id_id, machines, cpm, n_flavor)
# 
# flavor_CPM <- ss1 %>% filter(n_flavor >= 4 & cpm >= 15) %>% 
#   filter(machines == 2)
# 
# 
# 
# ### Linear Regression
# 
# # EDA
# summary(flavor_CPM)
# flavor_CPM$n_flavor <- as.factor(flavor_CPM$n_flavor)
# 
# # Box plot
# ggplot(data = flavor_CPM, mapping = aes(x = n_flavor, y = cpm)) +
#   geom_boxplot() +
#   theme_bw() +
#   theme(aspect.ratio = 1)
# 
# # Jitter Scatter plot
# ggplot(data = flavor_CPM, mapping = aes(y = theCPM, x = n_flavor)) +
#   geom_point() +
#   geom_jitter(height = 0.2) +
#   theme_bw() +
#   theme(aspect.ratio = 1)
# 
# # Covert to dummy variable
# flavor_CPM$f2 <- ifelse(flavor_CPM$n_flavor == 2, 1, 0)
# flavor_CPM$f3 <- ifelse(flavor_CPM$n_flavor == 3, 1, 0)
# flavor_CPM$f7 <- ifelse(flavor_CPM$n_flavor == 7, 1, 0)
# flavor_CPM$f4 <- ifelse(flavor_CPM$n_flavor == 4, 1, 0)
# flavor_CPM$f6 <- ifelse(flavor_CPM$n_flavor == 6, 1, 0)
# flavor_CPM$f5 <- ifelse(flavor_CPM$n_flavor == 5, 1, 0)
# flavor_CPM$f8 <- ifelse(flavor_CPM$n_flavor == 8, 1, 0)
# flavor_CPM$f9 <- ifelse(flavor_CPM$n_flavor == 9, 1, 0)
# flavor_CPM$f10 <- ifelse(flavor_CPM$n_flavor == 10, 1, 0)
# 
# flavor_CPM_lm <- lm(cpm ~ f3 +  f4 + f5 + f6 + f7 +f8 + f9 + f10, 
#                     data = flavor_CPM)
# summary(flavor_CPM_lm)
# 
# 
# 
# 
# 
# #### Anova 2 flavor vs all other n_flavor
# data <- flavor_CPM %>% 
#   mutate(flav = ifelse(n_flavor == 2,
#                        1,
#                        0))
# 
# 
# data$flav <- as.factor(data$flav)
# my.aov <- aov(cpm ~ flav, data = data, contrasts = list(flav = contr.sum))
# summary(my.aov)
# dummy.coef(my.aov)
# hist(my.aov$residuals, breaks = 50)
# qqPlot(my.aov$residuals)
# favstats(data$cpm ~ data$flav, data = data)
# 
# 



###################################################################################
### Flavor Hierarchy
# find different level of flavors and make recommendation
csales1 <- csales %>%
  left_join(cfprod[,1:3], by = c("product_id_id"= "id")) %>%
  rename(item = name,
         qty = quantity) %>%
  mutate(date = floor_date(date, "month")) %>%
  filter(qty != 0,
         !is.na(qty),
         # only want the data that not older than 2018
         date >= "2018-01-01",
         # date <= "2019-12-01",
         sls(case, category)) %>% 
  select(qty, date, product_id_id, store_id_id)


# find the cpm for the last month of the year
test <- ss %>% 
  filter(f.date == "2020-12-01") %>% 
  distinct(store_id_id, .keep_all = TRUE) %>% 
  select(store_id_id, cpm) %>% 
  rename(theCPM = cpm)


flavor_data <- ss %>%
  filter(f.date >= "2018-01-01") %>%
  filter(sls(bundle, program),
         all(sls(yes,data)),
         all(machines == first(machines)),
         machines == 2) %>%
  left_join(test, by = "store_id_id") %>%
  filter(theCPM >= 15) %>%
  left_join(csales1, by = c("store_id_id", "f.date" = "date")) %>%
  #select(store_id_id, machines, cpm, n_flavor, product_id_id) %>%
  group_by(product_id_id, f.date) %>%
  summarise(cases = sum(qty)) %>%
  filter(!product_id_id %in% c('10', '45', '46', '287', '7', '340'))



# find coefficient for all flavor
flavor_data$product_id_id <- as.factor(flavor_data$product_id_id)
my.aov_all <- aov(cases ~ product_id_id, data = flavor_data, contrasts = list(product_id_id = contr.sum))
summary(my.aov_all)
dummy.coef(my.aov_all)
par(mfrow = c(1,2))
plot(my.aov_all, which = 1:2)
hist(my.aov_all$residuals, breaks = 30)
qqPlot(my.aov_all$residuals)
favstats(flavor_data$cases ~ flavor_data$product_id_id)


layer <- function(product) {
  
  data <- flavor_data %>% 
    filter(product_id_id %in% product)
  
  data$product_id_id <- as.factor(data$product_id_id)
  my.aov <- aov(cases ~ product_id_id, data = data, contrasts = list(product_id_id = contr.sum))
  summary <- summary(my.aov)
  dummy.coef(my.aov)
  #plot(my.aov, which = 1:2)
  hist(my.aov$residuals, breaks = 30)
  qqPlot(my.aov$residuals)
  stats <- favstats(data$cases ~ data$product_id_id, data = data)
  summary
}


group <- function(product, f1, f2) {
  data <- flavor_data %>% 
    filter(product_id_id %in% product) %>%
    mutate(size = ifelse(product_id_id == f1 | product_id_id == f2,
                         2,
                         1))
  
  data$size <- as.factor(data$size)
  my.aov <- aov(cases ~ size, data = data, contrasts = list(size = contr.sum))
  summary <- summary(my.aov)
  #anova(my.aov_small)
  dummy.coef(my.aov)
  # plot(my.aov, which = 1:2)
  hist(my.aov$residuals, breaks = 30)
  qqPlot(my.aov$residuals)
  favstats(data$cases ~ data$size)
  summary
}



## group (layer vs another layer)
group(c('14', '11', '1', '13', '138'), 1, 14)
group(c('339', '6', '11', '13', '138'), 339, 6)
group(c('6', '339', '9', '139', '12'), 6, 339)
#group(c('6', '339', '7', '9', '12', '139', '340'), 6, 339)



## layer (product vs product in the same layer)
layer(c('14', '1'))
layer(c('13', '138', '11'))
layer(c('6', '339'))
layer(c('139', '9', '12'))




#################################################################################
### Flavor Sequence
# discover customer's buying sequence or pattern to make recommendation
csales1 <- csales %>%
  left_join(cfprod[,1:3], by = c("product_id_id"= "id")) %>%
  rename(item = name,
         qty = quantity) %>%
  mutate(date = floor_date(date, "month")) %>%
  filter(qty != 0,
         !is.na(qty),
         # all data after 2017/01
         date >= "2017-01-01",
         # date <= "2019-12-01",
         sls(case, category)) %>%
  select(store_id_id, product_id_id, date, qty)
# group_by(store_id_id) %>%
# summarise(n_flavor = n_distinct(item)) 



ss1 <- ss %>%
  filter(
    f.date >= "2017-01-01",
    # f.date <= "2019-12-01",
    f.date >= st.date
  ) %>%
  filter(all(machines == first(machines)),
         all(sls(yes, data)),
         all(sls(bundle, program)),
         machines > 0) %>%
  # choose what machine number to use
  filter(all(machines == 1)) %>%
  # filter(f.date == "2019-12-01",
  #        cpm > 0,
  #        cpm <= 70) %>%
  left_join(csales1, by = c("store_id_id" = "store_id_id", "f.date" = "date")) %>%
  select(store_id_id, f.date, product_id_id, qty) %>%
  filter(!is.na(product_id_id)) %>%
  # filter out unuse flavor
  filter(!product_id_id %in% c('14', '10', '45', '46', '287'))



## Sequential Itemset Mining - SPADE
# Start time of data to be considered
start_month <- "2016-12-01"
# organize flavor by month
flavor_sequence <- ss1 %>%
  #  filter(product_id_id != 14) %>%
  group_by(store_id_id, f.date) %>%
  summarise(size = n(),
            flavor = paste(as.character(product_id_id), collapse = ";"))

# Make the variable into factor
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

flavor_sequence$event_id <- elapsed_months(flavor_sequence$f.date, start_month)
flavor_sequence <- flavor_sequence %>% select(store_id_id, event_id, size, flavor)
# they have to have these names
names(flavor_sequence) = c("sequenceID", "eventID", "SIZE", "items")
flavor_sequence <- data.frame(lapply(flavor_sequence, as.factor))
flavor_sequence <- flavor_sequence[order(flavor_sequence$sequenceID, flavor_sequence$eventID),]

# Convert to transaction matrix data type
write.table(flavor_sequence, "mytxtout.txt", sep=";", row.names = FALSE, col.names = FALSE, quote = FALSE)
flavor_matrix <- read_baskets("mytxtout.txt", sep = ";", info = c("sequenceID","eventID","SIZE"))
summary(flavor_matrix)
s1 <- cspade(flavor_matrix, parameter = list(maxlen = 2, support = 0.001),
             control = list(verbose = TRUE, bfstype = TRUE))
s1.df <- as(s1, "data.frame")
summary(s1)
# Get induced temporal rules from frequent itemsets
r1 <- ruleInduction(s1, confidence = 0.0001, control = list(verbose = FALSE))
# remove repeated sequence
r1 <- r1[!is.redundant(r1)]
r1 <- as(r1, "data.frame") %>% as.tibble() %>%
  arrange(-confidence, -lift)



# Separate LHS and RHS rules
r1$rulecount <- as.character(r1$rule)
max_col <- max(sapply(strsplit(r1$rulecount,' => '),length))
r_sep <- separate(data = r1, col = rule, into = paste0("Time",1:max_col), sep = " => ")
r_sep$Time2 <- substring(r_sep$Time2,3,nchar(r_sep$Time2)-2)
# Strip LHS baskets
max_time1 <- max(sapply(strsplit(r_sep$Time1,'},'),length))
r_sep$TimeClean <- substring(r_sep$Time1,3,nchar(r_sep$Time1)-2)
r_sep$TimeClean <- gsub("\\},\\{", "zzz", r_sep$TimeClean)
r_sep_items <- separate(data = r_sep, col = TimeClean, into = paste0("Previous_Items",1:max_time1), sep = "zzz")
# Get cleaned temporal rules: time reads sequentially from left to right
r_shift_na <- r_sep_items



r10 <- r_shift_na %>% rename(alpha = Previous_Items1,
                             beta = Time2) %>%
  # remove sequence that has same product in both alpha and beta
  filter(beta != alpha) %>%
  select(7, 2:5)

gg <- r10 %>% 
  filter(support >= 0.1) %>% 
  mutate(a = str_split(alpha, pattern = ",")) %>% 
  rowwise() %>% 
  filter(!(beta %in% a)) %>% 
  # remove sequence that has same product in both alpha and beta
  separate(beta, c("b1", "b2", "b3"), sep = ",") %>% 
  rowwise() %>% 
  filter(!(b1 %in% a)) %>% 
  rowwise() %>% 
  filter(ifelse(!is.na(b2), !(b2 %in% a), TRUE)) %>%
  rowwise() %>% 
  filter(ifelse(!is.na(b3), !(b3 %in% a), TRUE))

final_spade <- gg %>% 
  # set specific parameter to filter the target data
  filter(lift >= 1.1 | lift <= 0.85) %>% 
  unite("beta", b1:b3, na.rm = TRUE, sep = ",") %>% 
  select(1:5) %>% pall

# change ID to real product name
final_spade$alpha <- sub("139", "Lemonade", c(final_spade$alpha))
final_spade$beta <- sub("139", "Lemonade", c(final_spade$beta))
final_spade$alpha <- sub("339", "Triangle", c(final_spade$alpha))
final_spade$beta <- sub("339", "Triangle", c(final_spade$beta))
final_spade$alpha <- sub("138", "Geog Peach", c(final_spade$alpha))
final_spade$beta <- sub("138", "Geog Peach", c(final_spade$beta))
final_spade$alpha <- sub("6", "Lemon Ice", c(final_spade$alpha))
final_spade$beta <- sub("6", "Lemon Ice", c(final_spade$beta))
final_spade$alpha <- sub("9", "Rock n Roll", c(final_spade$alpha))
final_spade$beta <- sub("9", "Rock n Roll", c(final_spade$beta))
final_spade$alpha <- sub("11", "Mango", c(final_spade$alpha))
final_spade$beta <- sub("11", "Mango", c(final_spade$beta))
final_spade$alpha <- sub("12", "Smash Berry", c(final_spade$alpha))
final_spade$beta <- sub("12", "Smash Berry", c(final_spade$beta))
final_spade$alpha <- sub("13", "Sour Apple", c(final_spade$alpha))
final_spade$beta <- sub("13", "Sour Apple", c(final_spade$beta))
final_spade$alpha <- sub("1", "Blue Raz", c(final_spade$alpha))
final_spade$beta <- sub("1", "Blue Raz", c(final_spade$beta))

t1m <- final_spade %>% 
  arrange(-support) %>% pall





##################################################################################
### Flavor vs Race
# see if different race have different preference for the flavor
# DECENNIAL DATASET 
library(tidycensus)
cen_var <- load_variables(2010, "sf1", cache = TRUE)

dec <- get_decennial(geography = "zcta",
                     variables = c(paste("P00500", 3:9, sep = ""), "P005010",
                                   "P017001", "P017002", "P037001", "P037002", "P001001"),
                     year = 2010) %>%
  rename(zip = GEOID,
         name = NAME) %>%
  left_join(cen_var, c("variable" = "name")) %>% 
  filter(!grepl("TOTAL POPULATION", concept))

race_house <- dec %>% 
  mutate(label = case_when(
    grepl("White", label) == TRUE ~ "white",
    grepl("Indian", label) == TRUE ~ "indian",
    grepl("Black", label) == TRUE ~ "black",
    grepl("Asian", label) == TRUE ~ "asian",
    grepl("Other Race", label) == TRUE ~ "other_Race",
    grepl("Pacific Islander", label) == TRUE ~ "island",
    grepl("Two or More", label) == TRUE ~ "Race2",
    grepl("Total!!Hispanic or Latino", label) == TRUE ~ "hispanic",
    grepl("household size!!Total$", label) == TRUE ~ "household",
    grepl("household size!!Total!!", label) == TRUE ~ "householdU18",
    grepl("family size!!Total$", label) == TRUE ~ "family",
    grepl("family size!!Total!!", label) == TRUE ~ "familyU18"
  )) %>% 
  pivot_wider(names_from = label, values_from = value) %>% 
  group_by(zip) %>% 
  fill(c('white', 'black', 'indian', 'asian', 'island', 'other_Race', "Race2",
         'hispanic', 'household', 'householdU18', 'family', 'familyU18'), 
       .direction = c("downup")) %>% 
  select(-c("name", "variable", "concept")) %>% 
  distinct() %>% 
  mutate(
    # race = as.factor(case_when(
    # (hispanic / sum(white, black, indian, asian, island, other_Race, Race2, hispanic, na.rm = TRUE)) > 0.5
    # ~ "hisp",
    # (white / sum(white, black, indian, asian, island, other_Race, Race2, hispanic, na.rm = TRUE)) > 0.5
    # ~ "wh",
    # TRUE ~ "other")),
    hisp = (hispanic / sum(white, black, indian, asian, island, other_Race, Race2, hispanic, na.rm = TRUE)),
    wh = (white / sum(white, black, indian, asian, island, other_Race, Race2, hispanic, na.rm = TRUE)),
    blac = (black / sum(white, black, indian, asian, island, other_Race, Race2, hispanic, na.rm = TRUE)),
    other = (sum(indian, asian, island, other_Race, Race2, na.rm = TRUE) / 
               sum(white, black, indian, asian, island, other_Race, Race2, hispanic, na.rm = TRUE)),) %>% 
  select(zip, household, householdU18, family, familyU18, hisp, wh, blac, other)

race <- dec %>% 
  mutate(label = case_when(
    grepl("White", label) == TRUE ~ "white",
    grepl("Indian", label) == TRUE ~ "indian",
    grepl("Black", label) == TRUE ~ "black",
    grepl("Asian", label) == TRUE ~ "asian",
    grepl("Other Race", label) == TRUE ~ "other_Race",
    grepl("Pacific Islander", label) == TRUE ~ "island",
    grepl("Two or More", label) == TRUE ~ "Race2",
    grepl("Total!!Hispanic or Latino", label) == TRUE ~ "hispanic",
    grepl("household size!!Total$", label) == TRUE ~ "household",
    grepl("household size!!Total!!", label) == TRUE ~ "householdU18",
    grepl("family size!!Total$", label) == TRUE ~ "family",
    grepl("family size!!Total!!", label) == TRUE ~ "familyU18"
  )) %>% 
  pivot_wider(names_from = label, values_from = value) %>% 
  group_by(zip) %>% 
  fill(c('white', 'black', 'indian', 'asian', 'island', 'other_Race', "Race2",
         'hispanic', 'household', 'householdU18', 'family', 'familyU18'), 
       .direction = c("downup")) %>% 
  select(-c("name", "variable", "concept")) %>% 
  distinct()

race <- race %>% 
  ungroup(zip) %>% 
  summarise(white = sum(white),
            black = sum(black),
            hispanic = sum(hispanic),
            # indian = sum(indian),
            # asian = sum(asian),
            # island = sum(island),
            # other_race = sum(other_Race),
            # race2 = sum(Race2),
            other = sum(indian, asian, island, other_Race, Race2)) %>% 
  pivot_longer(1:4, names_to = "race", values_to = "popula") %>% 
  mutate(perc = popula / sum(popula),
         total = sum(popula))


race_house <- race_house %>% 
  mutate(hisp_dif = hisp - race$perc[3],
         other_dif = other - race$perc[4],
         wh_dif = wh - race$perc[1],
         blac_dif = blac -  race$perc[2]) %>% 
  pivot_longer(cols = c(hisp_dif, other_dif, wh_dif, blac_dif),
               names_to = "race",
               values_to = "diff") %>% 
  filter(diff == max(diff))



### get data
sales <- dbload("sales")
products <- dbload("products")
stores <- dbload("stores")

t <- products %>%
  filter(grepl("CASE", category)) %>% 
  inner_join(sales, by = c("id" = "product_id_id")) %>% 
  select(product_code, quantity, date, store_id_id) %>% 
  mutate(date = floor_date(date, "month")) %>% 
  filter(date <= "2019-12-01" & date >= "2018-01-01") %>% 
  left_join(stores %>% 
              select(id, zip),
            by = c("store_id_id" = "id")) %>% 
  group_by(zip, product_code) %>% 
  summarise(n = n(),
            n_store = n_distinct(store_id_id),
            qty = sum(quantity, na.rm = TRUE)) %>% 
  filter(qty >= 0) %>% 
  # mutate(mean_qty = qty) %>%
  mutate(flav_perc = qty / sum(qty, na.rm = TRUE)) %>%
  select(zip, product_code, flav_perc) %>%
  pivot_wider(names_from = product_code, values_from = flav_perc)

# t[is.na(t)] <- 0

T1 <- t %>% 
  inner_join(race_house, by = "zip") %>% 
  # select(-c("household", "householdU18", "family", "familyU18")) %>% 
  filter(
    blac > 0 & blac < 1 & wh > 0 & wh < 1 & hisp > 0 & hisp < 1 & other > 0 &
           other < 1
            # &
      # `FP-BLURAZ` < 1 & `FP-BLURAZ` > 0 & `FP-TIGBLO` < 1 &
      #      `FP-TIGBLO` > 0 & `FP-GREAPP` < 1 & `FP-GREAPP` > 0 & `FP-SIMMAN` < 1 &
      #      `FP-SIMMAN` > 0 & `FP-LEMICE` < 1 & `FP-LEMICE` > 0 & `FP-PINCOL` < 1 &
      #      `FP-PINCOL` > 0 & `FP-BERTRI` < 1 &`FP-BERTRI` > 0 &
      #      `FP-GEOPEA` < 1 & `FP-GEOPEA` > 0
         ) #%>%
  # inner_join(ed_income %>% 
  #              mutate(BA_perc = sum(college, Bachelor, na.rm = TRUE) / sum(High, LHigh, college, Bachelor, na.rm = TRUE),
  #                     marry_perc = Married / sum(Married, Cohabit, Female_h, Male_h, na.rm = TRUE),
  #                     work_perc = work / sum(work, noWork, na.rm = TRUE),
  #                     foodst_perc = foodst / sum(foodst, noFoodst, na.rm = TRUE),
  #                     two_more_car = sum(car2, car3, car4, na.rm = TRUE) / 
  #                       sum(car0, car1, car2, car3, car4, na.rm = TRUE)) %>% 
  #              ungroup(zip) %>% 
  #              select(zip, income, BA_perc, marry_perc, 
  #                     work_perc, foodst_perc, two_more_car),
  #            by = "zip") %>% 
  # filter(BA_perc > 0 & BA_perc < 1 & marry_perc > 0 & marry_perc < 1 & 
  #          work_perc > 0 & work_perc < 1 & foodst_perc > 0 & foodst_perc < 1 & 
  #          two_more_car > 0 & two_more_car < 1)


library(betareg)
betaRegression <- function(x) {
  df_lm <- betareg(x ~ `FP-BLURAZ` + `FP-TIGBLO` + `FP-GREAPP` + `FP-SIMMAN` + `FP-LEMICE` + 
                     `FP-PINCOL` + `FP-BERTRI` + `FP-GEOPEA`,
                   data = T1,
                   na.action = "na.omit",
                   link = "logit")
  summary(df_lm)
}
# betaRegression(T1$BA_perc)
# betaRegression(T1$marry_perc)
# betaRegression(T1$work_perc)
# betaRegression(T1$foodst_perc)
# betaRegression(T1$two_more_car)
betaRegression(T1$wh)
betaRegression(T1$blac)
betaRegression(T1$hisp)
betaRegression(T1$other)

# POISSON_flavor <- function(x, y) {
#   df_lm <- glm(x ~ wh + blac + hisp + other,
#                data = T2,
#                poisson(link = "log"),
#                na.action = "na.omit"
#   )
#   summary(df_lm)
# }
# POISSON_flavor(T2$`FP-BLURAZ`)
# POISSON_flavor(T2$`FP-TIGBLO`)
# POISSON_flavor(T2$`FP-GREAPP`)
# POISSON_flavor(T2$`FP-SIMMAN`)
# POISSON_flavor(T2$`FP-LEMICE`)
# POISSON_flavor(T2$`FP-PINCOL`)
# POISSON_flavor(T2$`FP-BERTRI`)
# POISSON_flavor(T2$`FP-GEOPEA`)
# df_lm <- glm(`FP-BLURAZ` ~ wh + blac + hisp + other,
#                  data = T2,
#                  poisson(link = "log"),
#                  na.action = "na.omit"
#                  )
# 
# 
# T2 <- products %>%
#   filter(grepl("CASE", category)) %>% 
#   inner_join(sales, by = c("id" = "product_id_id")) %>% 
#   select(product_code, quantity, date, store_id_id) %>% 
#   mutate(date = floor_date(date, "month")) %>% 
#   filter(date <= "2019-12-01" & date >= "2018-01-01") %>% 
#   left_join(stores %>% 
#               select(id, zip),
#             by = c("store_id_id" = "id")) %>% 
#   group_by(zip, product_code) %>% 
#   summarise(n = n(),
#             n_store = n_distinct(store_id_id),
#             qty = sum(quantity, na.rm = TRUE)) %>% 
#   filter(qty >= 0) %>% 
#   mutate(qty = qty / n_store) %>% 
#   pivot_wider(names_from = product_code, values_from = qty) %>% 
#   select(zip, `FP-BLURAZ`, `FP-TIGBLO`, `FP-GREAPP`, `FP-SIMMAN`, `FP-LEMICE`, 
#            `FP-PINCOL`, `FP-BERTRI`, `FP-GEOPEA`) %>% 
#   inner_join(race_house, by = "zip") %>% 
#   # select(-c("household", "householdU18", "family", "familyU18")) %>% 
#   filter(blac > 0 & blac < 1 & wh > 0 & wh < 1 & hisp > 0 & hisp < 1 & other > 0 &
#            other < 1)





df_lm <- betareg(`FP-BLURAZ` ~ wh + blac + hisp,
             data = T1 %>% 
             filter(`FP-BLURAZ` < 1 & `FP-BLURAZ` > 0),
             na.action = "na.omit")
summary(df_lm)

df_lm <- betareg(`FP-TIGBLO` ~ wh + blac + hisp,
                 data = T1 %>% 
                   filter(`FP-TIGBLO` < 1 & `FP-TIGBLO` > 0),
                 na.action = "na.omit")
summary(df_lm)

df_lm <- betareg(`FP-GREAPP` ~ wh + blac + hisp,
                 data = T1 %>% 
                   filter(`FP-GREAPP` < 1 & `FP-GREAPP` > 0),
                 na.action = "na.omit")
summary(df_lm)

df_lm <- betareg(`FP-SIMMAN` ~ wh + blac + hisp,
                 data = T1 %>% 
                   filter(`FP-SIMMAN` < 1 & `FP-SIMMAN` > 0),
                 na.action = "na.omit")
summary(df_lm)

df_lm <- betareg(`FP-LEMICE` ~ wh + blac + hisp,
                 data = T1 %>% 
                   filter(`FP-LEMICE` < 1 & `FP-LEMICE` > 0),
                 na.action = "na.omit")
summary(df_lm)

df_lm <- betareg(`FP-PINCOL` ~ wh + blac + hisp,
                 data = T1 %>% 
                   filter(`FP-PINCOL` < 1 & `FP-PINCOL` > 0),
                 na.action = "na.omit")
summary(df_lm)

df_lm <- betareg(`FP-BERTRI` ~ wh + blac + hisp,
                 data = T1 %>% 
                   filter(`FP-BERTRI` < 1 & `FP-BERTRI` > 0),
                 na.action = "na.omit")
summary(df_lm)

df_lm <- betareg(`FP-GEOPEA` ~ wh + blac + hisp,
                 data = T1 %>% 
                   filter(`FP-GEOPEA` < 1 & `FP-GEOPEA` > 0),
                 na.action = "na.omit")
summary(df_lm)
