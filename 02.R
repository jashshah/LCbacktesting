# setwd('/home/jash/Lending Club Mega Model/Backtesting Code')
source('functions.R')

# Read the file containing the xgboost and gbm probabilities
gradewise_df <- read_csv("FE 19-6-2017/operations on new data/df_for_backtest.csv") %>%
  filter(term == '36 months')

# Read the file(s) containing the subgrade probabilities

subgrade_13_14 <- read_csv("subgrades/2013_14_subgrades.csv") %>%
  filter(term == 0) %>%
  select(id, subgrade_prob)

subgrade_15 <- read_csv("subgrades/2015_subgrades.csv") %>%
  filter(term == 0) %>%
  select(id, subgrade_prob)

subgrades_df <- rbind(subgrade_13_14, subgrade_15) %>%
  rename(prob_subgrade = subgrade_prob)

rm(subgrade_13_14, subgrade_15)

mydata <- gradewise_df %>%
  left_join(subgrades_df, by = 'id')

# mydata %>%
#   write_csv('mydata.csv')

mydata <- read_csv('mydata.csv')

rm(gradewise_df, subgrades_df)

cut_off <- 0.2
recovery_ratio <- 0.65

# Read each grades separately
md_b <- dataframe_process(mydata = mydata, grade_ = 'B', cutoff = cut_off, recovery = recovery_ratio)
md_c <- dataframe_process(mydata = mydata, grade_ = 'C', cutoff = cut_off, recovery = recovery_ratio)
md_d <- dataframe_process(mydata = mydata, grade_ = 'D', cutoff = cut_off, recovery = recovery_ratio)

md_df <- rbind(md_b, md_c, md_d)

qtr_year <- "2014Q3"

qtr_conf_mat(md_df, quarter_year_ = qtr_year)

qtr_imp_metrics(md_df, quarter_year_ = qtr_year)$accuracy

qtr_alpha(md_df, quarter_year_ = qtr_year)

qtr_grade_mix(md_df, quarter_year_ = qtr_year)

qtr_grade_props(md_df, quarter_year_ = qtr_year)