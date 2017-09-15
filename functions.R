library(dplyr)
library(lubridate)
library(readr)

# load_file_filter <- function(path, filter_){
#   my_df <- read_csv(path) %>%
#     filter(term == filter_)
#   
#   return(my_df)
# }

dataframe_preprocess <- function(mydata){
  
  df1 <- mydata %>%
    mutate(issue_date = dmy(paste('01', issue_date, sep = '-')), # Change the date
           int_rate = as.numeric(gsub("%", "", int_rate))) %>% 
    select(-starts_with('prob'))
  
  df2 <- mydata %>%
    select(starts_with('prob')) %>%
    mutate(average = rowMeans(.))
  
  df_comb <- cbind(df1, df2)
  
  return(df_comb)
}


add_qtr <- function(mydata){
  
  mydata <- dataframe_preprocess(mydata)
  
  qtr_vec <- mydata %>%
    mutate(quarter = paste('Q', quarter(issue_date), sep = ''),
           quarter_year = paste(year(issue_date), quarter, sep = '')) %>%
    pull(quarter_year)
  
  qtr_vec <- unique(qtr_vec)[order(unique(qtr_vec))]
  
  return(qtr_vec)
}

filter_grade <- function(mydata, grade_){
  mydata <- dataframe_preprocess(mydata)
  
  bt_data <- mydata %>%
    filter(grade == grade_)
  
  return(bt_data)
}


dataframe_process <- function(mydata, grade_, cutoff = 0.2, recovery = 0.65){
  
  bt_data <- filter_grade(mydata, grade_)
  
  bt_data <- bt_data %>%
    mutate(percentile = ecdf(average)(average),
           recovery_amnt = recovery * loan_amnt,
           new_loan_status = if_else(loan_status %in% c('Current', 'Fully Paid'), 
                                     'Paid',
                                     'Default'),
           purchased = if_else(percentile <= cutoff, 
                               'Purchased', 
                               'Not Purchased'),
           returns = if_else(new_loan_status == 'Paid', 
                             1 + ((int_rate - 1)/100)*3,
                             recovery),
           quarter = paste('Q', quarter(issue_date), sep = ''),
           quarter_year = paste(year(issue_date), quarter, sep = '')) %>%
    select(-quarter)
  
  return(bt_data)
  
}

qtr_df <- function(bt_df, quarter_year_){
  
  # Ensure a column called quarter is created in the form YYYYQ1 for example 2013Q3
  
  conf_mat_df <- bt_df %>%
    filter(quarter_year == quarter_year_)
  
  return(conf_mat_df)
}

qtr_conf_mat <- function(bt_df, quarter_year_){
  
  conf_mat_df <- qtr_df(bt_df, quarter_year_)
  
  # Ensure Predicted on the rows and Actuals on the columns
  conf_mat_tbl <- table(Predicted = conf_mat_df$purchased, Actual = conf_mat_df$new_loan_status)
  
  return(conf_mat_tbl)
}

# qtr_conf_mat(bt_df = b_grade_bt_df, quarter_year_ = '2013Q4')

qtr_imp_metrics <- function(bt_df, quarter_year_){
  
  conf_mat_tbl <- qtr_conf_mat(bt_df, quarter_year_)
  
  acc <- (conf_mat_tbl[1, 1] + conf_mat_tbl[2, 2])/sum(conf_mat_tbl)
  prec <- conf_mat_tbl[2, 2]/(conf_mat_tbl[2, 1] + conf_mat_tbl[2, 2])
  rec <- conf_mat_tbl[2, 2]/(conf_mat_tbl[2, 2] + conf_mat_tbl[1, 2])
  
  # print(paste('Accuracy:', round(acc, 3), sep = ' '))
  # print(paste('Precision:', round(prec, 3), sep = ' '))
  # print(paste('Recall:', round(rec, 3), sep = ' '))
  
  return(list(accuracy = round(acc, 3),
              precision = round(prec, 3),
              recall = round(rec, 3)))
  
}

# qtr_imp_metrics(bt_df = b_grade_bt_df, quarter_year_ = '2013Q4')

qtr_grade_mix <- function(bt_df, quarter_year_){
  
  gm_df <- qtr_df(bt_df, quarter_year_)
  
  gm_df_tbl <- table(gm_df$purchased, gm_df$grade)
  
  return(gm_df_tbl)
}

qtr_grade_props <- function(bt_df, quarter_year_){
  
  gm_df <- qtr_df(bt_df, quarter_year_) %>%
    filter(purchased == 'Purchased')
  
  table_prop <- round(prop.table(table(gm_df$grade)), 3)*100
  
  return(table_prop)
  
}


# qtr_grade_props(md_df, quarter_year_ = qtr_year)

qtr_alpha <- function(bt_df, quarter_year_){
  
  conf_mat_df <- qtr_df(bt_df, quarter_year_)
  
  all_returns <- mean(conf_mat_df$returns) # All returns
  
  purchased_returns <- conf_mat_df %>%
    group_by(purchased) %>%
    summarise(mean = mean(returns)) %>%
    filter(purchased == 'Purchased') %>%
    pull(mean)
  
  all_returns_cubed <- all_returns^(1/3)
  purchased_returns_cubed <- purchased_returns^(1/3)
  
  return(list(all_loans = all_returns_cubed - 1,
              purchased_loans = purchased_returns_cubed - 1))

}

# qtr_alpha(b_grade_bt_df, quarter_year_ = '2013Q4')


