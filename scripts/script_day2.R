library(tidyverse)

blp_df <- read_csv('data/blp-trials-short.txt')

mutate(blp_df,
       accuracy = lex == resp)

summarise(blp_df,
          mean_rt = mean(rt.raw, na.rm = T),
          median_rt = median(rt.raw, na.rm = T),
          stdev_rt = sd(rt.raw, na.rm = T),
          accuracy = mean(lex == resp))

          
summarise_all(blp_df, n_distinct)

mean_na <- function(x) mean(x, na.rm = T)

summarise_at(blp_df, vars(rt:rt.raw), mean_na)

summarise_at(blp_df, vars(rt:rt.raw), function(x) mean(x, na.rm = T))

summarise_at(blp_df, vars(rt:rt.raw), ~mean(., na.rm = T))

summarize_if(blp_df, is.numeric, ~mean(., na.rm = T))



summarise_at(blp_df, 
             vars(rt),
             list(avg = ~mean(., na.rm = T),
                  med = ~median(., na.rm = T),
                  stdev = ~sd(., na.rm = T))
)

summarise(blp_df,
          avg_rt = mean(rt, na.rm = T),
          med_rt = median(rt, na.rm = T),
          stdev_rt = sd(rt, na.rm = T),
          avg_rt.raw = mean(rt.raw, na.rm = T),
          med_rt.raw = median(rt.raw, na.rm = T),
          stdev_rt.raw = sd(rt.raw, na.rm = T))


summarise_at(blp_df, 
             vars(rt:rt.raw),
             list(avg = ~mean(., na.rm = T),
                  med = ~median(., na.rm = T),
                  stdev = ~sd(., na.rm = T))
)

group_by(blp_df, lex) %>% 
  summarise(mean_rt = mean(rt, na.rm = T))


group_by(blp_df, lex, resp) %>% 
  summarize(rt = mean(rt.raw, na.rm = T))


# read in data using URL
messy_data_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs01/master/data/example_1_messy.csv")
tidy_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs01/master/data/example_1_tidy.csv")

messy_data_df 

read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs01/master/data/example_1_messy.csv") %>% 
  mutate(delta = stimulus_left_number_of_circles - stimulus_right_number_of_circles,
         choice_left = choice == stimulus_left,
         more_left = delta > 0,
         accuracy = more_left == choice_left) %>% 
  select(subject = ID, age, delta, accuracy, rt = latency) %>%
  mutate(age_group = cut(age, breaks = seq(10, 80, by = 10)),
         age_group = str_remove_all(age_group, '[\\(\\]]') %>% str_replace(',', '-'),
         delta = abs(delta)) %>% 
  group_by(delta) %>% 
  summarise(accuracy = mean(accuracy),
            rt = mean(rt)) %>% 
  ggplot(aes(x = delta, y = accuracy)) + geom_point() + geom_line()


tidy_df_2 %>% 
  group_by(age_group) %>% 
  summarise(accuracy = mean(accuracy),
            rt = mean(rt))




# Combining data frames

Df_1 <- tibble(x = c(1, 2, 3),
               y = c(2, 7, 1),
               z = c(0, 2, 7))

Df_2 <- tibble(y = c(5, 7),
               z = c(6, 7),
               x = c(1, 2))

Df_3 <- tibble(a = c(5, 6, 1),
               b = c('a', 'b', 'c'),
               c = c(T, T, F))

Df_a <- tibble(x = c(1, 2, 3),
               y = c('a', 'b', 'c'))

Df_b <- tibble(x = c(2, 3, 4),
               z = c('d', 'e', 'f'))

Df_4 <- tibble(x = c(1, 2, 3),
               y = c(2, 7, 1),
               z = c(0, 2, 7))

Df_5 <- tibble(a = c(1, 1, 7),
               b = c(2, 3, 7),
               c = c('a', 'b', 'c'))

Df_6 <- tibble(x = c(1, 2, 3),
               y = c(4, 5, 6),
               z = c(7, 8, 9))

Df_7 <- tibble(y = c(6, 7),
               z = c(9, 10),
               x = c(3, 4))


stimuli <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs01/master/data/blp_stimuli.csv")


inner_join(blp_df, stimuli)


inner_join(Df_a, Df_b)
left_join(Df_a, Df_b)
right_join(Df_a, Df_b)
full_join(Df_a, Df_b)



inner_join(blp_df, stimuli)
left_join(blp_df, stimuli)

all_equal(inner_join(blp_df, stimuli),
          left_join(blp_df, stimuli))

right_join(blp_df, stimuli)


inner_join(blp_df, stimuli, by = 'spell')

inner_join(Df_4, Df_5, by = c('x'='a'))

inner_join(Df_4, Df_5, by = c('x' = 'a', 'y' = 'b'))



subjects <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs01/master/data/example_1_subjects.csv")


left_join(tidy_df, subjects, by = c('subject' = 'ID')) %>% 
  group_by(is_mobile) %>% 
  summarise(accuracy = mean(accuracy),
            rt = mean(rt))

          
left_join(tidy_df, subjects, by = c('subject' = 'ID')) %>% 
  select(subject, age, is_mobile) %>% 
  distinct()


x = list(a = 10, b = 5, c = 7)
result <- lapply(x, runif)

lapply(x, runif)
map(x, runif)

data_df <- dir_ls('data/exp_data') %>%
  map(read_csv) %>% 
  bind_rows()

map(file_names, read_csv)

file_names <- dir_ls('data/exp_data')
list_of_dfs <- dir_ls('data/exp_data') %>% map(read_csv)   
final_df <- bind_rows(list_of_dfs)

final_df <- dir_ls('data/exp_data/') %>% map(read_csv) %>% bind_rows()


# Pivots

recall_df <- read_csv('data/repeated_measured_a.csv')

# subject, condition, recall
# Faye, Neg, 26
# Faye, Neu, 12
# Faye, Pos, 42

recall_long <- pivot_longer(data = recall_df,
                            cols = -Subject,
                            names_to = 'condition',
                            values_to = 'recall')

pivot_wider(recall_long,
            names_from = condition,
            values_from = recall)


recall_b_df <- read_csv('data/repeated_measured_b.csv')


pivot_longer(recall_b_df, 
             -Subject, 
             names_to = 'condition', 
             values_to = 'recall') %>% 
  separate(condition, into = c('cue', 'emotion'))

pivot_longer(recall_b_df,
             -Subject,
             names_to = c('cue', 'emotion'),
             names_pattern = '(.*)_(.*)',
             #names_pattern = '(Cued|Free)_(Neg|Neu|Pos)',
             values_to = 'recall')



recall_long_b <- pivot_longer(recall_b_df,
                              -Subject,
                              names_to = c('cue', 'emotion'),
                              names_pattern = '(Cued|Free)_(Neg|Neu|Pos)',
                              values_to = 'recall')

pivot_wider(recall_long_b, 
            names_from = c('cue', 'emotion'), 
            values_from = 'recall',
            names_sep = '.')


summarise_at(blp_df, 
             vars(rt:rt.raw),
             list(avg = ~mean(., na.rm = T),
                  med = ~median(., na.rm = T),
                  stdev = ~sd(., na.rm = T))
) %>% pivot_longer(cols = everything(),
                   names_to = 'var_summary',
                   values_to = 'descriptive') %>% 
  separate(var_summary, into = c('variable', 'summary'), sep = '_') %>% 
  pivot_wider(names_from = summary, values_from = descriptive)

tmp_df <- summarise_at(blp_df, 
             vars(rt:rt.raw),
             list(avg = ~mean(., na.rm = T),
                  med = ~median(., na.rm = T),
                  stdev = ~sd(., na.rm = T))
) 

tmp_df %>% pivot_longer(cols = everything(),
                        names_to = c('variable', '.value'),
                        names_pattern = "(.*)_(.*)")
  

download.file("https://raw.githubusercontent.com/mark-andrews/dwrs01/master/data/example_2_messy.xlsx",
              destfile = 'data/example_2_messy.xlsx')

tmp_df <- read_excel('data/example_2_messy.xlsx')

tmp_df %>% rename(subject = `unique identification name`) %>% 
  rename_at(vars(Hate:Mad), ~str_c('List_1', ., sep = ' ')) %>% 
  rename_at(vars(Over:Clouds), ~str_c('List_2', ., sep = ' ')) %>%
  pivot_longer(cols = starts_with('List_'), names_to = 'list_item', values_to = 'response') %>% 
  separate(list_item, into = c('list', 'item'), sep = ' ')

tmp_df %>% rename(subject = `unique identification name`) %>% 
  rename_at(vars(Hate:Mad), ~str_c('List_1', ., sep = ' ')) %>% 
  rename_at(vars(Over:Clouds), ~str_c('List_2', ., sep = ' ')) %>%
  pivot_longer(cols = starts_with('List_'), 
               names_to = c('list', 'item'), 
               names_sep = ' ',
               values_to = 'response') %>% 
  mutate(list = as.integer(str_remove(list, 'List_')),
         response = as.integer(str_extract(response, '[1234]'))) %>% 
  mutate_at(vars('gender', 'item'), str_to_lower)




tidy_df_2 <- read_csv("https://raw.githubusercontent.com/mark-andrews/dwrs01/master/data/example_1_messy.csv") %>% 
  mutate(delta = stimulus_left_number_of_circles - stimulus_right_number_of_circles,
         choice_left = choice == stimulus_left,
         more_left = delta > 0,
         accuracy = more_left == choice_left) %>% 
  select(subject = ID, age, delta, accuracy, rt = latency) %>%
  mutate(age_group = cut(age, breaks = seq(10, 80, by = 10)),
         age_group = str_remove_all(age_group, '[\\(\\]]') %>% str_replace(',', '-'),
         delta = abs(delta)) 


tidy_df_2 %>% 
  group_by(subject) %>% 
  summarize(n = n()) %>% 
  arrange(n)

subject_n_df <- tidy_df_2 %>% 
  group_by(subject) %>% 
  tally()

left_join(tidy_df_2, subject_n_df, by = 'subject') %>% 
  filter(n > 90)


tidy_df_2 %>% 
  group_by(subject) %>% 
  nest() %>% 
  mutate(n = map_int(data, nrow)) %>% 
  unnest(cols = data) %>% 
  filter(n > 50) %>% 
  select(-n)

list_of_dfs <- dir_ls('data/exp_data/') %>% map(read_csv)
map_int(list_of_dfs, nrow)



x <- tidy_df_2 %>% 
  group_by(subject) %>% 
  nest() %>% 
  mutate(beta = map_dbl(data, ~coef(lm(rt ~ delta, data = .))[2]))
