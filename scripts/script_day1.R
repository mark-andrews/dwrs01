library(tidyverse)

blp_df <- read_csv("data/blp-trials-short.txt")

# select variables `participant`, `lex`, `resp`, `rt`
select(blp_df, participant, lex, resp, rt)

select(blp_df, lex:rt)

select(blp_df, 2:5)

select(blp_df, participant:spell, rt)

select(blp_df, 2:4, rt)

select(blp_df, starts_with('r'))

select(blp_df, starts_with('rt'))

select(blp_df, ends_with('t'))

select(blp_df, contains('rt'))

# equiv of starts_with('r)
select(blp_df, matches('^r'))

# equiv of ends_with('t)
select(blp_df, matches('t$'))

select(blp_df, matches('^rt|rt$'), spell, lex)

select(blp_df, -participant)

select(blp_df, 2:5)

select(blp_df, -(2:5))

select(blp_df, -2:5)

select(blp_df, -contains('rt'))

select(blp_df, spell, participant, lex)

select(blp_df, everything())

select(blp_df, rt, resp, everything())

select_if(blp_df, is.numeric)


# read in csv file with missing values
read_csv('data/missing_example.csv', na = c('NA', '', '--', 'na', '-9'))

has_low_mean <- function(x){
  is.numeric(x) && (max(x, na.rm=T) > 1500)
}

select_if(blp_df, has_low_mean)

select_if(blp_df, function(x){is.numeric(x) && (max(x, na.rm=T) > 1500) })

select_if(blp_df, ~is.numeric(.) && mean(., na.rm=T) < 1000)


# renaming
select(blp_df, lex, spell, reaction_time = rt)

rename(blp_df, reaction_time = rt, lexical = lex)

rename_all(blp_df, ~str_replace_all(., '\\.', 'reaction'))

rename_all(blp_df, ~str_replace_all(., 'rt', 'reaction_time'))

rename_at(blp_df, vars(matches('^rt|rt$')), ~str_replace_all(., 'rt', 'reaction_time'))

rename_at(blp_df, vars(rt, rt.raw), ~str_replace_all(., 'rt', 'reaction_time'))

rename_if(blp_df, is.numeric, toupper)

# slice

# base R
blp_df[1:5, ]

# tidyverse
slice(blp_df, 1:5)

slice(blp_df, c(10, 20, 40, 50, 100))

slice(blp_df, c(10, 20, 40, 50, 100, 500:550))

slice(blp_df, -(1:5))

slice(blp_df, (n()-5):n())

filter(blp_df, lex == 'W')

filter(blp_df, lex == 'W', resp == 'W')

filter(blp_df, rt < 500)

filter(blp_df, lex == 'W', resp == 'W', rt < 500)

filter(blp_df, lex == 'W', resp == 'W', rt < 500)

filter(blp_df, rt > 1000 | rt < 500)

filter(blp_df, rt >= 500 & rt <= 800)
filter(blp_df, rt %in% 500:800)

filter(blp_df, (lex == 'W' & str_length(spell) < 5) | rt > 1000)

filter_all(blp_df, any_vars(is.na(.)))

filter_all(blp_df, all_vars(!is.na(.)))

filter_at(blp_df, vars(rt:rt.raw), all_vars( 500 > .))

filter_at(blp_df, vars(rt:rt.raw), all_vars(. > median(., na.rm = T)))

filter_if(blp_df, is.numeric, all_vars(. > median(., na.rm = T)))


# mutate

mutate(blp_df, accuracy = lex == resp)

# base R
# this will modify the original `blp_df`
blp_df$accuracy <- (blp_df$lex == blp_df$resp)
blp_df[['accuracy']] <- (blp_df[['lex']] == blp_df[['resp']])

mutate(blp_df,
       accuracy = lex == resp,
       word_length = str_length(spell)
)

mutate(blp_df,
       accuracy = lex == resp,
       word_length = str_length(spell),
       fast_rt = rt.raw > median(rt.raw, na.rm = T)
)

mutate(blp_df,
       accuracy = lex == resp,
       word_length = str_length(spell),
       rt.raw = rt.raw > median(rt.raw, na.rm = T)
)

mutate(blp_df,
       rt = as.character(rt)
)
mutate(blp_df,
       rt = as.character(rt),
       rt.raw = as.character(rt.raw)
)

mutate_all(blp_df, as.character)

mutate_at(blp_df, 
          vars(matches('^rt|rt$')),
          log)

blp_df$spell <- as.factor(blp_df$spell)
blp_df$spell <- as.factor(blp_df$spell)

mutate_if(blp_df, is.character, as.factor)

mutate_at(blp_df,
          vars(rt:rt.raw),
          ~as.vector(scale(.))
)

re_scale <- function(x){
  as.vector(scale(x))
}


mutate_at(blp_df,
          vars(rt:rt.raw),
          re_scale)

mutate_at(blp_df,
          vars(rt:rt.raw),
          function(x) as.vector(scale(x)))

mutate(blp_df,
       scale(rt, rt.raw)
)


mutate(blp_df,
       speed = ifelse(rt.raw > median(rt.raw, na.rm = T),
                      'fast',
                      'slow')
)

mutate(blp_df,
       rt = as.vector(scale(rt)),
       speed = ifelse(rt > median(rt, na.rm = T),
                      'fast',
                      'slow')
)

mutate(blp_df,
       lex = recode(lex, 'W' = 'word', 'N' = 'nonword')
)

mutate(blp_df,
       lex = recode(lex, 'W' = 'word', 'N' = 'nonword'),
       resp = recode(resp, 'W' = 'word', 'N' = 'nonword')
)

mutate_at(blp_df,
          vars(lex, resp),
          ~recode(., 'W' = 'word', 'N' = 'nonword')
)


mutate(blp_df,
       rt = recode(rt, `977` = 101, `565` = 102)
)

# speed = ifelse(rt > median(rt, na.rm = T),
#                'fast',
#                'slow')
mutate(blp_df,
       speed = case_when(
         rt > 900 ~ 'fast',
         rt < 600 ~ 'slow',
         TRUE ~ 'medium'
       )
)

mutate(blp_df,
       speed = case_when(
         rt > 900 ~ 'fast',
         rt > 950 ~ 'extra-fast',
         TRUE ~ 'medium'
       )
)

mutate(blp_df,
       speed = case_when(
         rt > 950 ~ 'extra-fast',
         rt > 900 ~ 'fast',
         TRUE ~ 'medium'
       )
)

mutate(blp_df,
       rt_new = rt * 2)

transmute(blp_df,
          rt_new = rt * 2)


transmute(blp_df,
          accuracy = lex == resp,
          word_length = str_length(spell),
          rt.raw = rt.raw > median(rt.raw, na.rm = T)
)

arrange(blp_df, rt)
arrange(blp_df, rt, prev.rt)
arrange(blp_df, lex, rt)

arrange(blp_df, lex, desc(rt))
arrange(blp_df, desc(rt))

# Pipelines
blp_df <- read_csv("data/blp-trials-short.txt")
blp_df <- select(blp_df, participant, rt)

blp_df <- read_csv('data/blp-trials-short.txt') %>% 
  select(participant, rt)


# Version 1: pipeline
blp_df <- read_csv('data/blp-trials-short.txt') %>% 
  mutate(accuracy = lex == resp,
         length = str_length(spell)
  ) %>% 
  select(participant, length, accuracy, rt)

# Version 2: non pipeline 
blp_df <- read_csv('data/blp-trials-short.txt')

blp_df <- mutate(blp_df,
                 accuracy = lex == resp,
                 length = str_length(spell)
)

blp_df <- select(blp_df, participant, length, accuracy, rt)

