##################)
### Author: Peter Kress
### Date: 2022/04/06
### Purpose: Analyze AITA posts and comments
##################)
#' # Is Polarization Popular on AITA?
#' 
#' I examined the top comments from top AITA subreddit posts from 2018-2019 
#' to explore whether
#' popular posts are associated with more engaged and polarized comments section.
#' 
#' AITA is a subreddit seeking to give access to crowdsourced social judgement
#' to clarify those sticky situations where we aren't quiite sure if
#' we're being an A-hole.
#' 
#' The analysis is comprised of three main steps: 
#' 
#' * Determining basic descriptive facts about the top posts
#' 
#' * Determining if more intense and balanced posts are more popular
#' 
#' * Determining if more polarizing posts more popular
#' 
#' Eventually, we seek to extend this analysis by exploring how post 
#' characteristics
#' (e.g. age of poster, family vs relationship content) may impact community 
#' responses to determine which biases manifest in this social judgement context.
#' Such an analysis would build off the analysis in Alice Wu 2019 (
#' here: https://scholar.harvard.edu/files/alicewu/files/wu_ejr_paper_2019.pdf)
#' and Ferrer et al 2020 (here: https://arxiv.org/pdf/2008.02754.pdf).
#' 

#+include = F
##################)
# Initialize Workspace ----
##################)
#+ include = F
## Paths ----
setwd("~/Documents/Personal Projects/AITA/")

## Packages ----

if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, magrittr, stringr, ggplot2
       , lubridate
       , tm, syuzhet)

## Handy Functions ----
`%p%` = paste0

month_diff = function(d1, d2){
  12*(year(d2) - year(d1)) + (month(d2) - month(d1))
}

make_title = function(x){ str_to_title(gsub("_", " ", x))}

clean_labs = function(ggp){
  out = copy(ggp)
  for (lab in names(ggp$labels)) {
    out$labels[[lab]] = make_title(out$labels[[lab]])
  }
  return(out)
}

##################)
# Read in Data ----
##################)
#+ include = F
posts = fread("~/Documents/Personal Projects/AITA/data/intermediate/top_1_300_posts.csv")
comments = fread("~/Documents/Personal Projects/AITA/data/intermediate/top_1_300_posts_top_50_comments.csv")

##################)
# Clean Data ----
##################)
#+ include = F
## Clean Post data ----
post_cln = posts[## Select Columns
  , mget(c("id", "created", "author"
           , "title", "selftext", "link_flair_text"
           , "score", "num_comments"))
  ][## Clean data
  , `:=`(selftext=iconv(selftext, from = "ISO-8859-1", to = "UTF-8")
         , created = as.POSIXct(created, origin = "1970-01-01", tz="UTC")
         , link_flair_text = fifelse(link_flair_text=="Asshole", "A-hole"
                                     , link_flair_text))
  ][## Drop deleted Posts
  !selftext%in%c("[removed]", "[deleted]")
  ]

## Clean Comment data ----
comment_cln = comments[## Select Columns
  , mget(c("parent_id", "id", "created", "author"
           , "body", "controversiality", "is_submitter"
           , "score", "reply_count"))
  ][## Clean data
    , `:=`(body=iconv(body, from = "ISO-8859-1", to = "UTF-8")
           , created = as.POSIXct(created, origin = "1970-01-01", tz="UTC")
           , parent_id = str_sub(parent_id, 4L, -1L))
  ][## Drop deleted comments
  !body%in%c("[removed]", "[deleted]")
  ][## Add NTA/YTA/ESH/NAH flags
  , comment_result := fifelse(
    substr(body_comment, 1, 3)%in%c("NTA", "YTA", "ESH", "NAH")
    , body_comment, "no_result")
  ] %>%
  setnames(c("id", "id_comment", "created_comment", "author_comment"
             , "body_comment", "controversiality_comment", "is_submitter_comment"
             , "score_comment", "reply_count_comment"))


## Add Sentiment ----
## Get sentiment
post_sentiment = str_replace_all(post_cln$selftext, c('\"\"'="'")) %>%
  get_nrc_sentiment()
post_words = str_replace_all(post_cln$selftext, c('\"\"'="'")) %>%
  str_count(" ") %>%
  data.table(words = .) %>%
  .[, words:=words + 1]
post_sentiment_dt = cbind(post_cln[, .(id)]
                          , post_sentiment
                          , post_words) %>%
  setDT()

## Add intensity and balance
post_sentiment_dt[
  , `:=`(intensity = rowSums(.SD)/words
         , balance = positive/(negative + positive))
  , .SDcols = -c("id", "positive", "negative", "words")
]

## Merge to post data
post_cln[## Add all columns from sentiment data not in post data
  post_sentiment_dt
  , setdiff(names(post_sentiment_dt), names(post_cln)):=
    mget("i."%p%setdiff(names(post_sentiment_dt), names(post_cln)))
  , on = intersect(names(post_sentiment_dt), names(post_cln))
]


## Get sentiment
comment_sentiment = str_replace_all(comment_cln$body_comment, c('\"\"'="'")) %>%
  get_nrc_sentiment()
comment_words = str_replace_all(comment_cln$body_comment, c('\"\"'="'")) %>%
  str_count(" ") %>%
  data.table(words = .) %>%
  .[, words:=words + 1]

comment_sentiment_dt = cbind(comment_cln[, .(id = id_comment)]
                             , comment_sentiment
                             , comment_words)%>%
  setDT()

## Add intensity and balance
comment_sentiment_dt[
  , `:=`(intensity = rowSums(.SD)/words
         , balance = positive/(negative + positive))
  , .SDcols = -c("id", "positive", "negative", "words")
  ]%>%
  setnames(names(.), names(.)%p%"_comment")

## Merge to Comment data
comment_cln[## Add all columns from sentiment data not in comment data
  comment_sentiment_dt
  , setdiff(names(comment_sentiment_dt), names(comment_cln)):=
    mget("i."%p%setdiff(names(comment_sentiment_dt), names(comment_cln)))
  , on = intersect(names(comment_sentiment_dt), names(comment_cln))
]

## Merge data ----
top_posts = post_cln[
  comment_cln
  , on = c("id")
]


## Save Data ----
fwrite(post_cln, "data/intermediate/posts_cln.csv")
fwrite(comment_cln, "data/intermediate/comments_cln.csv")
fwrite(top_posts, "data/intermediate/merged_comments_and_posts_cln.csv")

## Load Saved Data ----
post_cln = fread("~/Documents/Personal Projects/AITA/data/intermediate/posts_cln.csv")
comment_cln = fread("~/Documents/Personal Projects/AITA/data/intermediate/comments_cln.csv")
top_posts = fread("~/Documents/Personal Projects/AITA/data/intermediate/merged_comments_and_posts_cln.csv")

##################)
# Basic Descriptive Facts ----
##################)

#' # Basic Descriptive Facts
#' 
#' We want to explore the overall distributions of the key variables in this 
#' analysis, and confirm that the data adhere to our expectations. We focus this
#' analysis on comments/replies, score, intensity, and balance. 
#' 
#' * Comments/replies refers to the number of comments or replies that a given
#' post or comment receives.
#' 
#' * Score refers to the net upvotes a post or comment receives. 
#' 
#' * Intensity is the ratio of the sum of words from 8 emotions to total words 
#' in a given post or comment. The values are calculated based on matching the 
#' words in the post or comment to the NRC dictionary 
#' (Saif Mohammad’s NRC Emotion lexicon, 
#' see http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm). A post with 
#' a higher intensity has more emotionally laden words. 
#' 
#' * Balance is the ratio of the positive valance value to the sum of the 
#' positive and negative valance values. Again, the values are derived from the
#' NRC dictionary. A balanced post will have a balance of
#' 0.5, indicating that there are as many positive words as there are negative
#' words. 
#' 
#' ## Overall
#' 
#' First, we consider the data posts overall by checking if censoring over time 
#' is a big driver of comment counts or 
#' score. The following plots indicate that censoring isn't a driving issue.

#+ include = T, message=F, warning=F, echo = F, fig.height = 6, fig.width = 9
post_cln %>% 
  ggplot()+
  geom_boxplot(aes(lubridate::round_date(created, "month"), num_comments
                   , group = lubridate::round_date(created, "month")))+
  labs(x = "Month", y = "Comments Per Post"
       , title = "Distribution of Comments per Post over time")+
  theme_bw()
post_cln %>% 
  ggplot()+
  geom_boxplot(aes(lubridate::round_date(created, "month"), score
                   , group = lubridate::round_date(created, "month")))+
  labs(x = "Month", y = "Score"
       , title = "Distribution of Score over time")+
  theme_bw()

#' ## Distributions of key variables in Posts
#' 
#' We consider the distributions of post comments, score, intensity and balance
#' to identify outliers or observations that should be dropped. 
#' 
#' We don't see anything unusual among the non-deleted posts. 

#+ include = T, message=F, warning=F, echo = F, fig.height = 6, fig.width = 9
post_cln %>% 
  ggplot()+
  geom_histogram(aes(x = num_comments))+
  labs(x = "Comments per Post", y = "Posts"
       , title = "Distribution of Comments per Post")+
  theme_bw()

post_cln %>% 
  ggplot()+
  geom_histogram(aes(x = score))+
  labs(x = "Score", y = "Posts"
       , title = "Distribution of Post Scores")+
  theme_bw()


#' Having checked the marginal distributions of comments and score, we also
#' want to consider the joint distribution. 
#' 
#' For both level-level and log-log, comments and score are correlated which is
#'  as we might expect. A 1 point increase in score is associated with a 0.05 
#' increase in comments, and a 1 percent increase in score is associated with 
#' a 0.47 percent increase in comments. 
#' 

#+ include = F
lev_lev_plot = post_cln %>% 
  ggplot()+
  geom_point(aes(x = score,y=num_comments))+
  geom_smooth(aes(x = score,y=num_comments), method = "lm")+
  theme_bw()+
  labs(x = "Post Score", y = "Comments"
       , title = "Comparison of Post Comments and Score")

lev_lev = post_cln[
  , fixest::feols(num_comments~score, data = .SD)
]

log_log_plot = post_cln%>% 
  ggplot()+
  geom_point(aes(x = score,y=num_comments))+
  geom_smooth(aes(x = score,y=num_comments), method = "lm")+
  theme_bw()+
  labs(x = "Post Score", y = "Comments"
       , title = "Comparison of Post Comments and Score (Log-Log)")+
  scale_x_log10()+
  scale_y_log10()

log_log = post_cln[
  , fixest::feols(log(num_comments)~log(score), data = .SD)
]
#+ include = T, message=F, warning=F, echo = F, fig.height = 6, fig.width = 9
ggpubr::ggarrange(lev_lev_plot, log_log_plot, ncol = 2, nrow = 1)
fixest::etable(list(lev_lev = lev_lev, log_log = log_log))


#' Lastly, we want to check the distributions of intensity and balance. 
#' 
#' We observe that intensity is somewhat right skewed, so most posts
#' tend to be less intense than the most extreme posts. 
#' 
#' We observe that balance is fairly evenly distributed, but is centered
#' above 0.5. This indicates that balance varies by post, but tends to be a bit
#' more positive than negative.


#+ include = T, message=F, warning=F, echo = F, fig.height = 6, fig.width = 9
post_cln %>% 
  ggplot()+
  geom_histogram(aes(x = intensity))+
  labs(x = "Intensity", y = "Posts"
       , title = "Distribution of Post Intensity")+
  theme_bw()

post_cln%>% 
  ggplot()+
  geom_histogram(aes(x = balance))+
  labs(x = "Balance", y = "Posts"
       , title = "Distribution of Post Balance")+
  theme_bw()


#' ## Distributions of key variables in Comments
#' 
#' We consider the distribution of comment replies, score, intensity and balance
#' to identify outliers or observations that should be dropped. 
#' 
#' We notice that an enormous share of comments have 1 upvote. Since this may
#' be a self-voted value and is therefore unrelated to a replies impact on
#' other people, we don't consider single upvote comments when investigating 
#' comment scores. 

#+ include = T, message=F, warning=F, echo = F, fig.height = 6, fig.width = 9
comment_cln %>% 
  ggplot()+
  geom_histogram(aes(x = reply_count_comment))+
  labs(x = "Replies per Comment", y = "Comments"
       , title = "Distribution of Replies per Comment")+
  theme_bw()+
  scale_x_log10()

comment_cln %>% 
  ggplot()+
  geom_histogram(aes(x = score_comment))+
  labs(x = "Score", y = "Comments"
       , title = "Distribution of Score per Comment")+
  theme_bw()+
  scale_x_log10()


#' Having checked the marginal distributions of replies and score, we also
#' want to consider the joint distribution. 
#' 
#' For both level-level and log-log, replies and score are correlated which is
#'  as we might expect. A 1 point increase in score is associated with a 0.003 
#' increase in replies, and a 1 percent increase in score is associated with 
#' a 0.32 percent increase in comments. 
#' 
#' Including post fixed effects, we observe similar correlations: 
#' 0.003 and 0.34 respectively. 
#' 


#+ include = F

lev_lev_plot = comment_cln[score_comment>1] %>% 
  ggplot()+
  geom_point(aes(x = score_comment, y = reply_count_comment))+
  geom_smooth(aes(x = score_comment, y = reply_count_comment), method = "lm")+
  theme_bw()+
  labs(x = "Comment Score", y = "Replies"
       , title = "Comparison of Comment Replies and Score")

log_log_plot = comment_cln[score_comment>1] %>% 
  ggplot()+
  geom_point(aes(x = score_comment, y = reply_count_comment))+
  geom_smooth(aes(x = score_comment, y = reply_count_comment), method = "lm")+
  theme_bw()+
  labs(x = "Comment Score", y = "Replies"
       , title = "Comparison of Comment Replies and Score")+
  scale_x_log10()+
  scale_y_log10()

lev_lev = comment_cln[score_comment>1] %>% 
  fixest::feols(reply_count_comment~score_comment)
lev_lev_post_fe = comment_cln[score_comment>1] %>% 
  fixest::feols(reply_count_comment~score_comment|id)
log_log = comment_cln[score_comment>1] %>% 
  fixest::feols(log(reply_count_comment)~log(score_comment))
log_log_post_fe = comment_cln[score_comment>1] %>% 
  fixest::feols(log(reply_count_comment)~log(score_comment)|id)

#+ include = T, message=F, warning=F, echo = F, fig.height = 6, fig.width = 9
ggpubr::ggarrange(lev_lev_plot, log_log_plot, ncol = 2, nrow = 1)
fixest::etable(lev_lev, log_log, lev_lev_post_fe, log_log_post_fe)


#' Lastly, we want to check the distributions of intensity and balance. 
#' 
#' We observe that intensity ranges greatly and is severely right skewed.
#' Most comments tend to be very unintense, but some are very intense. Note that
#' values above 1 come from comments with words that appear in multiple 
#' emotions. 
#' 
#' We observe that balance is fairly evenly distributed, but is concentrated 
#' at 0, 0.5, and 1, as well as 1/3, 2/3, 1/4, 3/4, and other fractions. This
#' is because most comments are much shorter than posts, and so often have few 
#' if any valance (positive or negative) words. 

#+ include = T, message=F, warning=F, echo = F, fig.height = 6, fig.width = 9
comment_cln%>% 
  ggplot()+
  geom_histogram(aes(x = intensity_comment))+
  labs(x = "Intensity", y = "Comments"
       , title = "Distribution of Comment Intensity")+
  theme_bw()

comment_cln%>% 
  ggplot()+
  geom_histogram(aes(x = balance_comment))+
  labs(x = "Balance", y = "Comments"
       , title = "Distribution of Comment Balance")+
  theme_bw()


##################)
# Post Intensity, Balance and Popularity ----
##################)
#' # Post Intensity, Balance and Popularity
#' 
#' We now turn to the relationship between post intensity and popularity. We
#' expect that more intense posts are likely to be more popular since 
#' many forum posters don't engage unless moved emotionally. Post intensity 
#' measures the emotional impact of a post, so we expect more
#' intense posts to correspond to more engaging posts. 
#' 
#' We also consider whether 
#' balanced or unbalanced posts are more popular. On the one hand, balanced
#' posts are likely to be more moderate in tone and thereby less engaging. 
#' On the other hand, unbalanced posts may be alienating or unambiguous, 
#' rendering them uninteresting.

## Intensity and Popularity ----
#' ## Intensity and Popularity
#' 
#' We measure intensity, as described above, based on the share of words in a 
#' post that correspond to emotional responses in the NRC emotion lexicon. 
#' 
#' We run a regression of post score on intensity, and find some 
#' correlation but no obvious trend. Similarly, post comments appear mostly 
#' unrelated to post intensity. As a result, the role of intensity and 
#' score remains inconclusive. 
#' 
#' One interesting takeaway is that 
#' the least intense posts (intensity<=0.06) are all low score/comments, while 
#' the some of the highest intensity posts have high scores and many commments
#' (intensity>=0.15). Additionally, almost all the very high score/comment posts
#' are in the middle intenstiy range. 
#' 
#' Perhaps there is some negative engagement response associated with overly 
#' bland or intense posts, though with the current lack of correlation it's hard 
#' to say.
#' 
#' Further analysis with a larger sample size and evaluation of specific 
#' emotions may give more insight into the relationship of emotional intensity
#' and post popularity. 


#+ include = F

lev_lev_plot = post_cln %>% 
  ggplot()+
  geom_point(aes(x = intensity, y = score))+
  geom_smooth(aes(x = intensity, y = score), method = "lm")+
  theme_bw()+
  labs(x = "Post Intensity", y = "Score"
       , title = "Comparison of Intensity and Score")

log_lev_plot =  post_cln %>% 
  ggplot()+
  geom_point(aes(x = intensity, y = score))+
  geom_smooth(aes(x = intensity, y = score), method = "lm")+
  theme_bw()+
  labs(x = "Post Intensity", y = "Score"
       , title = "Comparison of Intensity and Score")+
  scale_y_log10()

lev_lev = post_cln %>% 
  fixest::feols(score~intensity)
log_lev = post_cln %>% 
  fixest::feols(log(score)~intensity)

#+ include = T, message=F, warning=F, echo = F, fig.height = 6, fig.width = 9
ggpubr::ggarrange(lev_lev_plot, log_lev_plot, ncol = 2, nrow = 1)
fixest::etable(lev_lev, log_lev)

#+ include = F

lev_lev_plot = post_cln %>% 
  ggplot()+
  geom_point(aes(x = intensity, y = num_comments))+
  geom_smooth(aes(x = intensity, y = num_comments), method = "lm")+
  theme_bw()+
  labs(x = "Post Intensity", y = "Comments"
       , title = "Comparison of Intensity and Comments")

log_lev_plot =  post_cln %>% 
  ggplot()+
  geom_point(aes(x = intensity, y = num_comments))+
  geom_smooth(aes(x = intensity, y = num_comments), method = "lm")+
  theme_bw()+
  labs(x = "Post Intensity", y = "Comments"
       , title = "Comparison of Intensity and Comments")+
  scale_y_log10()

lev_lev = post_cln %>% 
  fixest::feols(num_comments~intensity)
log_lev = post_cln %>% 
  fixest::feols(log(num_comments)~intensity)

#+ include = T, message=F, warning=F, echo = F, fig.height = 6, fig.width = 9
ggpubr::ggarrange(lev_lev_plot, log_lev_plot, ncol = 2, nrow = 1)
fixest::etable(lev_lev, log_lev)

## Balance and Popularity ----
#' ## Balance and Popularity
#' 
#' We measure balance, as described above, based on the share of words in a 
#' post that correspond to a valance response in the NRC emotion lexicon. 
#' 
#' We run a regression of post score on balance, and find some 
#' correlation but no obvious trend. Similarly, post comments appear mostly 
#' unrelated to post balance. As a result, the role of balance and 
#' score remains inconclusive. 
#' 
#' One interesting takeaway is that almost all the most positive posts have low 
#' scores, while very negative posts have more positive scores. Additionally, 
#' nearly all the highest scored/most commented as well as the least commented
#' posts are in the middle range of balance. 
#' 
#' 
#' Perhaps there is some negative engagement response associated with overly 
#' positive or negative posts, though with the current lack of correlation it's 
#' hard to say.
#' 
#' Further analysis with a larger sample size may give more insight into the 
#' relationship of emotional balance and post popularity. 


#+ include = F

lev_lev_plot = post_cln %>% 
  ggplot()+
  geom_point(aes(x = balance, y = score))+
  geom_smooth(aes(x = balance, y = score), method = "lm")+
  theme_bw()+
  labs(x = "Post Balance", y = "Score"
       , title = "Comparison of Balance and Score")

log_lev_plot =  post_cln %>% 
  ggplot()+
  geom_point(aes(x = balance, y = score))+
  geom_smooth(aes(x = balance, y = score), method = "lm")+
  theme_bw()+
  labs(x = "Post Balance", y = "Score"
       , title = "Comparison of Balance and Score")+
  scale_y_log10()

lev_lev = post_cln %>% 
  fixest::feols(score~balance)
log_lev = post_cln %>% 
  fixest::feols(log(score)~balance)

#+ include = T, message=F, warning=F, echo = F, fig.height = 6, fig.width = 9
ggpubr::ggarrange(lev_lev_plot, log_lev_plot, ncol = 2, nrow = 1)
fixest::etable(lev_lev, log_lev)

#+ include = F

lev_lev_plot = post_cln %>% 
  ggplot()+
  geom_point(aes(x = balance, y = num_comments))+
  geom_smooth(aes(x = balance, y = num_comments), method = "lm")+
  theme_bw()+
  labs(x = "Post Balance", y = "Comments"
       , title = "Comparison of Balance and Comments")

log_lev_plot =  post_cln %>% 
  ggplot()+
  geom_point(aes(x = balance, y = num_comments))+
  geom_smooth(aes(x = balance, y = num_comments), method = "lm")+
  theme_bw()+
  labs(x = "Post Balance", y = "Comments"
       , title = "Comparison of Balance and Comments")+
  scale_y_log10()

lev_lev = post_cln %>% 
  fixest::feols(num_comments~balance)
log_lev = post_cln %>% 
  fixest::feols(log(num_comments)~balance)

#+ include = T, message=F, warning=F, echo = F, fig.height = 6, fig.width = 9
ggpubr::ggarrange(lev_lev_plot, log_lev_plot, ncol = 2, nrow = 1)
fixest::etable(lev_lev, log_lev)

#+ include = F
##################)
# Polarization and Popularity ----
##################)
#' # Polarization and Popularity
#' 
#' We now turn to the relationship between polarization and popularity. We
#' expect that more polarizing posts are likely to be more popular since 
#' many forum posters don't engage unless moved emotionally. Comment 
#' polarization measures the emotional impact of a post, so we expect more
#' polarization to correspond to more engaging posts. 
#' 
#' While polarization may be a good measure of emotional engagement, other 
#' explanations may exist. For example, non-polarized posts from particularly 
#' humorous or outlandish posts may also excel on the platform. This analysis 
#' seeks to determine whether polarization is indeed associated with popularity 
#' on AITA, which gives some indication into whether the most engaging posts are 
#' divisive. 
#' 
#' We measure polarization in two ways: emotional polarization of comments and
#' voting breakdowns of comments. 
#'  

## Emotional Polarization ----
#' ## Emotional Polarization and Popularity
#' 
#' We construct an index of polarization for each post based on intensity and 
#' balance of its comments. A post is highly polarized if there are many intense 
#' comments that disagree in terms of balance.
#' 
#+ include = F

post_polar = top_posts[
  
  ]

post_cln[between(score, 15000, 70000)] %>% 
  ggplot()+
  geom_point(aes(x = intensity,y=score))+
  geom_smooth(aes(x = intensity,y=score), method = "lm")+
  theme_bw()+
  labs(x = "Intensity", y = "Post Score"
       , title = "Comparison of Post Score and Intensity")+
  scale_y_log10()

post_cln[between(score, 15000, 70000)] %>% 
  ggplot()+
  geom_point(aes(x = balance,y=score))+
  geom_smooth(aes(x = balance,y=score), method = "lm")+
  theme_bw()+
  labs(x = "Balance", y = "Post Score"
       , title = "Comparison of Post Score and Balance")

post_cln[between(score, 15000, 70000)] %>% 
  ggplot()+
  geom_point(aes(x = abs(balance-0.5),y=score))+
  geom_smooth(aes(x =  abs(balance-0.5),y=score), method = "lm")+
  theme_bw()+
  labs(x = "Balance", y = "Post Score"
       , title = "Comparison of Post Score and Imbalance Magnitude")+
  scale_y_log10()

## Comments ----



##################)
# Compare polarization to popularity ----
##################)

post_cln[
  nchar(link_flair_text)>0
] %>% 
  ggplot()+
  geom_histogram(aes(x = intensity))+
  facet_wrap(~link_flair_text, scales = "free_y")+
  theme_bw()+
  labs(x = "Post Intensity", y = "Posts"
       , title = "Post Intensity Distributions by Post Type")
post_cln[
  nchar(link_flair_text)>0
] %>% 
  ggplot()+
  geom_histogram(aes(x = balance))+
  facet_wrap(~link_flair_text, scales = "free_y")+
  theme_bw()+
  labs(x = "Post Balance", y = "Posts"
       , title = "Post Balance Distributions by Post Type")





