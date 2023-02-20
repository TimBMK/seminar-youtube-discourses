{
  library(tidyverse)
  library(tuber)
  library(lubridate)
  library(rytstat)
  library(igraph)
  library(ggraph)
}

## tuber ##
yt_oauth("81481321824-btqvdmev607hbbq3n34i4qpb194spdq7.apps.googleusercontent.com", 
         "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")
# set up the API access via the google development console. Create a new project with access to all yotube APIs. 
#   See: https://developers.google.com/youtube/v3/getting-started
# Set up OAuth 2.0 to pass the OAuth credentials to tuber
#   See: https://developers.google.com/youtube/v3/guides/authentication
# Remember to add the local host address used by the tuber authentication to the trusted URIs in the project:
#   Add "http://localhost:1410/" to the authorised forwarding URIs under OAauth 2.0 access modification (the small pencil icon)
#   See: https://stackoverflow.com/questions/60314547/error-redirect-uri-mismatch-google-api-how-to-fix
# If you get a 401 error, rerun this line of code to re-authenticate



comments <- get_all_comments(video_id = "P8f-Qb-bwlU")
# not all comments seem to be retrieved - video description in browser states 5.606 comments, but 4.740 are retrieved
#   Similarly for video P701paKEMXs, where only 11.398 out of 16.718 comments are retrieved
#   This seems to be a known issue: https://github.com/soodoku/tuber/issues/33
#   See also: https://github.com/soodoku/tuber/issues/52
#   It seems that, according to the API documentation, the endpoint does not necessarily return all replies to a comment: 
#         https://github.com/soodoku/tuber/issues/52#issuecomment-818951422
#   => it seems that, in order to get all replies to all comments, we need to scrape them via the comments.list method
# note that all columns are parsed as chr, also like counts, dates etc
# every requests uses one of 10.000 daily quota unit. See: https://developers.google.com/youtube/v3/getting-started#quota
# Additional documentation: 
#         https://developers.google.com/youtube/v3/docs/comments/list
#         https://developers.google.com/youtube/v3/docs/commentThreads/list

comments <- as_tibble(comments) %>%     # as_tibble for performance
  mutate(
    publishedAt = ymd_hms(publishedAt), # mutate to proper date time format
    updatedAt = ymd_hms(updatedAt),     # mutate to proper date time format
    likeCount = as.numeric(likeCount),  # like count as proper numbers
    canRate = as.logical(canRate)       # logicals to proper logicals
  )

#### ! the get_comment_threads() function is buggy and never returns more than 100 results. See https://github.com/soodoku/tuber/issues/107 ! ####
# comment_replies <- get_comment_threads(filter = c(video_id = "P8f-Qb-bwlU"),
#                                        max_results = Inf,
#                                        part = "snippet",
#                                        simplify = T)
# 
# comment_replies <- as_tibble(comment_replies) %>%     # as_tibble for performance
#   mutate(
#     publishedAt = ymd_hms(publishedAt), # mutate to proper date time format
#     updatedAt = ymd_hms(updatedAt),     # mutate to proper date time format
#     likeCount = as.numeric(likeCount),  # like count as proper numbers
#     canRate = as.logical(canRate)       # logicals to proper logicals
#   )
# 
# comment_replies_ids <- get_comment_threads(filter = c(video_id = "P8f-Qb-bwlU"),
#                                            max_results = Inf,
#                                            part = "id",
#                                            simplify = T)
# 
# all_comments <- comments %>% bind_rows(comment_replies) %>% distinct()
#####

# get related videos
related_videos <- get_related_videos(video_id = "P8f-Qb-bwlU", 
                                     max_results = 50) # only 50 related videos can be returned (API limit)


# function to get related videos of the related videos
get_related_related_videos <- function(video_ids, max_results, ...) { # ... passes additional arguments to get_related_video
  result <- tibble() # it might be more efficient to set the cols and expected rows of the container here (and delete empty rows later)
  for (id in video_ids) {
    videos <- get_related_videos(video_id = id, max_results = max_results, ...)
    result <- result %>% bind_rows(videos)
  }
  return(result)
}


# get two-hop related videos
related_related_videos <- get_related_related_videos(related_videos$rel_video_id, 50)


# prepare graph data
graph_data <- related_videos %>% select(video_id, rel_video_id) %>% # take video and related video IDs from original video
  bind_rows(related_related_videos %>% select(video_id, rel_video_id)) # and add the related related videos to the DF

# make graph
graph <- graph_from_data_frame(graph_data)


# add some attributes
attributes <- tibble(name = V(graph)$name) %>% 
  left_join(related_videos %>% bind_rows(related_related_videos) %>% 
              distinct(rel_video_id, title) %>%  # it is important to remove duplicats here to maintain the same number and order of entries as in the graph
              mutate(root = ifelse(rel_video_id == unique(related_videos$video_id), TRUE, FALSE)), # add root indicator for original video
    by = c("name" = "rel_video_id"))
  

V(graph)$title <- attributes$title # add video title to graph

V(graph)$root <- attributes$root   # add root indicator to graph


# calculate communities
#communities <- cluster_infomap
communities <- cluster_leiden(as.undirected(graph), objective_function = "modularity") # temporarily convert the graph to undirected

V(graph)$community <- membership(communities)

# visualize graph
ggraph(graph, layout = "kk") +
  geom_edge_arc(color = "darkgrey") +
  geom_node_point(aes(size = root, color = as.factor(community)))





## .... we could sample a number of videos of different categories and see how well they connect through their suggested videos
##  tweaking the get_related_related_videos to do several hops would be useful then (don't forget try() for running out of API capacity, then)

## Alternatively: get comments for videos and recommended videos, check user overlap or term overlap

# check yt_topic_search() and get_captions()




## rytstat ## 

# run authorization - this does not require setting up your own google project (but that can be used as well)
ryt_auth("tim.b.m.koenig@live.de")

comments_2 <- ryt_get_comments(video_id = "P8f-Qb-bwlU")
# very slow requests
# retrieves only 4.417 out of 5.606 comments
# more messy format, but additional variables - that seem mostly due to the raw structure of the payload

