#' Cabinet portfolio proportionality
#' 
#' A function used to calculate the proportionality of portfolio allocation using the Saint Lague method with first divider being 1.
#' 
#' @param seats Vector containing the seats each party has in cabinet
#' @param portfolios Vector containing the total amount of portfolios in the cabinet
#' @param party Vector identifying the party (either id or party name)
#' 
#' @return Returns a vector of perfect proportionality portfolio allocation estimates to be compared with acctual portfolio allocation
#' 
#' 
#' 
#' @examples 
#' norPortf <- ministersNor::norPortf
#' 
#' nor_stlague<-norPortf %>%
#'  group_by(cabinet_name) %>%
#'  mutate(stlague_posts = st.lague(seats, total_cab_posts[1], party_name))
#'  
#' @export
#' 

st.lague<-function(seats, n.posts, n.parties){
  
  posts_now = rep(0, length(n.parties)) # start with 0 placed posts
  
  
  seats_now = seats # seats in parliament
  
  dividers = cbind(seq(1, 100, 1), seq(3, 202, 2))
  dividers[1:2,1]
  while(n.posts > sum(posts_now)){
    seats_now = seats # it is important to not write over the seats vector, as it is reset after each loop 
      for(i in 1:nrow(dividers)){
        seats_now = ifelse(posts_now==dividers[i,1], seats_now/(dividers[i,2]), seats_now)
      }
    posts_now = ifelse(seats_now==max(seats_now), posts_now+1, posts_now)
  }
  
  posts_now
}