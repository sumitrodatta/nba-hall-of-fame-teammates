library(tidyverse)
library(rvest)
library(janitor)
library(polite)

user_agent_for_bow=readline("Enter a name for the user agent: ")

bbref_bow=bow("https://www.basketball-reference.com/",
              user_agent = user_agent_for_bow,force=TRUE,delay = 10)

hof_prob_session=nod(bbref_bow,path="leaders/hof_prob.html")
hof_prob=scrape(hof_prob_session) %>% html_nodes(css="#tot") %>% 
  html_table() %>% .[[1]] %>% clean_names()

hof_prob_hyperlinks = scrape(hof_prob_session) %>%
  html_nodes(css="#tot") %>%
  html_nodes("td") %>% 
  html_nodes("a") %>% 
  html_attr("href")

hof_prob_w_slugs=hof_prob %>% add_column(hof_prob_hyperlinks)

hof_prob_over_0.7=hof_prob_w_slugs %>% filter(str_detect(player,"\\*",negate=TRUE)) %>%
  filter(ho_f_prob>=0.7)

hof=nod(bbref_bow,path="awards/hof.html")
hall_of_famers=scrape(hof) %>% html_nodes(css="#hof") %>% 
  html_table() %>% .[[1]] %>% row_to_names(row_number=1) %>%
  clean_names()

hof_hyperlinks=scrape(hof) %>%
  html_nodes(css="#hof") %>%
  html_nodes("td") %>% 
  html_nodes("a") %>% 
  html_attr("href")

player_hyperlinks=tibble(hof_hyperlinks) %>% 
  filter(str_starts(hof_hyperlinks,"/players"))

hall_of_fame_players= hall_of_famers %>% filter(str_detect(name,"Player")) %>%
  select(year:ws_48) %>% 
  mutate(across(.cols=g:ws_48,.fns=~na_if(.,""))) %>%
  mutate(name=str_trim(word(name,start=1,sep="Player"))) %>% 
  add_column(player_hyperlinks) %>% 
  add_row(hof_prob_over_0.7 %>% 
            mutate(category="Player") %>% 
            select(name=player,hof_hyperlinks=hof_prob_hyperlinks,category)) %>%
  mutate(player_slug=str_remove(word(hof_hyperlinks,start=4,sep="/"),".html")) %>%
  filter(category != "Coach") %>%
  distinct(player_slug,.keep_all=TRUE) %>%
  mutate(name=case_when(str_detect(name,"Cooper")~"Chuck Cooper",
                        str_detect(name,"Petrovic")~"Dražen Petrović",
                        str_detect(name,"Ginobili")~"Manu Ginóbili",
                        str_detect(name,"Clifton")~"Nat Clifton",
                        str_detect(name,"Marciulionis")~"Šarūnas Marčiulionis",
                        str_detect(name,"Kukoc")~"Toni Kukoč",
                        TRUE~name))

slugs_try=hall_of_fame_players %>% pull(player_slug)

check_hof_teammates<-function(player_slug){
  sesh=nod(bbref_bow,path=paste0("friv/teammates_and_opponents.fcgi?pid=",player_slug,"&type=t"))
  teammates=scrape(sesh) %>% html_nodes(css="#teammates-and-opponents") %>% 
    html_table() %>% .[[1]] %>% row_to_names(1) %>% clean_names() %>% 
    select(teammate:w_percent) %>% 
    mutate(teammate=str_remove(teammate,"\\*")) %>% 
    filter(g!="Overall"&teammate!="Teammate")
  mate_links=scrape(sesh) %>% html_nodes(css="#teammates-and-opponents") %>%
    html_nodes("td") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  hof_teammates=inner_join(teammates %>% add_column(mate_links),
                           hall_of_fame_players,by=c("mate_links"="hof_hyperlinks")) %>% 
    add_column(source=player_slug) %>% select(source,teammate:w_percent) %>% 
    mutate(across(g.x:w_percent,as.numeric))
  return(hof_teammates)
}

check_hof_teammates("horryro01")

check_hof_teammates("embiijo01")

check_hof_teammates("doncilu01")

hof_w_hof_teammates=tibble()
for (x in slugs_try){
  hof_teammates_result=check_hof_teammates(x)
  hof_w_hof_teammates<<-hof_w_hof_teammates %>% rbind(hof_teammates_result)
  print(x)
}

final_hof_w_hof_mates=inner_join(hof_w_hof_teammates,hall_of_fame_players,
                                 by=c("source"="player_slug")) %>% 
  select(name,teammate:w_percent) %>% arrange(name,desc(g.x),desc(w_percent))

pci=read_csv("Data/Player Career Info.csv")

totals=read_csv("Data/Player Totals.csv") %>% filter(tm!="TOT")

hof_and_mates_summary=final_hof_w_hof_mates %>% group_by(name) %>% 
  summarize(num_hof_teammates=n(),g_w_hof_teammates=sum(g.x)) %>% ungroup() %>%
  right_join(hall_of_fame_players %>% select(name)) %>%
  replace_na(list(num_hof_teammates=0,g_w_hof_teammates=0)) %>%
  left_join(.,pci %>% distinct(player,.keep_all = TRUE),by=c("name"="player")) %>% 
  mutate(avg_hof_mates_season=num_hof_teammates/num_seasons) %>%
  select(-c(birth_year,first_seas)) %>%
  left_join(.,totals %>% group_by(player_id,player) %>% 
              summarize(tot_g=sum(g)) %>% ungroup(),
            by=join_by(player_id==player_id,name==player)) %>% 
  mutate(avg_hof_mates_season=num_hof_teammates/num_seasons,
         avg_hof_mates_g=g_w_hof_teammates/tot_g) %>%
  relocate(player_id,name,hof,last_seas,.before=everything()) %>%
  relocate(g_w_hof_teammates,.after=avg_hof_mates_season)

write_csv(final_hof_w_hof_mates,"HOFers with HOF Teammates.csv")

write_csv(hof_and_mates_summary,"HOFer Teammate Summary.csv")
