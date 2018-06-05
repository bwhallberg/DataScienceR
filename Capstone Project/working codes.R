sumhof <- hof %>%
  filter(category =="Player") %>%
  select(player_id, inducted) %>%
  filter(inducted == "Y")

cnthof <- hof %>%
  select(player_id, inducted, yearid, category) %>%
  group_by(yearid) %>%
  filter(inducted == "Y") %>%
  count(yearid)

hof %>%
  select(player_id, inducted, yearid, category) %>%
  group_by(category) %>%
  filter(inducted == "Y") %>%
  count(category)

sumallstar <- all_star %>%
  select(player_id, league_id, year) %>%
  group_by(player_id) %>%
  count(player_id)

player %>%
  select(player_id, debut, final_game) %>%
  mutate(timein = floor(difftime(final_game, debut, units="days")/365))

player %>%
  mutate(timein = floor(difftime(final_game, debut, units="days")/365)) %>% 
  mutate(Allstar = 0) %>%
  mutate(inHOF = "N") %>%
  filter(timein > 10) %>%
  filter(final_game <= "2010-01-01")

fullplay <- player %>%
  select(player_id, debut, final_game) %>%
  mutate(timein = floor(difftime(final_game, debut, units="days")/365)) %>% 
  mutate(Allstar = floor(0)) %>%
  mutate(inHOF = "N") %>%
  filter(timein > 10) %>%
  filter(final_game <= "2011-01-01")

tmp <- full_join(player, sumhof, by = "player_id")
tmp1 <- full_join(tmp, sumallstar, by = "player_id")

sumhit <- hit %>%
  group_by(player_id) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(ba = h/ab) %>%
  select(-year, -stint)

sumhitf <- hit %>%
  group_by(player_id) %>%
  summarise_if(is.numeric, sum, na.rm = FALSE) %>%
  mutate(ba = h/ab) %>%
  select(-year, -stint)

sumpitch <- pitch %>%
  group_by(player_id) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  select(-year, -stint, -baopp, -era)

tmp <- player

sumf <- fullplay %>% filter(inducted == 'Y') %>% select (player_id, inducted, debut, final_game, ab, w, n)
sume <- eligible %>% filter(inducted == 'Y') %>% select (player_id, inducted, debut, final_game, ab, w, n)
rmiss <- setdiff(sumf, sume)
