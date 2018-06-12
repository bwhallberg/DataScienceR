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

#
#  All game potential
#
tmp <- eligible %>%
  mutate(pas = case_when (
    debut > '1933-01-01'       ~ timein,
    final_game <= '1933-01-01' ~ 0, 
    TRUE                       ~ timein - floor(difftime('1933-01-01', debut, units="days")/365)
  )
  ) %>%
  mutate(pas = as.integer(pas)) %>%
  mutate(pcas = allstar/pas)

tmp %>% 
  select(player_id, allstar, timein, pas, pcas) %>%
  mutate(itimein = as.integer(timein)) %>%
  mutate(ipas = as.integer(pas))

#
#  What is my current working directory
#
getwd()
setwd("..")  # this moves the current working directory
#  In Rstudio, Session > Set Working Directory > can be used as well

#
#  Clean up the environment
#
rm(list = ls(all = TRUE))


hof <- eligible %>%
  filter(inducted == 'Y') %>%
  filter(final_game > '1936-01-01') %>%
  select(player_id, inducted, allstar, timein, final_game)

ggplot(hof, aes(x=player_id, y=allstar)) +
  geom_point()

ggplot(hof, aes(x=allstar, fill=factor(timein), col=factor(timein))) +
  geom_histogram()

ggplot(hof, aes(x=allstar, fill=factor(timein))) +
  geom_histogram()

ggplot(hof, aes(x=timein)) +
  geom_histogram()