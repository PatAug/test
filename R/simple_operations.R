class(5)

"5" + 5

class(5L)

class(5L + 5)

class(5.55)

class("napis")

"napis" + "napis"

paste("napis_1", "napis_2", sep = "")
paste0("napis_1", "napis_2")
paste0

class(TRUE)
class(FALSE)
class(T)
class(F)
T <- 5

T

class(Sys.Date())

as.Date("2017-01-01") + months(5)
as.Date("2017-01-01") + lubridate::years(5)

class(NA)
class(NULL)

library(dplyr)

kp_tib <- PogromcyDanych::koty_ptaki %>%
  as_tibble()

data(koty_ptaki, package = "PogromcyDanych")
kp_tib <- koty_ptaki %>%
  as_tibble()

# filter
# select
# mutate
# group_by
# summarise
# arrange
# distinct

filter(kp_tib, druzyna == "Kot", !(habitat %in% c("Afryka", "Ameryka")),
       predkosc > 70)

select(kp_tib, waga, dlugosc, predkosc, zywotnosc)
select(kp_tib, -waga, -dlugosc, -predkosc, -zywotnosc)
select(kp_tib, -c(waga, dlugosc, predkosc, zywotnosc))

select(iris_tbl, matches("Sepal"))

mutate(kp_tib, waga_do_dlugosci = waga / dlugosc, suma_wagi_i_dlogosci = round(waga + dlugosc))
transmute(kp_tib, waga_do_dlugosci = waga / dlugosc, suma_wagi_i_dlogosci = round(waga + dlugosc))

grouped_kp <- group_by(kp_tib, druzyna, habitat)
mutate(grouped_kp, srednia = mean(predkosc))
summarise(grouped_kp, srednia = mean(predkosc))
summarise(grouped_kp, srednia_predkosc = mean(predkosc), max_waga = max(waga))

grouped_kp <- group_by(kp_tib, druzyna)
arrange(grouped_kp, desc(druzyna), desc(zywotnosc))

distinct(kp_tib, druzyna, habitat)
select(kp_tib, qwerty)
distinct(kp_tib, duzyna)

summarise(
  group_by(
    filter(
      select(kp_tib, -gatunek),
      waga < 100
    ),
    habitat
  ),
  sr_dl = mean(dlugosc)
)

kp_tib %>%
  select(-gatunek) %>%
  filter(waga < 100) %>%
  group_by(habitat) %>%
  summarise(sr_del = mean(dlugosc))

# funkcja(x, y) is like x %>% funkcja(y)

fun1 <- function(vec) {
  is.integer(vec) | is.factor(vec)
}

select(kp_tib, gatunek, habitat, druzyna)
select_if(kp_tib, is.factor)
select_if(kp_tib, is.numeric)
select_if(kp_tib, fun1)
select_all(kp_tib)
kp_tib
select(kp_tib, druzyna, gatunek, everything())

filter(kp_tib, waga < 1, dlugosc < 1)

filter_at(kp_tib, vars(waga, dlugosc), all_vars(. < 1))
filter_at(kp_tib, vars(waga, dlugosc), any_vars(. < 1))

filter_if(kp_tib, is.numeric, any_vars(. < 1))
filter_if(kp_tib, is.factor, any_vars(. == "Jerzyk"))

filter_all(kp_tib, any_vars(. < 1))

mutate_if(kp_tib, is.numeric, round)

mutate_at(kp_tib, vars(waga, dlugosc), funs(round))
rename(kp_tib, dlugowiecznosc = zywotnosc)
rename_all(kp_tib, toupper)

kp_tib %>%
  group_by(druzyna) %>%
  summarise_if(is.numeric, funs(max, mean, min))

kp_tib %>%
  group_by(druzyna) %>%
  summarise_at(vars(waga, dlugosc), funs(max, mean, min))

kp_tib %>%
  group_by_if(is.factor)

kp_tib %>%
  group_by_at(vars(habitat, druzyna))

kp_tib %>%
  group_by(habitat, druzyna)

xxx <- tibble(a = c(1, 2, NA, 3),
              b = c(4, NA, 6, 7))

xxx %>%
  summarise_at(vars(a, b), funs(max, min), na.rm = TRUE)

summarised_by_duzyna <- kp_tib %>%
  group_by(druzyna) %>%
  summarise_at(vars(waga), funs(max, mean, min))

summarised_by_duzyna <- kp_tib %>%
  group_by(druzyna) %>%
  summarise_at(vars(waga, dlugosc), funs(max))

summarised_by_duzyna <- kp_tib %>%
  group_by(druzyna) %>%
  summarise_at(vars(waga, dlugosc), funs(max, mean, min))

left_join(kp_tib, summarised_by_duzyna, by = "druzyna")

aaa <- kp_tib %>%
  select(gatunek, habitat) %>%
  mutate(habitat = factor("Azja"))
bbb <- kp_tib %>%
  select(gatunek, habitat) %>%
  mutate(habitat = factor("Afryka")) %>%
  rename(gtnk = gatunek)
left_join(aaa, bbb) %>%
  mutate(habitat = as.factor(habitat))
