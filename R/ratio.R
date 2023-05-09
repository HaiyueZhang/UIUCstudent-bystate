calc_ratio = function(data) {
  data |>
    mutate(total = Undergraduate + Professional + Graduate) |>
    mutate(Undergraduate_ratio = round(Undergraduate/total, digits = 2)) |>
    mutate(Professional_ratio = round(Professional/total, digits = 2)) |>
    mutate(Graduate_ratio = round(Graduate/total, digits = 2)) |>
    select(-total)
}