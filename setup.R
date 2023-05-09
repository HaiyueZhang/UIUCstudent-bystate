library(tidyverse)

url = "https://raw.githubusercontent.com/wadefagen/datasets/master/students-by-state/uiuc-students-by-state.csv"
student_bystate = read_csv(url)

student_bystate = student_bystate |>
  select(-Total) |>
  rename(Undergraduate = "Undergrad", Graduate = "Grad")

write_csv(x = student_bystate, file = "data/student_bystate.csv")

student_bystate_degree = student_bystate |>
  rename(Undergraduate = "Undergrad", Graduate = "Grad") |>
  select(-Total) |>
  pivot_longer(Undergraduate:Graduate, names_to = "Degree", values_to = "Count")
  
write_csv(x = student_bystate_degree, file = "data/student_bystate_degree.csv")





  