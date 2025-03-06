library(tidyverse)
library(S7)

# get data
raw_df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv")

# define a new class
Rescue <- new_class(
    "Rescue",
    properties = list(
        name = class_character,
        type = class_character,
        color = class_character,
        sex = class_character,
        dob = class_Date
    ),
    validator = function(self) {
        if (length(self@name) != 1) {
            "@name must be length 1"
        } else if (length(self@type) != 1) {
            "@type must be length 1"
        } else if (length(self@sex) != 1) {
            "@sex must be length 1"
        } else if (length(self@dob) != 1) {
            "@dob must be length 1"
        }
    }
)

# create a new Rescue
r1 <- Rescue(
    name = raw_df$animal_name[1],
    type = raw_df$animal_type[1],
    color = raw_df$primary_color[1],
    sex = raw_df$sex[1],
    dob = raw_df$dob[1]
)

# define a new method for Rescue
method(print, Rescue) <- function(x) {
    cat("Hi, I'm ", x@name, "! I'm a ", x@color, " ", x@sex, " ", x@type, ". I was born on ", as.character(x@dob), sep = "")
}

print(r1)

# define a bulk constructor
many_rescues <- function(x) {
    n <- nrow(x)
    out <- vector(mode = "list", length = n)

    for (i in seq_len(n)) {
        out[[i]] <- Rescue(
            name = x[["animal_name"]][i],
            type = x[["animal_type"]][i],
            color = x[["primary_color"]][i],
            sex = x[["sex"]][i],
            dob = x[["dob"]][i]
        )
    }

    out
}

use_df <- raw_df |>
    drop_na() |>
    head(n = 10)

rescues <- many_rescues(use_df)

rescues
