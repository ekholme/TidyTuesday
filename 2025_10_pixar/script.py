import polars as pl
import seaborn as sns
import matplotlib.pyplot as plt


# get data
films = pl.read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/pixar_films.csv",
    null_values="NA",
)

public_reaction = pl.read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/public_response.csv",
    null_values="NA",
)

# check missing in each
films.null_count()
public_reaction.null_count()

# drop the missing film name -- it's fine to leave missing ratings for now
films_complete = films.drop_nulls()

# joining films
films_joined = films_complete.join(public_reaction, on="film")

# set change release_date to date type
films_joined = films_joined.with_columns(
    rd=pl.col("release_date").str.strptime(pl.Date, strict=False)
)

# bar plot with x as release_date, y as runtime
sns.barplot(
    x="rd",
    y="run_time",
    data=films_joined.to_pandas(),
)

plt.show()
# ok so this looks bad

# let's do release date by metacritic score
sns.scatterplot(
    x="rd",
    y="metacritic",
    data=films_joined.to_pandas(),
)

plt.show()

# ok, so what if we do title by metacritic score
films_sorted = films_joined.sort("metacritic", descending=True).drop_nulls("metacritic")

sns.barplot(
    y="film",
    x="metacritic",
    data=films_sorted.to_pandas(),
    color="steelblue",
)
plt.xlabel("Metacritic Score")
plt.ylabel("Film Title")
plt.title("Pixar Films by Metacritic Score")
plt.show()
