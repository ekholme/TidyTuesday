import polars as pl
import seaborn as sns
import matplotlib.pyplot as plt

# read in data
palmtrees = pl.read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv",
    encoding="latin1",
    null_values="NA",
)

# show column names
palmtrees.columns

# ok, what are the palm tribes
palmtrees["palm_tribe"].unique()

# let's count the data by tribe
n_by_tribe = palmtrees.group_by("palm_tribe").len(name="n").sort("n", descending=True)
sns.barplot(x="n", y="palm_tribe", data=n_by_tribe.to_pandas())
plt.show()

# ok, what if we just look at areceae palms
areceae_palms = palmtrees.filter(pl.col("palm_tribe") == "Areceae").drop_nulls(
    "max_stem_height_m"
)

# and say we want a histogram of max_stem_height_m
sns.histplot(data=areceae_palms.to_pandas(), x="max_stem_height_m", bins=20)
plt.show()

# ok, so let's take the 5 tribes with the most species
top_5_tribes = n_by_tribe.head(5)["palm_tribe"]

top5_df = palmtrees.filter(pl.col("palm_tribe").is_in(top_5_tribes)).drop_nulls(
    "max_stem_height_m"
)

# and let's do a faceted histogram

g = sns.FacetGrid(
    top5_df.to_pandas(), col="palm_tribe", col_wrap=2, sharex=True, sharey=False
)
g.map(sns.histplot, "max_stem_height_m", bins=20)
plt.show()
# ok, cool. this is super slow, though, and I'm not sure why
