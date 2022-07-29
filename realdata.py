import pyreadr
import pandas as pd
import jax
import pyro
import numpyro

data = pyreadr.read_r("dat.rds")
filter_col = [col for col in df if data.colname
.startswith('foo')]