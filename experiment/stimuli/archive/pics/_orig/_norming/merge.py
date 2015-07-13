#!/usr/bin/env python
import pandas as pd

c = pd.read_csv('complexity.csv')
d = pd.read_csv('diagnostic.csv')
f = pd.read_csv('familiarity.csv')
i = pd.read_csv('imagery.csv')
r = pd.read_csv('ratings.csv')

cd = pd.merge(c, d, how='outer')
cdf = pd.merge(cd, f, how='outer')
cdfi = pd.merge(cdf, i, how='outer')
cdfir = pd.merge(cdfi, r, how='outer')

cdfir.to_csv('all.csv', index=False)
