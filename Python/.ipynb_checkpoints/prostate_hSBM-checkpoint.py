#!/usr/bin/env python
# coding: utf-8

# # Implementing Network approach to topic modelling
# ## Gerlach

# In[1]:


import matplotlib


# In[2]:


from platform import python_version

print(python_version())


# In[44]:


# Gerlach
#%load_ext autoreload
#%autoreload 2

import os
import pylab as plt
#%matplotlib inline  

from sbmtm import sbmtm
import graph_tool.all as gt


# In[45]:


import pandas as pd
import numpy as np
import re
from itertools import chain


# In[46]:


data = pd.read_csv("../data/Prostate/clean_posts.csv")


# In[47]:


#texts = data["Text"].values.tolist()
#titles = data["Day"].values.tolist()

#texts = data["word"].values.tolist()
#titles = data["ID"].values.tolist()

texts = data["Content"].values.tolist()
titles = data["Post_ID"].values.tolist()


# In[48]:


texts = [c.split() for c in texts]


# In[49]:


i_doc = 0
texts


# In[50]:


for i in range(10):
    print(i)
    model = sbmtm()

    ## we have to create the word-document network from the corpus
    model.make_graph(texts,documents=titles)

    ## we can also skip the previous step by saving/loading a graph
    # model.save_graph(filename = 'graph.xml.gz')
    # model.load_graph(filename = 'graph.xml.gz')

    ## fit the model
    #gt.seed_rng(32) ## seed for graph-tool's random number generator --> same results
    model.fit()
    topics = model.topics(l=0,n=10)
    
    if len(topics) > 1:
        break


# In[51]:


#model.plot(nedges = 1000)


# In[52]:


topics = model.topics(l=0,n=20)


# In[53]:


#topics


# In[54]:


clusters = model.clusters(l=0,n=99999)


# In[55]:


#clusters


# In[56]:


group_results0 = model.get_groups(l=0)
group_results1 = model.get_groups(l=1)
group_results2 = model.get_groups(l=2)
group_results3 = model.get_groups(l=3)

## group-membership distributions
# group membership of each word-node P(t_w | w)
p_tw_w0 = group_results0['p_tw_w']
p_tw_w1 = group_results1['p_tw_w']
p_tw_w2 = group_results2['p_tw_w']
p_tw_w3 = group_results3['p_tw_w']

# group membership of each doc-node P(t_d | d)
p_td_d0 = group_results0['p_td_d']
p_td_d1 = group_results1['p_td_d']
p_td_d2 = group_results2['p_td_d']
p_td_d3 = group_results3['p_td_d']

## topic-distribution for words P(w | t_w)
p_w_tw0 = group_results0['p_w_tw']
p_w_tw1 = group_results1['p_w_tw']
p_w_tw2 = group_results2['p_w_tw']
p_w_tw3 = group_results3['p_w_tw']

## Mixture of word-groups into documetns P(t_w | d)
p_tw_d0 = group_results0['p_tw_d']
p_tw_d1 = group_results1['p_tw_d']
p_tw_d2 = group_results2['p_tw_d']
p_tw_d3 = group_results3['p_tw_d']


# In[57]:


pd.DataFrame.to_csv(pd.DataFrame(p_tw_w0), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_tw_w0.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_tw_w1), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_tw_w1.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_tw_w2), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_tw_w2.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_tw_w3), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_tw_w3.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_td_d0), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_td_d0.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_td_d1), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_td_d1.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_td_d2), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_td_d2.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_td_d3), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_td_d3.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_w_tw0), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_w_tw0.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_w_tw1), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_w_tw1.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_w_tw2), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_w_tw2.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_w_tw3), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_w_tw3.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_tw_d0), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_tw_d0.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_tw_d1), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_tw_d1.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_tw_d2), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_tw_d2.csv")
pd.DataFrame.to_csv(pd.DataFrame(p_tw_d3), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_p_tw_d3.csv")


# In[58]:


pd.DataFrame.to_csv(pd.DataFrame(model.words), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_words_all.csv")
pd.DataFrame.to_csv(pd.DataFrame(model.documents), "/Users/a1670295/Dropbox/PhD/Prostate/data/Prostate/Topic_Model/prostate_documents_all.csv")


# In[ ]:




