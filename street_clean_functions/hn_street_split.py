#!/usr/bin/env python
# coding: utf-8

# In[ ]:


#import the packages that needed
import pandas as pd
import numpy as np
import regex as re
import warnings
warnings.filterwarnings('ignore')


# In[ ]:


def split(x):
    
    pattern_parentheses = re.compile(r'\([^)]*\)')
    x = re.sub(pattern_parentheses, '', x)
    pattern_hn = re.compile(r'^\d+\s')
    x = re.sub(pattern_hn, '', x)
    pattern_hn = re.compile(r'^\d+R\s')
    x = re.sub(pattern_hn, '', x)
#     pattern = re.compile(r'\d+')
#     length = re.findall(pattern, x)
    
#     if len(length) == 1:
#         return x
#     else:
#         pattern_first = re.compile(r'^\d*\s')
#         x = re.sub(pattern_first, "", x)
#         return x
    return x

