#import the packages that needed
import pandas as pd
import numpy as np
import regex as re
import warnings
warnings.filterwarnings('ignore')

def split(x):
    
    pattern_parentheses = re.compile(r'\([^)]*\)')
    x = re.sub(pattern_parentheses, '', x)
    pattern_hn = re.compile(r'^\d+\s')
    x = re.sub(pattern_hn, '', x)
    pattern_hn = re.compile(r'^\d+R\s')
    x = re.sub(pattern_hn, '', x)

    return x

