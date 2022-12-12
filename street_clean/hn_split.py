#import the packages that needed
import pandas as pd
import numpy as np
import regex as re
import warnings
warnings.filterwarnings('ignore')

def split(x: str) -> str:
    """
    Introduction:
    -------------
    This function can help users to seperate the house number and the street addresses and return the street addresses.
    
    Inputs:
    -------------
    'x': str. The street addresses in your dataset.

    Outputs:
    -------------
    'x': str. Street addresses without house number.

    Example:
    -------------
    >>> e = split('609 EAST 14TH STREET')
    >>> e
    'EAST 14TH STREET'
    """
    
    pattern_parentheses = re.compile(r'\([^)]*\)')
    x = re.sub(pattern_parentheses, '', x)
    pattern_hn = re.compile(r'^\d+\s')
    x = re.sub(pattern_hn, '', x)
    pattern_hn = re.compile(r'^\d+R\s')
    x = re.sub(pattern_hn, '', x)

    return x

