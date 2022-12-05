#import the packages that needed
import pandas as pd
import numpy as np
import regex as re
import warnings
warnings.filterwarnings('ignore')

def street_direction(x):
    
    # The orientations in the addresses
    pattern = re.compile(r'\sN\s|\sNORTH\s')
    x = re.sub(pattern, " N ", x)
    pattern = re.compile(r'\sNORTH$')
    x = re.sub(pattern, " N", x)
    pattern = re.compile(r'^NORTH\s')
    x = re.sub(pattern, "N ", x)
    pattern = re.compile(r'\sS\s|\sSOUTH\s')
    x = re.sub(pattern, " S ", x)
    pattern = re.compile(r'\sSOUTH$')
    x = re.sub(pattern, " S", x)
    pattern = re.compile(r'^SOUTH\s')
    x = re.sub(pattern, "S ", x)
    pattern = re.compile(r'\sE\s|\sEAST\s')
    x = re.sub(pattern, " E ", x)
    pattern = re.compile(r'\sEAST$')
    x = re.sub(pattern, " E", x)
    pattern = re.compile(r'^EAST\s')
    x = re.sub(pattern, "E ", x)
    pattern = re.compile(r'\sW\s|\sWEST\s')
    x = re.sub(pattern, " W ", x)
    pattern = re.compile(r'\sWEST$')
    x = re.sub(pattern, " W", x)
    pattern = re.compile(r'^WEST\s')
    x = re.sub(pattern, "W ", x)

    return x

