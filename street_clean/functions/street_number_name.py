#import the packages that needed
import pandas as pd
import numpy as np
import regex as re
import warnings
warnings.filterwarnings('ignore')

def street_number_name(x: str) -> str:
    """
    Introduction:
    -------------
    This function can help users to clean the street numbers in the street addresses. Specifically, turn the some desciption numbers to real numbers and extract the street numbers.
    
    Inputs:
    -------------
    'x': str. The street addresses in your dataset.

    Outputs:
    -------------
    'x': str. Street addresses with clean street numbers.

    Example:
    -------------
    >>> e = street_number_name('E FOURTEENTH STREET')
    >>> e
    'E 14 STREET'
    """
    
    pattern = re.compile(r"(\d+)(\s)(ND|RD|TH)")
    x = re.sub(pattern, r"\1\3", x)
    pattern = re.compile(r"(\d+)(ST|ND|RD|TH|D)")
    x = re.sub(pattern, r"\1", x)
    pattern = re.compile(r"ELEVENTH\s") 
    x = re.sub(pattern, "11", x)
    pattern = re.compile(r"TWELFTH\s") 
    x = re.sub(pattern, "12", x)    
    pattern = re.compile(r"THIRTEENTH\s") 
    x = re.sub(pattern, "13", x)     
    pattern = re.compile(r"FORTEENTH\s|FOURTHENTH\s|\sFOURTEENTH$") 
    x = re.sub(pattern, "14", x)    
    pattern = re.compile(r"FIFTEENTH\s") 
    x = re.sub(pattern, "15", x)
    pattern = re.compile(r"SIXTEENTH\s") 
    x = re.sub(pattern, "16", x)
    pattern = re.compile(r"SEVENTEENTH\s") 
    x = re.sub(pattern, "17", x)
    pattern = re.compile(r"EIGHTEENTH\s|EIGHTEENTH$") 
    x = re.sub(pattern, "18", x)
    pattern = re.compile(r"NINETEENTH\s") 
    x = re.sub(pattern, "19", x)
    pattern = re.compile(r"TWENTIETH\s|TWENTIEFTH\s") 
    x = re.sub(pattern, "20", x)
    pattern = re.compile(r"THIRTIETH\s|THIRTIEFTH\s") 
    x = re.sub(pattern, "30", x)
    pattern = re.compile(r"FORTIETH\s|FOURTIETH\s") 
    x = re.sub(pattern, "40", x)
    pattern = re.compile(r"FIFTIETH\s") 
    x = re.sub(pattern, "50", x)
    pattern = re.compile(r"SIXTIETH\s") 
    x = re.sub(pattern, "60", x)
    pattern = re.compile(r"SEVENTIETH\s") 
    x = re.sub(pattern, "70", x)    
    pattern = re.compile(r"EIGHTIETH\s|EIGHTETH\s") 
    x = re.sub(pattern, "80", x) 
    pattern = re.compile(r"NINETIETH\s|NINTIETH\s") 
    x = re.sub(pattern, "90", x) 
    pattern = re.compile(r"FRIST\s|FRST\s|FIRST\s|ONE HUNDRED\s|ONE HUNRED\s|ONEHUNDRED\s|HUNDRED\s|HUDRED\s|HUNDED\s|ONE\s") 
    x = re.sub(pattern, "1", x) 
    pattern = re.compile(r"TWO HUNDRED\s|TWOHUNDRED\s|TWENTY\s|TWENTI\s|TENTI\s|SECOND\s|SECORD\s|SCOND\s|TWO\s") 
    x = re.sub(pattern, "2", x)
    pattern = re.compile(r"\s(THIRTY)\s|THIRTY\s|THIRTHY\s|THIRTEY\s|TIRTY\s|TRITHY\s|THRID\s|THIRD\s|TIRD\s|TRIH\s|THIR$|THREE\s") 
    x = re.sub(pattern, "3", x)    
    pattern = re.compile(r"FORTY\s|FORTH\s|FORSETH\s|FOURTY\s|FOURTHY\s|FOURTH\s|FOURT\s|FRTY\s|FROTH\s|FROUTH\s|FOUR\s") 
    x = re.sub(pattern, "4", x)
    pattern = re.compile(r"FIFTY\s|FIFTHE\s|FIFTHY\s|FIFTH\s|FIFTEY\s|FIFT\s|FIFT|FITY\s|FIFETH\s|FIFFTH\s|FIVE\s") 
    x = re.sub(pattern, "5", x)
    pattern = re.compile(r"SIXTY\s|SXTY\s|SIXY\s|SIXTHY\s|SIXTEY\s|SIXTH\s|SXTH\s|SITH\s|SIHXT\s|SIX\s") 
    x = re.sub(pattern, "6", x)
    pattern = re.compile(r"SEVENTEY\s|SVENTY\s|SEVENTI\s|SEVENTH\s|SEVENTY-|SEVENTY\s|SVEN\s|SVENTH\s|SEVENH\s|SEVENT\s|SEVEN\s") 
    x = re.sub(pattern, "7", x) 
    pattern = re.compile(r"EIGHTY\s|EIGHTEH\s|EIGHTEY\s|EIGHTE\s|EIGHTH\s|EITH\s|EIGHT\s|EIGHTTH\s|EIGTH\s|FIGHT\s") 
    x = re.sub(pattern, "8", x)       
    pattern = re.compile(r"UNITY\s|NINETY\s|NINETY-|NINETEY\s|NINETIETH\s|NINTH\s|NINTH$|NINTY\s") 
    x = re.sub(pattern, "9", x) 
    pattern = re.compile(r"TENTH\s") 
    x = re.sub(pattern, "10", x)     

    return x

