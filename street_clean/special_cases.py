import pandas as pd
import numpy as np
import regex as re
import warnings
warnings.filterwarnings('ignore')

def special_character(x: str) -> str:
    
    """
    Introduction:
    -------------
    This function can help users to deal with some special cases in the street addresses. For example, correct the order of the street direction and street number.
    
    Inputs:
    -------------
    'x': str. The street addresses in your dataset.

    Outputs:
    -------------
    'x': str. Clean street addresses.

    Example:
    -------------
    >>> e = special_character('18 W ST')
    >>> e
    'W 18 ST'
    """
    
    pattern = re.compile(r'((\w+|\d+)\s\s(\w+|\d+))|((\w+|\d+)\s\s\s(\w+|\d+))')
    x = re.sub(pattern, r"\1 \2", x)

    # deal with the space between numbers - e.g. EAST 11 8 STREET, there is a space between 11 & 8
    pattern = re.compile(r'(\d+)\s(\d+)')
    x = re.sub(pattern, r"\1\2", x)

    # deal with special characters (\;';*'.')
    pattern = re.compile(r"\s(TO)\s") 
    x = re.sub(pattern, "-", x)
    pattern = re.compile(r"-") 
    x = re.sub(pattern, " ", x)
    pattern = re.compile(r"&") 
    x = re.sub(pattern, "AND", x)
    pattern = re.compile(r"[^\w\s]") 
    x = re.sub(pattern, "", x)

    # change the order of the address, e.g: 4 W to W 4 & AVE after A-Z and Digits
    pattern = re.compile(r'(\d+)\s(ST)\s([W|N|S|E])')
    x = re.sub(pattern, r'\3 \1 \2', x)
    pattern = re.compile(r'(CENTRAL PARK)\s([W|N|S|E])\s(AVE)')
    x = re.sub(pattern, r'\2 \1 \3', x)
    pattern = re.compile(r'(WASHINGTON SQ)\s([W|N|S|E])')
    x = re.sub(pattern, r'\2 \1', x)
    pattern = re.compile(r'(MORNINGSIDE AVE)\s([W|N|S|E])')
    x = re.sub(pattern, r'\2 \1', x)
    pattern = re.compile(r'(\w+|\d+)\s(\bN\b|\bW\b|\bS\b|\bE\b)')
    x = re.sub(pattern, r'\2 \1', x)
    pattern = re.compile(r'(AVE)\s(\d+)\s(\d+)')
    x = re.sub(pattern, r'\2-\3 \1', x) 
    pattern = re.compile(r'(AVE)\s(\d+)')
    x = re.sub(pattern, r'\2 \1', x) 
    pattern = re.compile(r'(AVE)\s([A-Z]+)')
    x = re.sub(pattern, r'\2 \1', x)  

    # When there is an 'AND' after hundred, I should convert it
    pattern = re.compile(r'(\d+)AND(\d+)')
    x = re.sub(pattern, r"\1\2", x)

    try:
        pattern = re.compile(r'(\d+)AND (\d+)')
        length = re.findall(pattern, x)
        length_list = list(length[0])

        if len(length_list[1]) == 1:
            x = re.sub(pattern, length_list[0] + '0' + length_list[1],  x)
        else:
            x = re.sub(pattern, length_list[0] + length_list[1],  x)
    except:
        x = x

    # when the address have no space between number and street type
    pattern = re.compile(r"([W|N|S|E])(\d+)")
    x = re.sub(pattern,r"\1 \2", x).strip()
    pattern = re.compile(r"(\d+)(ST|AVE|DR|CIR|CT|BLVD|ALY|PLZ|PARKS|PKWY|APPROACH|TER|PL|LN|BRG|HL|HTS|SLIP|ROW|SQ)")
    x = re.sub(pattern,r"\1 \2", x).strip()

    return x

