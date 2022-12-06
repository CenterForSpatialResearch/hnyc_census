#import the packages that needed
import pandas as pd
import numpy as np
import regex as re
from special_cases import special_character
from street_direction import street_direction
from street_name import street_name
from street_number_name import street_number_name
from hn_street_split import split
from street_type import street_type

import warnings
warnings.filterwarnings('ignore')

def clean(df, column):  
        
#         Clean the street name and return the final clean addresses
    df[column] = df[column].apply(lambda x: str(x).upper())
    df['street_direction_clean'] = df[column].apply(lambda x: street_direction(str(x)))
    df['street_type_clean'] = df['street_direction_clean'].apply(lambda x: street_type(x))
    
    df['street_without_hn'] = df['street_type_clean'].apply(lambda x: split(str(x)))

    df['street_number_name_clean'] = df['street_without_hn'].apply(lambda x: street_number_name(x))
    df['street_name_clean'] = df['street_number_name_clean'].apply(lambda x: street_name(x))
    df['final_clean_address'] = df['street_name_clean'].apply(lambda x: special_character(x))
    
    df['final_clean_address'] = np.where(df['final_clean_address'] == 'ST', df['street_type_clean'], df['final_clean_address'])
    
#         Extract different components of the addresses, e.g direction: W/E/S/N; street type: AVE/PL/ST/CIR, etc.
    pattern_street_direction = r'(?<=\s)[N|W|S|E]\s|^[N|W|S|E]\s|(?<=\s)[N|W|S|E]\d+'
    df['street_direction'] = df['final_clean_address'].apply(lambda x: ' '.join(re.findall(pattern_street_direction, x)))

    pattern_street_type = r'(?<=\s)ST$|(?<=\s)DR$|(?<=\s)CIR$|(?<=\s)AVE$|(?<=\s)CT$|(?<=\s)BLVD$|(?<=\s)ALY$|(?<=\s)PLZ$|(?<=\s)PARK$|(?<=\s)PKWY$|(?<=\s)APPROACH$|(?<=\s)TER$|(?<=\s)PL$|(?<=\s)LN$|(?<=\s)BRG$|(?<=\s)HL$|(?<=\s)HTS$|(?<=\s)SLIP$|(?<=\s)ROW$|(?<=\s)SQ$'
    df['street_type'] = df['final_clean_address'].apply(lambda x: ' '.join(re.findall(pattern_street_type, x)))

    df['street_name1'] = df.apply(lambda x: x["final_clean_address"].replace(x["street_type"], "").strip(), axis=1)
    df['street_name'] = df.apply(lambda x: x["street_name1"].replace(x["street_direction"], "").strip(), axis=1)
    df.drop('street_name1', axis=1, inplace = True)

    return df[[column, 'street_direction', 'street_name', 'street_type', 'final_clean_address']]
