#import the packages that needed
import pandas as pd
import numpy as np
import regex as re
from special_cases import special_case
from street_direction import street_direction
from street_name import street_name
from street_number_name import street_number_name
from hn_split import split
from street_type import street_type

import warnings
warnings.filterwarnings('ignore')


def clean(df: pd.DataFrame, column1: str, column2: str) -> pd.DataFrame: 
    
    """
    Introduction:
    -------------
    This function is used to apply different parts of functions to clean the raw street addresses. It will return one dataframe containing different parts of the street addresses, for example, the raw street address, house number, street direction, street name, street type and final clean street address.
    
    Inputs:
    -------------
    'df': pd.DataFrame. The dataset you import and try to clean.
    'column1': str. It means the column name in your dataset, expected to be house number if there is a house number column in your dataset.
    'column2': str. It means the column name in your dataset. This is the street address column which you want to clean.

    Outputs:
    -------------
    df: pd.DataFrame. One dataframe with clean street addresses and different parts of street addresses.

    Example:
    -------------
    >>> e = clean(df, 'house number', 'Street Address 2')
    >>> e
        raw_street_address house_number street_direction street_name street_type final_clean_address
    0        38 STREET                                        38          ST            38 ST
    1    EAST 107TH STREET                      E             107         ST           E 107 ST
    2       81ST STREET        337                            81          ST            81 ST
    3    EAST 117TH STREET                      E             117         ST           E 117 ST
    4     WEST 62ND STREET                      W             62          ST            W 62 ST
    
    """
    
    if column1 in df:
#         Clean the street name and return the final clean addresses
        df[column2] = df[column2].apply(lambda x: str(x).upper())
        df['street_direction_clean'] = df[column2].apply(lambda x: street_direction(str(x)))
        df['street_type_clean'] = df['street_direction_clean'].apply(lambda x: street_type(x))

        df['street_without_hn'] = df['street_type_clean'].apply(lambda x: split(str(x)))

        df['street_number_name_clean'] = df['street_without_hn'].apply(lambda x: street_number_name(x))
        df['street_name_clean'] = df['street_number_name_clean'].apply(lambda x: street_name(x))
        df['final_clean_address'] = df['street_name_clean'].apply(lambda x: special_case(x))

        df['final_clean_address'] = np.where(df['final_clean_address'] == 'ST', df['street_type_clean'], df['final_clean_address'])

    #         Extract different components of the addresses, e.g direction: W/E/S/N; street type: AVE/PL/ST/CIR, etc.
        pattern_street_direction = r'(?<=\s)[N|W|S|E]\s|^[N|W|S|E]\s|(?<=\s)[N|W|S|E]\d+'
        df['street_direction'] = df['final_clean_address'].apply(lambda x: ' '.join(re.findall(pattern_street_direction, x)))

        pattern_street_type = r'\sST$|\sDR$|\sCIR$|\sAVE$|\sCT$|\sBLVD$|\sALY$|\sPLZ$|\sPARK$|\sPKWY$|\sAPPROACH$|\sTER$|\sPL$|\sLN$|\sBRG$|(?<=\s)HL$|\sHTS$|\sSLIP$|\sROW$|\sSQ$'
        df['street_type'] = df['final_clean_address'].apply(lambda x: ' '.join(re.findall(pattern_street_type, x)))
        df['street_name1'] = df.apply(lambda x: x["final_clean_address"].replace(x["street_type"], "").strip(), axis=1)
        df['street_name'] = df.apply(lambda x: x["street_name1"].replace(x["street_direction"], "").strip(), axis=1)        
        df.rename(columns = {column2: 'raw_street_address', column1:'house_number'}, inplace = True)
        df = df.fillna('')
        
        df.drop('street_name1', axis=1, inplace = True)

        return df[['raw_street_address', 'house_number', 'street_direction', 'street_name', 'street_type', 'final_clean_address']]
    
    else:
    #         Clean the street name and return the final clean addresses
        df[column2] = df[column2].apply(lambda x: str(x).upper())
        df['street_direction_clean'] = df[column2].apply(lambda x: street_direction(str(x)))
        df['street_type_clean'] = df['street_direction_clean'].apply(lambda x: street_type(x))

        df['street_without_hn'] = df['street_type_clean'].apply(lambda x: split(str(x)))

        df['street_number_name_clean'] = df['street_without_hn'].apply(lambda x: street_number_name(x))
        df['street_name_clean'] = df['street_number_name_clean'].apply(lambda x: street_name(x))
        df['final_clean_address'] = df['street_name_clean'].apply(lambda x: special_case(x))

        df['final_clean_address'] = np.where(df['final_clean_address'] == 'ST', df['street_type_clean'], df['final_clean_address'])

    #         Extract different components of the addresses, e.g direction: W/E/S/N; street type: AVE/PL/ST/CIR, etc.

        pattern_hn = r'^\d+\s|^\d+R\s'
        df['house_number'] = df[column2].apply(lambda x: ' '.join(re.findall(pattern_hn, x)))

        pattern_number = re.compile(r'\d+')
        df['house_number'] = df['house_number'].apply(lambda x: ' '.join(re.findall(pattern_number, x)))

        pattern_street_direction = r'(?<=\s)[N|W|S|E]\s|^[N|W|S|E]\s|(?<=\s)[N|W|S|E]\d+'
        df['street_direction'] = df['final_clean_address'].apply(lambda x: ' '.join(re.findall(pattern_street_direction, x)))

        pattern_street_type = r'\sST$|\sDR$|\sCIR$|\sAVE$|\sCT$|\sBLVD$|\sALY$|\sPLZ$|\sPARK$|\sPKWY$|\sAPPROACH$|\sTER$|\sPL$|\sLN$|\sBRG$|(?<=\s)HL$|\sHTS$|\sSLIP$|\sROW$|\sSQ$'
        df['street_type'] = df['final_clean_address'].apply(lambda x: ' '.join(re.findall(pattern_street_type, x)))
        df['street_name1'] = df.apply(lambda x: x["final_clean_address"].replace(x["street_type"], "").strip(), axis=1)
        df['street_name'] = df.apply(lambda x: x["street_name1"].replace(x["street_direction"], "").strip(), axis=1)        
        df.rename(columns = {column2: 'raw_street_address'}, inplace = True)
        df = df.fillna('')
        
        df.drop('street_name1', axis=1, inplace = True)

        return df[['raw_street_address', 'house_number', 'street_direction', 'street_name', 'street_type', 'final_clean_address']]


