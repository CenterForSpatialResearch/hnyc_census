#import the packages that needed
import pandas as pd
import numpy as np
import regex as re
import warnings
warnings.filterwarnings('ignore')

def street_name(x: str) -> str:
    """
    Introduction:
    -------------
    This function can help users to correct the street names in the street addresses. Some street names are different from past in the geospitial dataset. Therefore, we need to correct the street names to make them conform to the present street names.
    
    Inputs:
    -------------
    'x': str. The street addresses in your dataset.

    Outputs:
    -------------
    'x': str. Street addresses with correct street names.

    Example:
    -------------
    >>> e = street_name('BATTIE AVE')
    >>> e
    'BALTIC AVE'
    """
    
    pattern = re.compile(r'(?<=\s)ALLANTIC\s|(?<=\s)ATLASTA\s')
    x = re.sub(pattern, "ATLANTIC", x)
    pattern = re.compile(r'(?<=\s)ALLEM\s')
    x = re.sub(pattern, "ALLEN", x)    

    pattern = re.compile(r'(?<=\s)CROTON\s|(?<=\s)GROTON\s')
    x = re.sub(pattern, "AUDUBON", x)    
    pattern = re.compile(r'(?<=\s)(AT RINS)\s')
    x = re.sub(pattern, "ATKINS", x)  
    pattern = re.compile(r'(?<=\s)AMSTERDAM\s')
    x = re.sub(pattern, "AMSTERDAM", x) 

    pattern = re.compile(r'(?<=\s)BATTIE\s')
    x = re.sub(pattern, "BALTIC", x)      
    pattern = re.compile(r'(?<=\s)BARREE\s')
    x = re.sub(pattern, "BARROW", x)   
    pattern = re.compile(r'(?<=\s)BESSHLEY\s')
    x = re.sub(pattern, "BURLING", x)       
    pattern = re.compile(r'(?<=\s)BIRY\s|(?<=\s)(B WAY)\s|(?<=\s)BWAY\s|(?<=\s)BRAKSWAY\s')
    x = re.sub(pattern, "BROADWAY", x)         
    pattern = re.compile(r'(?<=\s)BUSTWICK\s')
    x = re.sub(pattern, "BUSHWICK", x) 
    pattern = re.compile(r'(?<=\s)BUTTER\s')
    x = re.sub(pattern, "BUTLER", x) 
    pattern = re.compile(r'(?<=\s)BREEVORT\s')
    x = re.sub(pattern, "BREVOORT", x) 
    pattern = re.compile(r'(?<=\s)BRENNEL\s|(?<=\s)BROOMES\s|(?<=\s)BROOM\s|(?<=\s)BRANNAS\s|(?<=\s)BROWN\s')
    x = re.sub(pattern, "BROOME", x) 
    pattern = re.compile(r'(?<=\s)BLACKER\s|(?<=\s)BLENKER\s')
    x = re.sub(pattern, "BLEECKER", x) 

    pattern = re.compile(r'(?<=\s)CLAIR\s')
    x = re.sub(pattern, "CLASSON", x) 
    pattern = re.compile(r'(?<=\s)CLISTEN\s')
    x = re.sub(pattern, "CLINTON", x)     
    pattern = re.compile(r'(?<=\s)CHERY\s')
    x = re.sub(pattern, "CHERRY", x) 
    pattern = re.compile(r'(?<=\s)CHRYSTEE\s|(?<=\s)CHRYSTAL\s|(?<=\s)CHTYSTIE\s|(?<=\s)CHRYSTEL\s')
    x = re.sub(pattern, "CHRYSTIE", x)    
    pattern = re.compile(r'(?<=\s)CENTRAL PARK\s')
    x = re.sub(pattern, "CENTRAL PARK", x) 
    pattern = re.compile(r'(?<=\s)CARRAL\s|(?<=\s)COYAL\s')
    x = re.sub(pattern, "CANAL", x)    
    pattern = re.compile(r'(?<=\s)COLUMBIN\s')
    x = re.sub(pattern, "COLUMBIA", x) 
    pattern = re.compile(r'(?<=\s)CAMNON\s')
    x = re.sub(pattern, "CANNON", x) 
    pattern = re.compile(r'(?<=\s)CROWH\s')
    x = re.sub(pattern, "CROWN", x) 

    pattern = re.compile(r'(?<=\s)DEVAL\s')
    x = re.sub(pattern, "DEVOE", x)
    pattern = re.compile(r'(?<=\s)DEBOUCHEL\s')
    x = re.sub(pattern, "DEBEVOISE",x)
    pattern = re.compile(r'(?<=\s)DAFONT\s')
    x = re.sub(pattern, "DUPONT", x)
    pattern = re.compile(r'(?<=\s)DEGRAN\s')
    x = re.sub(pattern, "DEGRAW", x)
    pattern = re.compile(r'(?<=\s)DENBO\s|(?<=\s)DEKALB\s')
    x = re.sub(pattern, "DE KALB", x)
    pattern = re.compile(r'(?<=\s)DELAMERE\s|(?<=\s)DALANEY\s|(?<=\s)DELANEY\s|(?<=\s)DELANCY\s')
    x = re.sub(pattern, "DELANCEY", x) 

    pattern = re.compile(r'(?<=\s)ELTHZROTH\s|(?<=\s)ELLSWICK\s')
    x = re.sub(pattern, "ELLIOTT", x)
    pattern = re.compile(r'(?<=\s)ELDREDGE\s|(?<=\s)CLARIDGE\s')
    x = re.sub(pattern, "ELDRIDGE", x) 
    pattern = re.compile(r'(?<=\s)ESSEY\s')
    x = re.sub(pattern, "ESSEX", x) 

    pattern = re.compile(r'(?<=\s)FORSYTHE\s')
    x = re.sub(pattern, "FORSYTH", x) 
    pattern = re.compile(r'(?<=\s)FLATHISH\s')
    x = re.sub(pattern, "FLATBUSH", x)

    pattern = re.compile(r'(?<=\s)GLANCE\s')
    x = re.sub(pattern, "GRAND", x) 
    pattern = re.compile(r'(?<=\s)GOAST\s')
    x = re.sub(pattern, "GOERCK", x)
    pattern = re.compile(r'(?<=\s)GREENS\s')
    x = re.sub(pattern, "GREENE", x)
    pattern = re.compile(r'(?<=\s)GREENRICH\s|(?<=\s)GAMWICH\s')
    x = re.sub(pattern, "GREENWICH", x) 

    pattern = re.compile(r'(?<=\s)HOUTON\s')
    x = re.sub(pattern,  "HOUSTON", x) 
    pattern = re.compile(r'(?<=\s)HAVES\s')
    x = re.sub(pattern, "HEWES", x) 
    pattern = re.compile(r'(?<=\s)HAKEY\s')
    x = re.sub(pattern, "HALSEY", x)
    pattern = re.compile(r'(?<=\s)HEWEY\s')
    x = re.sub(pattern, "HENRY", x)
    pattern = re.compile(r'(?<=\s)HICK\s')
    x = re.sub(pattern, "HICKS", x)
    pattern = re.compile(r'(?<=\s)HUMBOLOT\s|(?<=\s)HUMBARD\s|(?<=\s)HUMBOLT\s')
    x = re.sub(pattern, "HUMBOLDT", x) 

    pattern = re.compile(r'(?<=\s)JOHOM\s')
    x = re.sub(pattern, "JOHNS", x) 

    pattern = re.compile(r'(?<=\s)KIOP\s|(?<=\s)HEAP\s')
    x = re.sub(pattern, "KEAP", x) 

    pattern = re.compile(r'(?<=\s)(LAY FAY ESTE)\s|(?<=\s)LADORATT\s|(?<=\s)LAFYAYETTE\s')
    x = re.sub(pattern, "LAFAYETTE", x)
    pattern = re.compile(r'(?<=\s)LIRA\s|(?<=\s)LOUMOR\s|(?<=\s)LARMER\s')
    x = re.sub(pattern, "LORIMER", x)
    pattern = re.compile(r'(?<=\s)LAAVIUK\s')
    x = re.sub(pattern, "LAWRENCE", x) 
    pattern = re.compile(r'(?<=\s)LAIDLOW\s')
    x = re.sub(pattern, "LUDLOW", x) 
    pattern = re.compile(r'(?<=\s)TEX|LEX\s') # perl = True
    x = re.sub(pattern, "LEXINGTON", x)   
    pattern = re.compile(r'(?<=\s)REPPERTS\s')
    x = re.sub(pattern, "LEFFERTS", x)

    pattern = re.compile(r'(?<=\s)PARLE\s|(?<=\s)MALLE\s|(?<=\s)MYETTE\s')
    x = re.sub(pattern, "MYRTLE", x)
    pattern = re.compile(r'(?<=\s)(MC DOUGALL)\s|(?<=\s)(MC DOUGAL)\s|(?<=\s)MCDOUGALL\s')
    x = re.sub(pattern, "MCDOUGAL", x)
    pattern = re.compile(r'(?<=\s)(MC DONOUGH)\s')
    x = re.sub(pattern, "MCDONOUGH", x)
    pattern = re.compile(r'(?<=\s)MANZA\s|(?<=\s)MAREY\s')
    x = re.sub(pattern, "MARCY", x)
    pattern = re.compile(r'(?<=\s)MADISON\s')
    x = re.sub(pattern, "MADISON", x)
    pattern = re.compile(r'(?<=\s)MESCOLE\s')
    x = re.sub(pattern, "MESEROLE", x)
    pattern = re.compile(r'(?<=\s)MEASE\s')
    x = re.sub(pattern, "MOORE", x)
    pattern = re.compile(r'(?<=\s)MEDDLER\s')
    x = re.sub(pattern, "MIDDLETON", x)
    pattern = re.compile(r'(?<=\s)MANGEN\s')
    x = re.sub(pattern, "MANGIN", x)
    pattern = re.compile(r'(?<=\s)HAULL\s|(?<=\s)MALLERY\s')
    x = re.sub(pattern, "MULBERRY", x)

    pattern = re.compile(r'(?<=\s)NAPOLK\s')
    x = re.sub(pattern, "NORFOLK", x)
    pattern = re.compile(r'(?<=\s)(VAST AND)\s')
    x = re.sub(pattern, "NOSTRAND", x)

    pattern = re.compile(r'(?<=\s)DAK\s')
    x = re.sub(pattern, "OAK", x)
    pattern = re.compile(r'(?<=\s)OLWEN\s')
    x = re.sub(pattern, "OLIVER", x)
    pattern = re.compile(r'(?<=\s)GERHARD\s')
    x = re.sub(pattern, "ORCHARD", x)

    pattern = re.compile(r'(?<=\s)PUTT\s')
    x = re.sub(pattern, "PITT", x)
    pattern = re.compile(r'(?<=\s)PERROTT\s|(?<=\s)(PERROTT PREMPONT)\s')
    x = re.sub(pattern, "PIERREPONT", x)
    pattern = re.compile(r'(?<=\s)PLAD\s')
    x = re.sub(pattern, "PLACE", x)
    pattern = re.compile(r'(?<=\s)PRUFER\s')
    x = re.sub(pattern, "PROSPECT", x)
    pattern = re.compile(r'(?<=\s)PREDIDUNT\s')
    x = re.sub(pattern, "PRESIDENT", x)
    pattern = re.compile(r'(?<=\s)PALOKA\s')
    x = re.sub(pattern, "PULASKI", x)

    pattern = re.compile(r'(?<=\s)RUTHIE\s')
    x = re.sub(pattern, "RUTLEDGE", x)
    pattern = re.compile(r'(?<=\s)RIDAL\s')
    x = re.sub(pattern, "RIDGE", x)
    pattern = re.compile(r'(?<=\s)RAYSON\s')
    x = re.sub(pattern, "RYERSON", x)
    pattern = re.compile(r'(?<=\s)REVENTON\s')
    x = re.sub(pattern, "RIVINGTON", x)
    pattern = re.compile(r'(?<=\s)RUALMAINE\s|(?<=\s)(RICER SIDE)\s')
    x = re.sub(pattern, "RIVERSIDE", x)
    pattern = re.compile(r'(?<=\s)REDERICK\s|(?<=\s)RENNICK\s')
    x = re.sub(pattern, "RENWICK", x)

    pattern = re.compile(r'(?<=\s)SELLTOWN\s')
    x = re.sub(pattern, "SULLIVAN", x)
    pattern = re.compile(r'(?<=\s)SISH\s')
    x = re.sub(pattern, "SIDE", x)
    pattern = re.compile(r'(?<=\s)STUCKER\s')
    x = re.sub(pattern, "STEUBEN", x)
    pattern = re.compile(r'(?<=\s)STATES\s')
    x = re.sub(pattern, "STATE", x)
    pattern = re.compile(r'(?<=\s)SCHAALS\s')
    x = re.sub(pattern, "SCHOLES", x)
    pattern = re.compile(r'(?<=\s)SUMME\s')
    x = re.sub(pattern, "SUMMIT", x)
    pattern = re.compile(r'(?<=\s)SCHOMERDOSA\s')
    x = re.sub(pattern, "SCHERMERHORN", x)
    pattern = re.compile(r'(?<=\s)DOUTH\s|(?<=\s)SONSE\s')
    x = re.sub(pattern, "SOUTH", x)
    pattern = re.compile(r'(?<=\s)STUYVESTANT\s')
    x = re.sub(pattern, "STUYVESANT", x)

    pattern = re.compile(r'(?<=\s)STONPSON\s')
    x = re.sub(pattern, "THOMPSON", x)
    pattern = re.compile(r'(?<=\s)TRAY\s')
    x = re.sub(pattern, "TROY", x)
    pattern = re.compile(r'(?<=\s)TAYLER\s')
    x = re.sub(pattern, "TAYLOR", x)

    pattern = re.compile(r'(?<=\s)WMON\s')
    x = re.sub(pattern, "UNION", x)

    pattern = re.compile(r'(?<=\s)(WAR CAREN)\s')
    x = re.sub(pattern, "VAN BUREN", x)
    pattern = re.compile(r'(?<=\s)VEMON\s')
    x = re.sub(pattern, "VERNON", x)
    pattern = re.compile(r'(?<=\s)VANDERLY\s|(?<=\s)VANDERSLIDE\s')
    x = re.sub(pattern, "VANDERBILT", x)

    pattern = re.compile(r'(?<=\s)WYONIA\s')
    x = re.sub(pattern, "WYONA", x)
    pattern = re.compile(r'(?<=\s)WITKINS\s')
    x = re.sub(pattern, "WATKINS", x)
    pattern = re.compile(r'(?<=\s)WALLWORTH\s')
    x = re.sub(pattern, "WALWORTH", x)
    pattern = re.compile(r'(?<=\s)WHIPPER\s')
    x = re.sub(pattern, "WHIPPLE", x)
    pattern = re.compile(r'(?<=\s)WALLABANK|(?<=\s)WALKABOUT\s')
    x = re.sub(pattern, "WALLABOUT", x)
    pattern = re.compile(r'(?<=\s)WASH\s|(?<=\s)WASTEWATER\s')
    x = re.sub(pattern, "WASHINGTON", x) 

    return x

