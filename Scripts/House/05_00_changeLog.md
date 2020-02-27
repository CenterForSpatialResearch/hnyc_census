---
Title: House Number Sequence Generation & Merging Change Log
Date: 25 Feb 2020
Author: Jolene Lim
---
# House Number Sequence Generation & Merging Change Log
This documents changes to Bo's original functions.

### Date: 26 Feb 2020
STATUS: getMergeSeq() can now be used as a wrapper function to get and merge sequences. This should be the only function accessed by users. For **documentation** on how to use this function, check the function's roxygen comments.   

Changes:  
- Introduced adaptive jump sizes for merge sequence step  
  - This asks users to set a jump size for each digit length, in the format of a vector, c(max_jump_2digithn, max_jump_3digithn, max_jump_4ormoredigithn). 
- Cleaned up code (removed old versions of code + comments)  
- getMergeSeq() now can incorporate both types of options- check params applied to get sequence and merge sequence (as e.g. jump sizes may differ for each stage)   

Remaining Issues (Low Priority):  
- Implement check direction for merge sequence, for now by default it is off  
- Incorporating jump size checks using getDirectionalHeads()  
  - can optimize code  

### Date: 25 Feb 2020
Remaining Issues:  
- **VERY HIGH**: How to deal with NAs? Right now, added new functionality to treat remove all NAs before sequence generation and simply fill down the SEQ for NA house numbers  
- **High**: Produce final documentation on how to use functions and clean up code  
- **Medium**: Complete test functions and apply on larger dataset  
  - Existing functions were to check for original mistakes  
  - Not carried on due to time constraints but quite useful to have  
- **Low**: getMergeSeq() can incorporate both types of options- check params applied to get sequence and merge sequence (as e.g. jump sizes may differ for each stage)   
- Low: Implement check direction for merge sequence, for now by default it is off  
- Low: incorporating jump size checks using getDirectionalHeads()  
  - can optimize code  
  - have adaptive jump sizes instead of absolute  

Changes:  
- **Major**: added new functionality to treat remove all NAs before sequence generation and simply fill down the SEQ for NA house numbers  
- Fixed all functions to work with corrected isHead  
- Incorporate all check params into functionality  

### Date: 18 Feb 2020

Remaining Issues:  
- **High**: Implement check_street in isHead()  
- **High**: To check if refactored isHead() works as intended 
- **High**: Functions are accurate up to getSequenceHead(), functions above that have not been checked if they work as intended  
- **High**: function `checkDistance()` now deprecated, unsure of original purpose  
- Low: incorporating jump size checks using getDirectionalHeads() 
  - can optimize code  
  - have adaptive jump sizes instead of absolute  

Changes:
- **Major**: extracted inner helper functions of getSequenceHead()  
  - added parameters where necessary so helper functions do not refer to global variables  
- **Major**: refactored getSequenceHead() to simplify boolean logic and allow for turning on/off of all parameters (check parity, check street, check direction and jump size). Previously only check street and jump size enabled.  
  - included parameters for all relevant functions too  
- **Major**: rewrote getDirections() to getDirectionalHeads().  
  - Originally returned a list of filled down directions  
  - Now returns overall directionality of the house num sequence it belongs in and marks if index is a directionalHead  
- **Major**: deprecated sameDirection(), replaced with isDirectionalHead() which works with new getDirectionalHeads()  
- **Major**: deprecated checkDistance() - unsure of purpose  
- **Minor**: improved readability of withinJumpSize()
  
Additions:
- Unit test file