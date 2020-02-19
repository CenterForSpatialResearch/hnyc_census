---
Title: House Number Sequence Generation & Merging Change Log
Date: 18 Feb 2020
Author: Jolene Lim
---
# House Number Sequence Generation & Merging Change Log
This documents changes to Bo's original functions.

## 05_00_getSequence.R Change Log
Date: 18 Feb 2020

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