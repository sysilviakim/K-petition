from kiwipiepy import Kiwi
import pandas as pd
from glob import glob
import os

## create kiwi object
kiwi = Kiwi()

## Define the years
years = range(2002, 2023 + 1)

## load and concat files
file_names = [f"data/tidy/pub_petition_content_{year}.csv" for year in years]
petition_df = pd.concat(map(pd.read_csv, file_names), ignore_index=True)

## append id and date variable
petition_df['id'] = range(1, len(petition_df) + 1)
petition_df['year'] = petition_df['date_petitioned'].str[:4]
petition_df['month'] = petition_df['date_petitioned'].str[5:7]

## use english column name
petition_df.columns.values[6] = "bodytext"

## function for space correction
def correct_spacing(text):
    if isinstance(text, str):
        return kiwi.space(text)  ## spacing correction for strings
    else:
        return text  ## if not string, just return the original value

## create new column with corrected spacing
petition_df['corrected_bodytext'] = petition_df['bodytext'].apply(correct_spacing)


petition_df.to_csv("data/tidy/pub_petition_corrected.csv",index=False)


## next steps
## preprocessing
## typos, special characters, ...
## reference: https://chocolemon.tistory.com/139
import re
import emoji ## remove emoji
from soynlp.normalizer import repeat_normalize ## remove repeated constants such as ㅋㅋ

def preprocess(text):
    if isinstance(text, str): ## preprocess only if the input is string class
        ## replace newline characters with space
        text = text.replace("\n", " ")
        
        ## replace double spaces with single spaces
        text = text.replace("  ", " ")
        
        ## replace everything that is not Korean, numbers, or spaces with a single space
        text = re.compile('[^ㄱ-ㅎㅏ-ㅣ0-9가-힣]+').sub(' ', text)
        
        ## normalize repeated characters (ㅋㅋㅋ becomes ㅋ, for example)
        text = repeat_normalize(text, num_repeats=1)
        
        ## trim leading and trailing whitespace
        text = re.sub(r"^\s+|\s+$", "", text)
    
    return text

petition_df['corrected_bodytext'] = petition_df['corrected_bodytext'].apply(preprocess)


## tokenize
## split into sentences? words?
