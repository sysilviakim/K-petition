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


## 1. preprocess corrected_bodytext using above
## typos, special characters, ...
## reference: https://chocolemon.tistory.com/139
import re
import emoji ## remove emoji
from soynlp.normalizer import repeat_normalize ## remove repeated constants such as ㅋㅋ

def cleanup(text):
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

petition_df['corrected_bodytext'] = petition_df['corrected_bodytext'].apply(cleanup)

## tokenize (remove stopwords)
from kiwipiepy.utils import Stopwords
stopwords = Stopwords()

## tokenize command
## each argument, when toggled true, will improve quality of preprocessing but will slow it down
## kiwi.tokenize("text", 
##                  normalize_coda = True, ## 받침으로 인한 분석실패 처리 e.g. 먹었엌ㅋㅋ 
##                  z_coda = True, ## 조사 및 어미에 붙는 받침 분리 e.g. 먹었어욥 -> 먹었어요 + 
##                  stopwords=stopwords)

## strategy1: keep only nouns, verbs, and adjectives
def extract_nva(text):
    nva = []
    if isinstance(text, str):
        tokens = kiwi.tokenize(text, normalize_coda = True, z_coda = True, stopwords=stopwords)
        for result in tokens:
            morpheme, tag, start, end = result
            
            if tag.startswith(('NN', 'VV', 'VA')):  ## keep only nouns, verbs, and adjectives
                nva.append(morpheme)  ## append the base form of morphemes
        
    return " ".join(nva)  ## reassemble nouns, verbs, and adjectives

## strategy2: lemmatize, then keep only nouns, verbs, and adjectives
def extract_nva(text):
    nva = []
    
    if isinstance(text, str):
        tokens = kiwi.analyze(text, normalize_coda = True, z_coda = True)
        for result in tokens[0][0]:
            morpheme, tag, start, end = result
            
            if tag.startswith(('NN', 'VV', 'VA')):  ## keep only nouns, verbs, and adjectives
                if (morpheme, tag) not in stopwords: ## remove stopwords
                    nva.append(morpheme)  ## append the base form of morphemes
                    
    return " ".join(nva)  ## reassemble nouns, verbs, and adjectives

## 2. tokenize and save output as a separate list (takes about 30 minutes)
petition_text = petition_df['corrected_bodytext'].apply(extract_nva)

petition_text.to_csv("data/tidy/pub_petition_corrected_lm.csv",index=False)

## -- codes below don't work nicely -- ##
## use R to create DFM as sparse matrix object

## 3. use below to turn the list into DFM
from sklearn.feature_extraction.text import CountVectorizer

## Bag of Words
vectorizer = CountVectorizer()
X = vectorizer.fit_transform(petition_text)

## Convert to a dense matrix and display the feature names (i.e., the words)
dfm = X.toarray()
vocab = vectorizer.get_feature_names_out()

##import numpy as np
##np.savetxt('data/tidy/pub_petition_dfm.txt', dfm)

from scipy.sparse import coo_matrix
from scipy.io import mmwrite

sparse_matrix = coo_matrix(dfm)
mmwrite('data/tidy/pub_petition_dfm.mtx', sparse_matrix) ## loadable in R using Matrix package, readMM("pub_petition_dfm.mtx")
## got killed..?!

with open('data/tidy/feature_names.txt', 'w') as f:
    for token in vocab:
        f.write(f"{token}\n")
