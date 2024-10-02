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
## clean up
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

## tokenize
## preprocessing
## remove stopwords
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
    tokens = kiwi.tokenize(text, normalize_coda = True, z_coda = True, stopwords=stopwords)
    nva = []
    for result in tokens:
        morpheme, tag, start, end = result
        
        if tag.startswith(('NN', 'VV', 'VA')):  ## keep only nouns, verbs, and adjectives
            nva.append(morpheme)  ## append the base form of morphemes
    
    return " ".join(nva)  ## reassemble nouns, verbs, and adjectives

## strategy2: lemmatize, then keep only nouns, verbs, and adjectives
def extract_nva(text):
    tokens = kiwi.analyze(text, normalize_coda = True, z_coda = True)
    nva = []
    for result in tokens[0][0]:
        morpheme, tag, start, end = result
        
        if tag.startswith(('NN', 'VV', 'VA')):  ## keep only nouns, verbs, and adjectives
            if (morpheme, tag) not in stopwords: ## remove stopwords
                nva.append(morpheme)  ## append the base form of morphemes
    
    return " ".join(nva)  ## reassemble nouns, verbs, and adjectives

## 1. preprocess corrected_bodytext using above
## 2. separate out as list preprocessed_texts = petition_df['corrected_bodytext']
## 3. use below to turn the list into BoW

## Bag of Words
from sklearn.feature_extraction.text import CountVectorizer

vectorizer = CountVectorizer()
X = vectorizer.fit_transform(preprocessed_texts)

# Convert to a dense matrix and display the feature names (i.e., the words)
bow = X.toarray()
vocab = vectorizer.get_feature_names_out()
