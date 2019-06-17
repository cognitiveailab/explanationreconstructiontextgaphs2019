# Scala Baseline Scaffold for TextGraphs 2019 Shared Task on Explanation Regeneration

This is a term frequency baseline for the TextGraphs 2019 shared task on explanation regeneration.  This method compares the terms in the question and answer candidates of a given question with the term frequency in each row of the tablestore, evaluating each row separately.  Rows for a given question are then ranked based on their scores, producing an overall ranked list of table rows in order of decreasing likelihood to appear in an explantion for a question. 

For more information, please see [https://github.com/umanlp/tg2019task](https://github.com/umanlp/tg2019task) .

## Data
The data for this shared task is available at: 

[http://cognitiveai.org/dist/worldtree_corpus_textgraphs2019sharedtask_withgraphvis.zip](http://cognitiveai.org/dist/worldtree_corpus_textgraphs2019sharedtask_withgraphvis.zip).

This should be unzipped in the main path of this repository (if it is placed in a different location, please point to this new location in the explregen.props file). Please note that this distribution is still subject to the terms set forth in the included license `EULA AI2 Mercury Dataset 01012018.docx`.

## SVM Rank
This exapmle also makes use of SVM Rank, which can be downloaded here:

https://www.cs.cornell.edu/people/tj/svm_light/svm_rank.html

The executables should be placed in the main path of this repository.

## To run
To run:
Main entry point: explanationregeneration.ExplanationRegeneration

 Suggested memory arguments:
 -Xmx8g

 Command line arguments:
-props props/explregen.props

## Output
The output displays both the overall score (MAP, here 0.2890), as well as a finer-grained breakdown of performance based on a given row's explanatory role (CENTRAL, GROUNDING, LEXICALGLUE), whether it has lexical overlap with the question/answer text or not, and what category of table it comes from (RETRIEVAL, INFERENCE SUPPORTING, COMPLEX INFERENCE).  For more information, please see the Worldtree corpus paper (Jansen et al., LREC 2018).
```
-------------------------------------------------
 Performance on Evaluation Set
-------------------------------------------------

MAP: 0.2890

MAP_ROLE_CENTRAL:    0.3181
MAP_ROLE_GROUNDING:  0.1142
MAP_ROLE_LEXGLUE:    0.0446
MAP_ROLE_BACKGROUND: 0.0139

MAP_LEXOVERLAP:      0.3027
MAP_NOLEXOVERLAP:    0.0006

MAP_TABKT_RET:       0.3023
MAP_TABKT_RET/LEX:   0.1297
MAP_TABKT_INFSUPP:   0.2753
MAP_TABKT_COMPLEX:   0.1944

Precision@1          0.4813
Precision@2          0.4136
Precision@3          0.3754
Precision@4          0.3629
Precision@5          0.3505


EnabledFeatures: TFIDF
Writing predictions to predict.txt

-------------------------------------------------
 Model Weights
-------------------------------------------------
SVM Weights: 
weights.size: 3
featureLexicon.size 2
weight: 26.43860 	 feature: COS_Q  (idx:0)
weight: 26.43860 	 feature: COS_Q  (idx:0)
weight: 14.72668 	 feature: COS_A  (idx:1)
weight: 14.72668 	 feature: COS_A  (idx:1)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - 

```

## Prediction file for Codalab competition
By default, this is output as predict.txt
