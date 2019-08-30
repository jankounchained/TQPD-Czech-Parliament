# TQPD-Czech-Parliament
[Towards quantifying parliamentary discourse](https://drive.google.com/file/d/1xJkmF63_oAZiWSHnZFLlm59z7yUaB0KC/view?usp=sharing) (2019) - code. Older versions can be found [here](https://github.com/supplyandcommand/CZpar).

### What's in here
  * code to scrape 9 years of Czech parliamentary discussion, consisting of 384,430 speeches. Scraped from: http://www.psp.cz/eknih/
  * tidying, structuring, stop word removal and adding metadata about MPs
  * tokenization and lemmatization using [udpipe](https://github.com/ufal/udpipe)
  * lda using [vowpal wabbit](https://github.com/VowpalWabbit/vowpal_wabbit)
  * measuring topic usage over time using Kullback–Leibler divergence.
    * novelty: how different (surprising) a speech was from the last w ones 
    * resonance: how lasting (impactful) a speech was over the next w ones
  * modelling the relationship between a speech's novelty and resonance using linear mixed-effect models.
  * a few plots

![alt text](https://raw.githubusercontent.com/supplyandcommand/TQPD-Czech-Parliament/master/plots/CZpar_27.PNG)

![alt text](https://raw.githubusercontent.com/supplyandcommand/TQPD-Czech-Parliament/master/plots/CZpar_5000.PNG)


Paper introducing the novelty, transience and resonance measures: [Barron, Huang, Spang & DeDeo (2018)](https://www.pnas.org/content/115/18/4607).
Thanks to [Malte Lau Petersen](https://gitlab.com/maltelau)!
