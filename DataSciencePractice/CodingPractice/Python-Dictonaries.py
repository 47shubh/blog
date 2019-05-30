
# coding: utf-8

# ## Dictonaries Made Easy - With Real Examples:

# ## Dictionaries

# A dictionary is like a list, but more general. In a list, the positions (a.k.a. indices) have to be integers; in a dictionary the indices can be (almost) any type.
# 
# You can think of a dictionary as a mapping between a set of indices (which are called keys) and a set of values. Each key maps to a value. The association of a key and a value is called a key-value pair or sometimes an item

# eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}
# 
# Here we have dictonary eng2sp and it is enclosed by curly braces(bracket) (or) second bracket and the key value pair is seperated using comma.
# 
# Here 'one' is key and 'uno' is value, likewise it applies for other elements in dictonary, in this case the keys are actually strings they are not integers.
# 
# In this dictonary eng2sp we are mapping the english word with the spanninsh word.

# In[3]:

eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}
eng2sp


# ## Indexing - Dictonary:

# In[10]:

eng2sp['two']


# In[11]:

eng2sp['one']


# In dictonary to get the value, we need to call the key. to get value 'uno' we need
# to call key 'one' likewise for otherelements.
# 
# To get(or)retrieve the element from the dictonary we need to subset the dictonary by keys.
# 
# Note: Please make sure the elements in dictonary should comparise of key and value pair.

# 

# ### An Example of Dictionary

# Let us build a dictionary that maps English to Spanish words. In this dictionary the keys will be some english words and their corresponding values will be the corresponding spanish words.

# In[2]:

eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}


# **Note:** 
# 
# * A dictionary is created using a curly bracket
# 
# * The values in a dictionary are indexed by keys. The keys are needed to be specified while creating a dictionary. 
# 
# * As an example, 'one':'uno' represents a key-value pair.

# *What if we enclose some values (separated by commas) in a curly bracket? What type of object do we get?*
# 
# * *A dictionary with only keys.*
# * *A dictionary with elements indexed by integer*
# * *This object is not a dictionary*

# In[2]:

d={'one', 'two', 'three'}


# In[3]:

type(d)


# Here the values enclosed with curly braces is not dictonary but it is a set, when checking the datatype it shows it is a set.

# In[4]:

d={1.2:'one', 2.75:'two'}


# In[5]:

type(d)


# Here we have created a dictonary with key/value pair, here the key is of datatype float.

# ### Retrieving elements from a dictionary

# To retrieve element from the dictonary we need to subset the dictonary and the keys,it is the same thing we are doing for list as well, for string etc.

# In[7]:

eng2sp['one']


# Here eng2sp is a dictonary and when we give key('one') to it, it will give us values,the corresponding value of the key will be displayed.

# In[8]:

eng2sp['two']


# The same applies here also, we have given the key to dictonary and corresponding 
# value for the key is displayed.

# In[9]:

d[1.2]


# Here the key is datatype float(1.2), the corresponding value for the key is displayed.

# ### Adding Key-Value in a dictionary

# If we want to add extra element to the dictonary, element is nothing but key value pair, this can done by adding key and corresponding value to the dictornary.

# In[10]:

eng2sp['four'] = 'cuatro'


# Here the key needed to be added is 'four' and the corresponding value is 'cuatro'which is given to dictonary "eng2sp".

# In[11]:

eng2sp


# In[ ]:

# From the result we can see the new key:value pair is added to dictornary.


# ### in Operator for Dictionary

# We can use the in operator in dictonaries, we can check the existance of key in the dictonary using in operator.

# In[12]:

'one' in eng2sp


# Here we are check wheather 'one' is used as key in dictonary(eng2sp)(or)checking its existiance.
# It shows the result as TRUE, it shows us the key 'one' is available in dictonary
# eng2sp.

# In[13]:

'uno' in eng2sp


# Here the result is False, it shows us the that there is no key called 'uno' is
# available in dictonary. 'uno' is value available in dictory not key.

# ### Working with Real Examples:

# ### Counting the Frequency Distribution of Letters in a Word

# In[ ]:

#Here we are demonstrating the application of dictonary. see the small problem below:

#Here we are going to find the frequency distribution of the word "banana" using dictonary.

word = 'banana'

#What is the frequency distribution?
#Finding the frequency of the letter in word (or) no of occurance of letter in a word.

#Here we have the word "banana".
# In[ ]:

#Frequency distriution of the letters in 'banana'

word = 'banana'
count= dict()

for letter in word:
    if letter not in count:
        count[letter] = 1
    else:
        count[letter] = count[letter]+1
    print(count)
print(count)


# In[18]:

#Explaining the code step by step:
word = 'banana'
for letter in word:
    print(letter) 
# Here we are creating word 'banana' and inputing it through for loop.
# the for loop counts each letter in the word


# step-2:
#     
# Here in the code we are creating a empty dictonary.
# 
# count= dict()
# 
# We are naming the empty dictonary as count.
# 
# The empty dictornary can be create in two ways as below:
#     
# count = dict()
# 
# (or)
# 
# count = {}
# 
# we can create dictonary using function dict() (or) simply giving open,close curly braces.

# In[17]:

#step-3 - whole program:

word = 'banana'
count = dict()

for letter in word:
    if letter not in count:
        count[letter] = 1
    else:
        count[letter] = count[letter] + 1
        
print(count)       


# In this code we have found the frequency of the word 'banana', and the result is
# diplayed as "{'b': 1, 'a': 3, 'n': 2}", in the word letter b occurs 1 time, a occurs 
# 3 times and n occurs 2 times.

# In[15]:


# **The 'get' method for dictionary:**

# Dictionaries have a method called get that takes a key and a default value. If the key appears in the dictionary, get returns the corresponding value; otherwise it returns the default value. 
# Note: The get method is data structure specific. 

# In[ ]:

eng2sp = {'one': 'uno', 'two': 'dos', 'three': 'tres'}


# In[3]:

#For example,
eng2sp.get('one',0) #Return 0 as default value
#Here value for the key 'one' is 'uno' which is returned by get method.


# In[4]:

eng2sp.get('five',0)
#Here we dont have the key "five" in dictonary hence it returned default value 0


# **Making the counting simple using the 'get' method**

# In[12]:

word = 'banana'
count= dict()
print(count)

for letter in word:
    count[letter] = count.get(letter,0) + 1
    print(count)
count


# Here we implementing the same frequency distribution of the word "banana" using get method.
# Here we are elminating the if else structure and using the get method.
# Code is simplified using get method.
