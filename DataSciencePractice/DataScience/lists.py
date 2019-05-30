# -*- coding: utf-8 -*-
"""
Created on Tue May 28 14:46:30 2019

@author: Gadiyaram7V
"""
#import libraries
import math
##Lists
#Create empty list in two ways
empty_list = []
empty_list = list()

#Create a list
simpsons = ['homer','marge','bart','vamsi','movie']

#examine a list
simpsons[0] #returns homer
simpsons[0:5] #returns homer
simpsons[2:4] #returns ['bart','vamsi]
len(simpsons)  #returns the length

#modify a list
simpsons.append('lisa') #note append takes only one argument
simpsons.extend(['itchy','scratchy']) #appends multiple arguments
print(*simpsons) #prints list 
simpsons.insert(0,'maggie') #insert element at index 0 and shift all the elements to right
simpsons.remove('bart') #searches for the first instance and removes
simpsons.pop(0) #removes element 0 and returns it
del simpsons[0] #removes element 0 and does not return it

simpsons[0] = 'krusty'


# concatenate lists (slower than 'extend' method)
neighbors = simpsons + ['ned','rod','todd']
print(*neighbors)
# find elements in a list
simpsons.count('lisa') # counts the number of instances
simpsons.index('itchy') # returns index of first instance


# list slicing [start:end:stride]
weekdays = ['mon','tues','wed','thurs','fri']
weekdays[0] # element 0
weekdays[0:3] # elements 0, 1, 2
weekdays[:3] # elements 0, 1, 2
weekdays[3:] # elements 3, 4
weekdays[-1] # last element (element 4)
weekdays[::2] # every 2nd element (0, 2, 4)
weekdays[::-1] # backwards (4, 3, 2, 1, 0)


# alternative method for returning the list backwards
list(reversed(weekdays))
# sort a list in place (modifies but does not return the list)
simpsons.sort()
print(*simpsons)
simpsons.sort(reverse=True) # sort in reverse
simpsons.sort(key=len) # sort by a key
# return a sorted list (but does not modify the original list)
sorted(weekdays)
sorted(weekdays, reverse=True)
sorted(weekdays, key=len)
# create a second reference to the same list
num = [1, 2, 3]
same_num = num
same_num[0] = 0 # modifies both 'num' and 'same_num'
# copy a list (three ways)
new_num = num.copy()
new_num = num[:]
new_num = list(num)



 
