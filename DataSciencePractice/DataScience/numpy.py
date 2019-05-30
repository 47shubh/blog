import numpy as np

#load the library and check its version
import numpy as np
print(np.__version__)

## The data manipulation capabilities of pandas are built on top of the numpy library. In a way, numpy is a dependency of the pandas library.
import pandas as pd

#1) Creating Arrays in numpy
get_zero = np.zeros(10, dtype='int')
print(get_zero)

#creating a 2 row x 4 column matrix
data = np.ones((2,4),dtype=float)
print(data)


#creating a matrix with a predefined value
data = np.full((2,4),4.2)
print(data)

##create an array with a set sequence
data = np.arange(0,20,2)
print(data)

#create an array of even space between the given range of values
data = np.linspace(0,10,4)
print(data)

#create a 3x3 array with mean 0 and standard deviation 1 in a given dimension
data = np.random.normal(0,1,(3,3))
print(data)

#create an identity matrix
data = np.eye(4)
print(data)

#set a random seed
#With the seed reset (every time), the same set of numbers will appear every time
np.random.seed(0)

# 1 d
x1 =np.random.randint(10,size=6)
# 2 D
x2 =np.random.randint(10,size=(3,5))
# 3 D
print("3d")
x3 = np.random.randint(10,size=(3,4,5))
print(x3)
print(x3.size)
print(x3.shape)
print(x3.ndim)


## 2 Array Indexing

x1 = np.array([4, 3, 4, 4, 8, 4])
print(x1[2])
x1[0]

#get the last value
x1[-1]

#in a multidimensional array, we need to specify row and column index
x2= np.array([[3, 7, 5, 5],
      [0, 1, 5, 9],
      [3, 0, 5, 0]])


#1st row and 2nd column value
print(x2[2,2])

#replace value at 0,0 index
x2[0,0] = 12
x2



## 3) Array Slicing
x = np.arange(10)
#from start to 4th position
print(x[:5])
#from 4th position to end
x[4:]

#from 4th to 6th position
x[4:7]

#return elements at even place
print(x[ : : 2])

#return elements from first position step by two
x[1::2]

#reverse the array
x[::-1]

## 4 Array Concatenation

x = np.array([1, 2, 3])
y = np.array([3, 2, 1])
z= np.concatenate([x, y])
print(z)

# for combine a 2D array with 1D  use np.vstack or np.hstack
x = np.array([3,4,5])
grid = np.array([[1,2,3],[17,18,19]])
np.vstack([x,grid])

z = np.array([[9],[9]])
np.hstack([grid,z])

### arrange , reshape it
reshap= np.arange(16).reshape((4,4))
print(reshap)

## function 
def greater(a,b):
    print("inside function")
    print(a)
    print(b)
    if a>b :
        return a
    else:
        return b
t = greater(np.array(3),np.array(4)) 
print("d") 
print(t)  