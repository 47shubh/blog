from __future__ import print_function
import torch
import numpy as np

# Creation of empty matrix in pytorch
x = torch.empty(5, 3) # where 5 is no_of_rows and 3 is no_of_columns
print(x)

# Creation of random matrix in pytorch
x = torch.rand(5, 3)
print(x)

# Creation of matrix with zeros and of data type long
x = torch.zeros(5, 3, dtype=torch.long)
print(x)

# Creation of matrix with zeros and of data type int
x = torch.zeros(5, 3, dtype=torch.int)
print(x)

# Creation of tensor from data , tensor is like narray's in numpy
x = torch.tensor([5.5, 3])
print(x)

# Creation of tensor based on existing tensor
x = x.new_ones(5, 3, dtype=torch.double)      # new_* methods take in sizes
print(x)

x = torch.randn_like(x, dtype=torch.float)    # override dtype!
print(x)                                      # result has the same size

print(x.size())                               # To get tensor size and gives output in tuple format

# Operations on tensors
# Add operation
y = torch.rand(5, 3)
print(x + y)                                    # Addition operation on two tensors

# Another way of add operation
print(torch.add(x, y))

# Another way of add operation  - providing an output tensor as argument
result = torch.empty(5, 3)
torch.add(x, y, out=result)
print(result)

# adds x to y - inplace addition
y.add_(x)
print(y)

# Printing using indexing
print(x[:, 1])

# Resizing/Reshaping tensor using tensor.view()
x = torch.randn(4, 4)
y = x.view(16)
z = x.view(-1, 8)  # the size -1 is inferred from other dimensions
print(x.size(), y.size(), z.size())

# If tensor has only 1 element we can access it as a python number as below using item()
x = torch.randn(1)
print(x)
print(x.item())


# Converting pytorch tensors to numpy arrays
a = torch.ones(5)
print(a)

b = a.numpy()                               # converting tensor a to numpy using numpy()
print(b)

a.add_(1)
print(a)
print(b)

# Converting numpy array to pytorch tensor
a = np.ones(5)
b = torch.from_numpy(a)                     # Converting numpy array to tensor using from_numpy(array_name)
np.add(a, 1, out=a)
print(a)
print(b)

