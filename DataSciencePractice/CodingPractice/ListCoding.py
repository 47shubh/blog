# Creating an  empty list
list = []
print("Initial list", list)

# Creating a list using integers
list_int = [1, 2, 3, 4]
print("Integer list", list_int)

# Creating a list using characters
list_char = ['a', 'c', 'd', 'g']
print("Character list", list_char)

# Creating a list using strings
list_str = ['srinivas', 'dasu']
print("String list", list_str)

# Creating a list with mixed data types
list_mix = [1, "hello", 2.5]
print("Mixed list", list_mix)

# Creating a multidimensional list or nested list
list_md = ["hello", [1, 2, 3], ["c"]]
print("Multidimensional list", list_md)

# Accessing elements from a list
# to print first element of a list
print(list_str[0])

# To print last element of a list
print(list_char[-1])

# Nested List
n_list = ["Happy", [2, 0, 1, 5]]

# Nested indexing

# Output: a
print(n_list[0][1])

# Output: 5
print(n_list[1][3])

# Slicing lists
my_list = ['s', 'r', 'i', 'n', 'i', 'v', 'a', 's']
# elements 3rd to 5th
print(my_list[2:5])

# elements beginning to 4th
print(my_list[:-5])

# elements 6th to end
print(my_list[5:])

# elements beginning to end
print(my_list[:])

# Add/Modify elements to a list
# Add 'd' to my_list at 0th index
my_list[0] = 'd'
print(my_list)

# Change elements from 2-4th elements in my_list
my_list[1:4] = ['a', 's', 'u']
print(my_list)

# Adding elements using append() method
my_list.append('c')  # Adds element at end of the list
print(my_list)

# Adding multiple elements at a time using extend() method
my_list.extend(['h', 'i', 'n', 'n', 'u'])
print(my_list)

# Adding a element at particular location using insert() method
even = [2, 4, 6]
# insert 8 at 1st index
even.insert(1, 8)
print(even)

# Combining two list using '+' called concatenation
odd = [1, 3, 5, 7]
odd1 = [9, 11, 13]
print(odd + odd1)  # prints combined list of odd and odd1

# Delete or remove elements from a list
# delete element at 2nd index
del odd[2]
print(odd)

# delete multiple elements at a time
del odd1[1:]  # It will delete elements from 1st index to end of list
print(odd1)

# Delete entire list
del odd1

# Uncomment print statement if you want to see error

# print(odd1)    # It throws an error as odd1 is not defined

# Delete element from a list using remove() method
odd.remove(1)
print(odd)

# Delete element at a given index using pop() method
odd.pop(0)  # By default pop() will remove last element from the list
print(odd)

# Some examples on list using list methods
my_list = [3, 8, 1, 6, 0, 8, 4]

print(my_list.index(8))

# Output: 1
print(my_list.count(8))

# Output: 2
my_list.sort()  # sorts the list

print(my_list)

# Output: [0, 1, 3, 4, 6, 8, 8]

my_list.reverse()  # Reverse the list

print(my_list)
# Output: [8, 8, 6, 4, 3, 1, 0]

# List Comprehension

# Example of creation of a list using elements ( power of 2's up to 10)
pow_list = [2 ** x for x in range(10)]

print(pow_list)


# Program to print even numbers in a list

# Using For loop

def even_for(input_list):
    for num in input_list:
        if num % 2 == 0:
            print(num, end=" ")


# Using While loop

def even_while(input_list):
    size = 0
    while size < len(input_list):
        if size % 2 == 0:
            print(size, end=" ")
        size += 1


# Using list Comprehension


def even_listcomp(input_list):
    even_nums = [x for x in input_list if x % 2 == 0]
    print(even_nums)


# Using lambda expressions


def even_lambda(input_list):
    even = filter(lambda x: (x % 2 == 0), input_list)
    for num in even:
        print(num, end=' ')


input_l=[1, 2, 3, 5, 7, 4, 6, 9, 11, 23, 12]

even_listcomp(input_l)
