# Python program to reverse a string without stack 
# using extended slice syntax 

# 1 Function to reverse a string
def fisrtReverseMethod(string):
    string = string[::-1]
    return string


# 2 Python code to reverse a string  
# using loop 

def secondReverseMethod(string):
    str = ""
    for i in string:
        str = i + str
    return str


# 3 Python code to reverse a string
# using recursion 

def thirdReverseMethod(string):
    if len(string) == 0:
        return string
    else:
        return thirdReverseMethod(string[1:]) + string[0]

    # 4 Python code to reverse a string

# using reversed()


# Function to reverse a string
def forthReverseString(string):
    string = "".join(reversed(string))
    return string

str = input("Plase enter the string    ")
rev_str = forthReverseString(str)
print("Original String is : ", str, "   And Reverse String is  : ", rev_str)
