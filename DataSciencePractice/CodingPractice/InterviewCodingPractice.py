import math;


def printPowerSet(set, set_size):
    # set_size of power set of a set
    # with set_size n is (2**n -1)
    pow_set_size = (int)(math.pow(2, set_size))
    counter = 0;
    j = 0;
    count = 0

    # Run from counter 000..0 to 111..1
    for counter in range(0, pow_set_size):
        for j in range(0, set_size):

            # Check if jth bit in the
            # counter is set If set then
            # print jth element from set
            if ((counter & (1 << j)) > 0):
                print(set[j], end="")
              #  print("count : ",sum(set[j]))

        print("");

    # Driver program to CodingPractice printPowerSet



# set = ['a', 'b', 'c'];
set = [1, 2, 3, 4, 5];

v=printPowerSet(set, 5)

t = sum(v)

print(t)


