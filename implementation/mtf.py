# Python program to implement self-organizing list
# using move to front method

# class for self-organizing list node
class self_list:
    def __init__(self, value=None, next=None):
        self.value = value
        self.next = next


# head and rear pointing to start and end of list resp.
head = None
rear = None

# function to insert an element


def insert_self_list(number):
    global head, rear

    # creating a node
    temp = self_list(number)

    # first element of list
    if head is None:
        head = rear = temp

    # rest elements of list
    else:
        rear.next = temp
        rear = temp

# function to search the key in list
# and re-arrange self-organizing list


def search_self_list(key):
    global head

    # pointer to current node
    current = head

    # pointer to previous node
    prev = None

    # searching for the key
    while current is not None:

        # if key found
        if current.value == key:

            # if key is not the first element
            if prev is not None:

                # re-arranging the elements
                prev.next = current.next
                current.next = head
                head = current
            return True
        prev = current
        current = current.next

    # key not found
    return False

# function to display the list
# foldl


def display():
    global head

    if head is None:
        print("List is empty")
        return

    # temporary pointer pointing to head
    temp = head
    print("List: ", end="")

    # sequentially displaying nodes
    while temp is not None:
        print(temp.value, end="")
        if temp.next is not None:
            print(",", end="")

        # incrementing node pointer.
        temp = temp.next
    print("\n")


# Driver Code
if __name__ == "__main__":
    # inserting five values
    initialList = [0, 1, 2, 3, 4]

    for i in initialList:
        insert_self_list(i)

    requests = [0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4]

    for i in requests:
        search_self_list(i)
        display()
