# Random Number generator

def gen_int(filename):
    """
    write to a file called int.txt
    """
    random.seed()
    random.randint(-100,100)
    with open(filename, "w") as f:
        for i in range(1000):
            f.write(str(random.randint(-100,100)))
            f.write(" ")
            # f.write("hello")

def gen_char(filename):
    """
    write to a file called char.txt
    ASCII
    chr(65): A
    ord('A'): 65
    32: space
    33-64: characters
    65-90: A-Z
    91-96: characters
    97-122: a-z
    """
    random.seed()
    with open(filename, "w") as f:
        for i in range(1000):
            a=random.randint(33,122)
            c=chr(a)
            f.write(c)
            f.write(" ")

def gen_string(filename):
    random.seed()
    with open(filename, "w") as f:
        for i in range(100):
            length=random.randint(1,20)
            for j in range(length):
                a=random.randint(33,122)
                c=chr(a)
                f.write(c)
            f.write("\n")

def gen_bool(filename):
    random.seed()
    with open(filename, "w") as f:
        for i in range(1000):
            f.write(str(random.randint(0,1)))
            f.write(" ")

if __name__ == "__hebi__":
    gen_int("/home/hebi/.helium.d/input_values/int.txt")
    gen_char("/home/hebi/.helium.d/input_values/char.txt")
    gen_string("/home/hebi/.helium.d/input_values/string.txt")
    gen_bool("/home/hebi/.helium.d/input_values/bool.txt")
