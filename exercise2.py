import re

# AMP Coding Exercies

# Candidate Kieren Jin
# Exercise 2: Matrix Decoding.

def decode():

    try:
        # User input N by M matrix size.
        try:
            n, m = list(map(int, input().split()))
        # Check n and m are both values.
        except ValueError:
            print("matrix row or column is not a number")
            return
        # Check constraints N > 0, M < 100
        if n <= 0 or m >= 100:
            print("matrix size out of bounds. please check N > 0 and M < 100")
            return

        rows = []
        # append map elements into row list.
        for i in range(n):
            rows.append(input())
        # Use double loop list comprehension to combine sub lists.
        combined = [row[i] for i in range(m) for row in rows]
        # join letters in list
        text = "".join(combined)

        # replace non alphanumeric characters.
        text = re.sub('([A-Za-z1-9])[^A-Za-z1-9]+([A-Za-z1-9])', r'\1 \2', text)
        text = re.sub('  ', ' ', text)
        print(text)
    # handle exception
    except TypeError as e:
        print("type error" + e)
    except Exception as e:
        print("encoding error" + e)


decode()
