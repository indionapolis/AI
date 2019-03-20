f = open('mysolution2_0.pl', 'r')

f1 = open('tamplate.pl', 'w')

import re

patterns = [['room','position'],
['isExplorable', 'is_explorable'],
['trim_unvisited', 'remove_unvisited'],
['trimNotAdjacent', 'remove_unadjacent'],
['countList','list_len']]

text = f.read()
result = text
for pattern in patterns:
    result = re.sub(pattern[0], pattern[1],text)
    text = result
f1.write(result)
