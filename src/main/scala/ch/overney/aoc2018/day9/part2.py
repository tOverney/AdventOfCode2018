import math
import collections
import re

p_count = 418
last_marble_id = 71339 * 100

curr_player = 1
circle = collections.deque()
scores = collections.defaultdict(int)

for marble_id in range(0, last_marble_id + 1):
  if (marble_id % 23) == 0 and marble_id is not 0:
    circle.rotate(-7)
    scores[curr_player] += (marble_id + circle.pop())
  else:
    circle.rotate(2)
    circle.append(marble_id)
  curr_player += 1
  if curr_player > p_count:
    curr_player = 1

print(scores[max(scores, key = scores.get)])
