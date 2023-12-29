#!/usr/bin/env python
import json
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle


def draw_tree(d, draw_level, color, level):
    bb = d['bb']
    if level == draw_level:
        ax.add_patch(Rectangle((bb[0], bb[1]),
                               (bb[2] - bb[0]), (bb[3] - bb[1]), fill=False, edgecolor=color, lw=25 - 5 * level))
    if 'children' in d:
        for child in d['children']:
            draw_tree(child, draw_level, color, level + 1)


with open('output.json') as f:
    d = json.load(f)

fig, ax = plt.subplots()
ax.plot([0, 0], [0, 0])
draw_tree(d, 0, 'black', 0)
draw_tree(d, 1, 'red', 0)
draw_tree(d, 2, 'blue', 0)
draw_tree(d, 3, 'pink', 0)
draw_tree(d, 4, 'purple', 0)
plt.axis("square")
plt.grid(True)
plt.show()
