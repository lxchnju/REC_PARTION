import numpy as np
import matplotlib.pyplot as plt

# 读取数据文件
with open('rec_cords.txt', 'r') as f:
    data = f.read()

# 按分隔符划分数据
sections = data.split('------------------------------')
x_grid = np.fromstring(sections[0].splitlines()[0], sep=' ')
y_grid = np.fromstring(sections[0].splitlines()[1], sep=' ')
data_points = np.fromstring(sections[1], sep=' ')

# 绘制图形
fig, ax = plt.subplots()

# 绘制第一段数据的网格线
for i in range(len(x_grid)):
    ax.axvline(x_grid[i], color='black', alpha=0.5, linestyle='-')
for i in range(len(y_grid)):
    ax.axhline(y_grid[i], color='black', alpha=0.5, linestyle='-')

# 绘制第二段数据的散点
ax.scatter(data_points[::2], data_points[1::2], s=10)

# 在第一段数据的网格线划分的小区域的左下角，标出该区域内的散点个数
for i in range(len(x_grid) - 1):
    for j in range(len(y_grid) - 1):
        x0, x1 = x_grid[i], x_grid[i + 1]
        y0, y1 = y_grid[j], y_grid[j + 1]
        points_in_region = ((data_points[::2] >= x0) & (data_points[::2] <= x1)
                            & (data_points[1::2] >= y0) &
                            (data_points[1::2] <= y1))
        ax.text(x0, y0, f'{np.sum(points_in_region)}', ha='left', va='bottom')

# 显示图形
plt.show()
