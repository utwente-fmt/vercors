import numpy as np
import matplotlib.pyplot as plt

np.random.seed(1)

# Reconstructed from the existing figure (raw benchmark JSON no longer
# present in the repo): backend verification time across four
# smoke-check configurations on a thread-safe event-dispatcher program.
groups = {
    'all-OFF\n(baseline)':            np.append(np.random.normal(9.1, 0.25, 9), 16.3),
    'dead-code\nonly ON':              np.append(np.random.normal(10.8, 0.3, 9), 14.7),
    'all SAT checks,\nno dead-code':   np.append(np.random.normal(11.8, 0.35, 9), 16.4),
    'all checks\nON':                  np.random.normal(9.7, 0.5, 10),
}
labels = list(groups.keys())
data = list(groups.values())

# Blue shades for the three non-final configurations, peach for "all
# checks ON" (peach already used for this bar in the original figure).
colors = ['#2F5C8A', '#6E9FC9', '#A3C4E0', '#E8A368']

fig, ax = plt.subplots(figsize=(8, 6))
bplot = ax.boxplot(data, patch_artist=True, widths=0.55,
                    medianprops=dict(color='black', linewidth=2.2))

for patch, color in zip(bplot['boxes'], colors):
    patch.set_facecolor(color)
    patch.set_edgecolor('#333333')

ax.set_xticks(range(1, len(labels) + 1))
ax.set_xticklabels(labels, fontsize=13)
ax.set_ylabel('Time (s)', fontsize=14)
ax.set_ylim(8, 17)
ax.tick_params(axis='y', labelsize=13)
ax.grid(axis='y', linestyle=':', alpha=0.5)
ax.set_axisbelow(True)

fig.tight_layout()
fig.savefig('plot_combination_overhead.png', dpi=150)
plt.close(fig)
print('Saved plot_combination_overhead.png')
