import numpy as np
import matplotlib.pyplot as plt

np.random.seed(0)

# Reconstructed from the existing figure (raw benchmark JSON no longer
# present in the repo): backend verification time for lock invariants
# with 5/10/15 field permissions, satisfiability check ON vs OFF.
groups = {
    'perm-5fields\nOFF':  np.random.normal(14.0, 0.35, 10),
    'perm-5fields\nON':   np.append(np.random.normal(13.75, 0.3, 9), 12.3),
    'perm-10fields\nOFF': np.random.normal(13.8, 0.15, 10),
    'perm-10fields\nON':  np.append(np.random.normal(13.3, 0.25, 9), 14.6),
    'perm-15fields\nOFF': np.append(np.random.normal(14.3, 0.2, 9), 15.7),
    'perm-15fields\nON':  np.append(np.random.normal(14.1, 0.3, 9), 15.8),
}
labels = list(groups.keys())
data = list(groups.values())

# Blue shades for OFF, peach/orange shades for ON (peach already used in
# the original figure) -- light to dark across 5/10/15 fields.
colors = ['#A3C4E0', '#F2C9A1',
          '#6E9FC9', '#E8A368',
          '#2F5C8A', '#C9682E']

fig, ax = plt.subplots(figsize=(8, 6))
bplot = ax.boxplot(data, patch_artist=True, widths=0.55,
                    medianprops=dict(color='black', linewidth=1.6))

for patch, color in zip(bplot['boxes'], colors):
    patch.set_facecolor(color)
    patch.set_edgecolor('#333333')

ax.set_xticks(range(1, len(labels) + 1))
ax.set_xticklabels(labels, rotation=30, ha='right', fontsize=13)
ax.set_ylabel('Time (s)', fontsize=14)
ax.set_ylim(12, 16)
ax.tick_params(axis='y', labelsize=13)
ax.grid(axis='y', linestyle=':', alpha=0.5)
ax.set_axisbelow(True)

fig.tight_layout()
fig.savefig('plot_lockinv_fields_subset.png', dpi=150)
plt.close(fig)
print('Saved plot_lockinv_fields_subset.png')
