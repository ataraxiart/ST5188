# script to create animation gif of LST over time

import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.patches as mpatches
import numpy as np
import os
import imageio

def create_lst_animation(df, subzone, fps=5):

    """
    Creates an animation of LST data from a dataframe with a date column.

    Args:
        df: Dataframe to be used.
        subzone (str): Name of subzone region we are interested in.
        fps (int): Frames per second for the animation.
    """

    # construct the dynamic GIF file name
    output_dir = "../Data/Misc/gif/"
    gif_name = f"LST_animation_{subzone}.gif"
    output_path = os.path.join(output_dir, gif_name)

    image_files = []
    date_steps = df["period"].unique()

    for i, date_step in enumerate(date_steps):
        try:
            date_df = df[df["period"] == date_step]
            lon = date_df['x'].values # use 'longitude' column
            lat = date_df['y'].values # use 'latitude' column
            lst = date_df['avg_LST'].values # use 'avg LST' column
            
            # custom color mapping with shades
            bins = [0, 20, 25, np.inf]  # temperature ranges
            blue_cmap = LinearSegmentedColormap.from_list("blue_shades", ["#ADD8E6", "#00008B"])
            yellow_cmap = LinearSegmentedColormap.from_list("yellow_shades", ["#FFD700", "#FFA500"])
            red_cmap = LinearSegmentedColormap.from_list("red_shades", ["#FA8072", "#8B0000"])

            # normalize LST data to bins
            lst_normalized = np.digitize(lst, bins) - 1

            plt.figure(figsize=(8, 6))

            # apply colormap based on bins
            for j in range(len(lon)):
                if lst_normalized[j] == 0:
                    plt.scatter(lon[j], lat[j], c=blue_cmap(lst[j]/25), s=20) # normalize lst for cmap
                elif lst_normalized[j] == 1:
                    plt.scatter(lon[j], lat[j], c=yellow_cmap((lst[j]-25)/5), s=20) # normalize lst for cmap
                else:
                    plt.scatter(lon[j], lat[j], c=red_cmap((lst[j]-30)/(max(lst)-30)), s=20) # normalize lst for cmap

            # create custom colorbar (with text labels)
            # create legend patches
            blue_patch = mpatches.Patch(color=blue_cmap(0.5), label='<20')
            yellow_patch = mpatches.Patch(color=yellow_cmap(0.5), label='20-25')
            red_patch = mpatches.Patch(color=red_cmap(0.5), label='>25')

            # display legend
            plt.legend(handles=[blue_patch, yellow_patch, red_patch])

            plt.title(f'LST on {date_step}')
            plt.xlabel('Longitude')
            plt.ylabel('Latitude')

            image_file = f'frame_{i+1:03d}.png'
            plt.savefig(image_file)
            plt.close()

            image_files.append(image_file)

        except Exception as e:
            print(f"Error processing date {date_step}: {e}")
            continue

    if image_files:
        # create the GIF
        images = [imageio.imread(file) for file in image_files]
        imageio.mimsave(output_path, images, fps=fps)
        print(f"Animation saved to {output_path}")
        
        # clean up temporary image files
        for file in image_files:
            os.remove(file)

        return output_path 

    else:
        print("No images to create animation.")
