# script to create animation gif of LST over time

import matplotlib.pyplot as plt
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
            
            plt.figure(figsize=(10, 6))
            plt.scatter(lon, lat, c=lst, cmap='viridis', s=20)
            plt.colorbar(label='Land Surface Temperature (°C)')
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
        imageio.mimsave(output_path, images, fps = fps)
        print(f"Animation saved to {output_path}")
        
        # clean up temporary image files
        for file in image_files:
            os.remove(file)

        return output_path 

    else:
        print("No images to create animation.")
