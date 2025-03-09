# script to create animation gif of LST over time

import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
import matplotlib.patches as mpatches
import numpy as np
import os
import imageio
import geopandas as gpd
import pandas as pd


def create_na_animation(df, subzone, value_df, fps=5):

    """
    Creates an animation of LST data from a dataframe with a date column.

    Args:
        df: Dataframe to be used.
        subzone (str): Name of subzone region we are interested in.
        fps (int): Frames per second for the animation.
    """
    
    sg_shapes = gpd.read_file("../Data/Misc/Subzone/MP14_SUBZONE_NO_SEA_PL.shp")

    if sg_shapes.crs != "EPSG:4326":
        sg_shapes = sg_shapes.to_crs("EPSG:4326")

    sg_boundary = sg_shapes.dissolve()
    
    # construct the dynamic GIF file name
    output_dir = "../Data/Misc/gif/"
    gif_name = f"NA_animation_{subzone}.gif"
    output_path = os.path.join(output_dir, gif_name)

    image_files = []
    date_steps = df["date"].unique()

    for i, date_step in enumerate(date_steps):
        try:
            date_df = df[df["date"] == date_step]
            lon = date_df['x'].values # use 'longitude' column
            lat = date_df['y'].values # use 'latitude' column
            na_df = date_df['missing'].values # use 'missing' column
          
            # get NA summary for this date
            value_row = value_df[(value_df["date"] == date_step)]
            if not value_row.empty:
              max_region = value_row["max_region"].values[0]
              
              # pivot to get region as column names and na_count as values
              na_counts_pivot = value_row.pivot(index="date", columns="region", values="na_count")

              # extract values
              east_count = na_counts_pivot.get("East", pd.Series([0])).values[0]
              west_count = na_counts_pivot.get("West", pd.Series([0])).values[0]
              north_count = na_counts_pivot.get("North", pd.Series([0])).values[0]
              south_count = na_counts_pivot.get("South", pd.Series([0])).values[0]
              central_count = na_counts_pivot.get("Central", pd.Series([0])).values[0]
              
            else:
              max_region = None
              east_count = 0
              west_count = 0
              north_count = 0
              south_count = 0
              central_count = 0

            plt.figure(figsize=(8, 6))
            
            sg_boundary.plot(ax=plt.gca(), color='lightgray', edgecolor='black', linewidth=1)


            plt.scatter(lon, lat, c=na_df, cmap=ListedColormap(['black', 'orange']), s=20)
            
            legend_patches = [
              mpatches.Patch(color="black", label="Not Missing"),
              mpatches.Patch(color="orange", label="Missing")
              ]

            plt.legend(handles=legend_patches, loc="upper right")
            

            # add text annotation for NA region results
            text_x, text_y = 0.02, 0.98  # position in figure (bottom-right)
            # annotate the text in bottom-right with a proper format
            plt.text(
                0.98, 0.02,
                f"Max NA Region: {max_region}\n"
                f"East: {east_count}\n"
                f"West: {west_count}\n"
                f"North: {north_count}\n"
                f"South: {south_count}\n"
                f"Central: {central_count}",
                fontsize=8, color="black",
                bbox=dict(facecolor="white", edgecolor="black", alpha=1),
                transform=plt.gca().transAxes,
                verticalalignment="bottom",
                horizontalalignment="right"
            )

            plt.title(f'Distribution of Missing Values on {date_step} in {subzone}')
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
