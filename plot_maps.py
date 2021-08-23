import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
import numpy as np
import matplotlib.font_manager as font_manager
import matplotlib.patches as mpatches
import json
# This might cause issues with your OS and installations of shapely, geos, and proj. 
# Try re-installing anaconda, and then previously mentioned dependencies. Then install cartopy
import cartopy.crs as ccrs
import cartopy.feature as cfeature

line_metadata =  pd.read_csv('./data/line_metadata.csv')
line_metadata = line_metadata.drop(columns='linenum')
# Load conversion guide
with open('./data/conversion_guide.json') as f:
  conversion_guide = json.load(f)

modal_profiles = pd.read_csv('./data/cantometrics_ethnographicatlas.csv', index_col=0)

def plot_pca_data(column_name, vname):

    musical_profiles = modal_profiles.dropna(subset=[column_name])

    ## Map parameters
    CENTRAL_LONGITUDE = 160.0
    font_family = "Roboto"
    alpha = 0.9
    color_map = {}
    variable_name = vname
    filename = './figs/'+column_name+'.png'

    ## Project to co-ordinates
    x,y = ([] for i in range(2))
    for i, row in musical_profiles['Society_latitude'].iteritems():
        y.append(float(row))
    for i, row in musical_profiles['Society_longitude'].iteritems():
        if float(row)+200 < 345:
            x.append(float(row)+200)
        else:
            x.append((float(row)+200)-345)
            
    fig = plt.figure(figsize=(25,10))
    projection=ccrs.PlateCarree(central_longitude=CENTRAL_LONGITUDE)
    ax = fig.add_subplot(1,1,1,projection=projection)
    ax.coastlines(alpha=.5, color="#B4B4B4")
    ax.set_global()
    ax.text(-175, -70, variable_name, fontsize=42, fontfamily=font_family, color="Black")


    scatter = ax.scatter(x,  y,
            c=musical_profiles[column_name],
            cmap = 'coolwarm', 
            alpha=alpha,
            marker='o',
            sizes=(150,150))

    ax.add_feature(cfeature.OCEAN, alpha=.6, color="#EFEFEF") 
    axins = inset_axes(ax,
                    width="2.5%",  
                    height="100%",
                    loc='right',
                    borderpad=-1
                   )
    cbar = plt.colorbar(scatter, cax=axins, orientation="vertical")
    for t in cbar.ax.get_yticklabels():
     t.set_fontsize(20)
    plt.rcParams['legend.title_fontsize'] = 'x-large'
    plt.rc('font',family=font_family, weight='regular')
    plt.savefig(filename, bbox_inches = 'tight',pad_inches = 0)

def plot_cantometrics_data(linenum,vname):
    ## Drop instances where there is no reading
    embellishment = ea[ea.line_23 != 0]

    ## Map parameters
    CENTRAL_LONGITUDE = 160.0
    font_family = "Roboto"
    alpha = 0.7

    ## output params
    line_title = 'Cantometrics Line '+str(linenum)
    column_name = 'line_'+str(linenum)
    color_name = 'line'+str(linenum)+'_col'
    color_map = {}
    variable_name = vname
    filename = './figs/'+vname.lower()+'_low.png'

    ## Project to co-ordinates
    x,y = ([] for i in range(2))
    for i, row in embellishment['Society_latitude'].iteritems():
        y.append(float(row))
    for i, row in embellishment['Society_longitude'].iteritems():
        if float(row)+200 < 345:
            x.append(float(row)+200)
        else:
            x.append((float(row)+200)-345)
            
    fig = plt.figure(figsize=(30,15))
    projection=ccrs.PlateCarree(central_longitude=CENTRAL_LONGITUDE)
    ax = fig.add_subplot(1,1,1,projection=projection)
    ax.coastlines(alpha=.5, color="#B4B4B4")
    ax.set_global()
    ax.text(-175, -70, variable_name, fontsize=42, fontfamily=font_family, color="Black")
    ax.text(-175, -78, line_title, fontsize=28, fontfamily=font_family, color="Black")

    colors = list(ea.line23_col.unique())
    order = [0, 4, 1, 3, 2]
    colors = [colors[i] for i in order]

    scatter = ax.scatter(x,  y,
            c=embellishment[color_name], 
            alpha=alpha,
            marker='o',
            sizes=(150,150))

    label_names = ["Extreme embelishment", "Much embelishment", "Medium or considerable embellishment", 
                "Slight embellishment", "Little or no embellishment"]
    label_names.reverse()
    patch_1 = mpatches.Circle((0,0), radius=50, fc=colors[0], label=label_names[0])
    patch_2 = mpatches.Circle((0,0), radius=50, fc=colors[1], label=label_names[1])
    patch_3 = mpatches.Circle((0,0), radius=50, fc=colors[2], label=label_names[2])
    patch_4 = mpatches.Circle((0,0), radius=50, fc=colors[3], label=label_names[3])
    patch_5 = mpatches.Circle((0,0), radius=50, fc=colors[4], label=label_names[4])

    plt.legend(handles=[patch_1, patch_2, patch_3, patch_4, patch_5],
                        loc="lower right", 
                        borderpad = 1,
                        framealpha = 1,
                        edgecolor ="none",
                        fontsize=15,
                        title='Embellishment')


    ax.add_feature(cfeature.OCEAN, alpha=.6, color="#EFEFEF") 

    plt.rcParams['legend.title_fontsize'] = 'x-large'
    plt.rc('font',family=font_family, weight='regular')

if __name__ == '__main__':
    plot_pca_data('musical_pc1', 'Musical PC1')
    plot_pca_data('social_pc1', 'Social PC1')
    # plot_cantometrics_data(23, "Embellishment")
    # plot_ea_data("Social Layering","stratification_col", "Social Factors")