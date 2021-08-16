import csv
import pandas as pd
import json
import numpy as np
from collections import *
import math
from statistics import mode

def get_basic_modal_profiles():
    canto_data = pd.read_csv('./data/full_cantometrics.csv').drop(columns=['Unnamed: 0'])
    # canto_data = pd.read_csv('./output/full_cantometrics.csv')
    features = ['line_'+str(i+1)for i in range(37)]
    cultures = canto_data['Preferred_name'].unique()
    columns = ['Preferred_name', 'soc_id']
    for feature in features:
        columns.append(feature)
        
    final_df = pd.DataFrame(columns = columns)
    for culture in cultures:
        modal_profile = dict(canto_data[canto_data['Preferred_name']==culture][features].mode().loc[0])
        modal_profile['Preferred_name'] = culture
        soc_id = canto_data[canto_data['Preferred_name']==culture]['society_id'].reset_index()['society_id'][0]
        modal_profile['soc_id'] = soc_id
        final_df = final_df.append(modal_profile, ignore_index=True)
    final_df.to_csv('./output/basic_modal_profiles.csv')

def encode(line, binary):
    with open('./data/conversion_guide.json') as f:
        conversion_guide = json.load(f)

    for conversion in conversion_guide[line]:
        if conversion['code']==binary:
            codes = [conversion["original_1"], 
                     conversion["original_2"], 
                     conversion["original_3"]]
            return [code for code in codes if code]

def get_single_modal_profiles():
    downloaded_data = pd.read_csv('./data/downloaded_data.csv')
    
    df = downloaded_data.drop(columns=['orv_1', 'orv_2', 'ensemble_value_id',
        'ensemble_value_label', 'instrument_value_id',
        'instrument_value_label','Unnamed: 0'])

    feature_cols = ["line_"+str(i+1) for i in range(37)]
    grouped = df.groupby('society_id')
    
    final_cols = ['soc_id', 'lat','lng', 'Preferred_name','line_1', 'line_2', 'line_3',
        'line_4', 'line_5', 'line_6', 'line_7', 'line_8', 'line_9', 'line_10',
        'line_11', 'line_12', 'line_13', 'line_14', 'line_15', 'line_16',
        'line_17', 'line_18', 'line_19', 'line_20', 'line_21', 'line_22',
        'line_23', 'line_24', 'line_25', 'line_26', 'line_27', 'line_28',
        'line_29', 'line_30', 'line_31', 'line_32', 'line_33', 'line_34',
        'line_35', 'line_36', 'line_37']
        
    final_df = pd.DataFrame(columns=final_cols)
    for name, group in grouped:
        modes = {}
        for line in feature_cols:
            line_encodings = []
            for item in group[line]:
                print(item)
                codes = []
                codes = encode(line, item)
                line_encodings.extend(codes)
                try:
                    line_mode = pow(2, mode(line_encodings))
                except:
                    line_mode = 'NaN'
            modes[line] = line_mode
        modes["soc_id"] = name
        modes["Preferred_name"] = mode(group["Preferred_name"])
        final_df = final_df.append(modes, ignore_index=True)
    metadata = pd.read_csv('./data/metadata.csv')
    for i, row in final_df.iterrows():
        culture = row['Preferred_name']
        try:
            lat = metadata[metadata['Preferred_name']==culture].reset_index()['Local_latitude'][0]
            lng = metadata[metadata['Preferred_name']==culture].reset_index()['Local_longitude'][0]
        except:
            final_df.drop([i])
        final_df.loc[i, ['lat']] = lat
        final_df.loc[i, ['lng']] = lng
    final_df.to_csv('./output/modal_profiles.csv')
    
if __name__ == '__main__':
    get_basic_modal_profiles()
    get_single_modal_profiles()
