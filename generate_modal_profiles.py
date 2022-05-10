import csv
import pandas as pd
import json
import numpy as np
from collections import *
import math
from statistics import mode
from progress.bar import Bar

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
    
    print("Generating Modal Profiles")
    downloaded_data = pd.read_csv('./data/downloaded_data.csv')
    
    df = downloaded_data.drop(columns=['orv_1', 'orv_2', 'ensemble_value_id',
        'ensemble_value_label', 'instrument_value_id',
        'instrument_value_label','Unnamed: 0'])

    feature_cols = ["line_"+str(i+1) for i in range(37)]
    grouped = df.groupby('society_id')
    bar = Bar('Processing', max=(len(grouped)))
    final_cols = ['soc_id', 'lat','lng', 'Preferred_name','line_1', 'line_2', 'line_3',
        'line_4', 'line_5', 'line_6', 'line_7', 'line_8', 'line_9', 'line_10',
        'line_11', 'line_12', 'line_13', 'line_14', 'line_15', 'line_16',
        'line_17', 'line_18', 'line_19', 'line_20', 'line_21', 'line_22',
        'line_23', 'line_24', 'line_25', 'line_26', 'line_27', 'line_28',
        'line_29', 'line_30', 'line_31', 'line_32', 'line_33', 'line_34',
        'line_35', 'line_36', 'line_37']
        
    final_df = pd.DataFrame(columns=final_cols)
    i = 0
    for name, group in grouped:
        i+=1
        modes = {}
        for line in feature_cols:
            line_encodings = []
            for item in group[line]:
                codes = []
                codes = encode(line, item)
                try:
                    line_encodings.extend(codes)
                except:
                    print("ERROR: on Cantometrics line %s in item %s in society %s" %(line, item, name))
                try:
                    line_mode = pow(2, mode(line_encodings))
                except:
                    line_mode = 'NaN'
            modes[line] = line_mode
        modes["soc_id"] = name
        modes["Preferred_name"] = mode(group["Preferred_name"])
        final_df = final_df.append(modes, ignore_index=True)
        bar.next()
    bar.finish()
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
    print("Saving Data")
    final_df.to_csv('./output/modal_profiles.csv')
    print("Modal Profile saved in output/modal_profiles.csv")
    print("Creating converted modal profiles")
    convert_modal_profiles()
    print("Modal Profile saved in output/converted_modal_profiles.csv")

def get_code(line, raw_encoding):
    for i in range(len(conversion_guide[line])):
        if(raw_encoding==conversion_guide[line][i]['code']):
            display_code = conversion_guide[line][i]['display_code']
            return display_code

def convert_modal_profiles():
    df = pd.read_csv('./output/modal_profiles.csv', index_col=0)
    f = open ('./data/conversion_guide.json', "r")
    conversion_guide = json.loads(f.read())
    for i in range(len(df)):
        for j in range(1,38):
            col_name = 'line_'+str(j)
            code = df.loc[[i],[col_name]].values[0][0]
            df.loc[[i],[col_name]] = get_code(col_name,code)
    df.to_csv(',/output/converted_modal_profiles.csv',index=False)

if __name__ == '__main__':
    get_single_modal_profiles()
