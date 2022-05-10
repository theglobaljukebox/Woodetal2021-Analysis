import csv
import pandas as pd
import json
import numpy as np
import math

f = open ('./data/conversion_guide.json', "r")
conversion_guide = json.loads(f.read())

def get_code(line, raw_encoding):
    for i in range(len(conversion_guide[line])):
        if(raw_encoding==conversion_guide[line][i]['code']):
            display_code = conversion_guide[line][i]['display_code']
            return display_code

def convert_modal_profiles():
    df = pd.read_csv('./output/modal_profiles.csv', index_col=0)
    
    for i in range(len(df)):
        for j in range(1,38):
            col_name = 'line_'+str(j)
            code = df.loc[[i],[col_name]].values[0][0]
            df.loc[[i],[col_name]] = get_code(col_name,code)
    df.to_csv('./output/converted_modal_profiles.csv',index=False)


if __name__ == '__main__':
    convert_modal_profiles()
