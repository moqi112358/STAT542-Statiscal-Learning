import pandas as pd
import numpy as np
data = pd.read_csv('tmdb_5000_movies_step1.csv')

unchange_attr = ['budget','id','revenue','runtime','vote_average']
trans_data = data[unchange_attr]

abstract_attr = ['genres','keywords','production_companies','production_countries','spoken_languages']
#abstract_attr = ['genres','production_companies','production_countries','spoken_languages']

abstract_attr2 = ['original_language','status']

for attr in abstract_attr2:
    col = data[attr]
    value_set = set(col)
    new_matrix = []
    for line in col:
        value_hash = {i:0 for i in value_set}
        value_hash[line] = 1
        d = [value_hash[key] for key in sorted(value_hash.keys())]
        new_matrix.append(d)
    new_data = np.array(new_matrix)
    new_data_pd = pd.DataFrame(new_data, columns=[attr+'.'+i for i in sorted(value_hash.keys())])
    trans_data = trans_data.join(new_data_pd)

def stat_attr(X,attr):
    attr_set = []
    for i in X:
        line = eval(i)
        set = [j[attr] for j in line]
        attr_set.extend(set)
    #attr_set = set(attr_set)
    attr_set = {i:0 for i in attr_set}
    return attr_set.keys()

for attr in abstract_attr:
    col = data[attr]
    if attr == 'spoken_languages':
        value_set = stat_attr(col,'iso_639_1')
    else:
        value_set = stat_attr(col,'name')
    new_matrix = []
    for line in col:
        value_hash = {i:0 for i in value_set}
        line = eval(line)
        if attr == 'spoken_languages':
            set = [j['iso_639_1'] for j in line]
        else:
            set = [j['name'] for j in line]        
        for s in set:
            value_hash[s] = 1
        d = [value_hash[key] for key in sorted(value_hash.keys())]
        new_matrix.append(d)
    new_data = np.array(new_matrix)
    new_data_pd = pd.DataFrame(new_data, columns=[attr+'.'+i for i in sorted(value_hash.keys())])
    if attr != 'genres' and attr != 'production_countries':
        col_name = new_data_pd.columns
        for i in col_name:
            if sum(np.array(new_data_pd[i])) < 5:
                new_data_pd = new_data_pd.drop([i], axis=1)
    trans_data = trans_data.join(new_data_pd)

attr = 'release_date'
new_matrix = []
col = data[attr]
for line in col:
    line = line.split('/')
    d = [line[0], line[1]]
    new_matrix.append(d)

new_data = np.array(new_matrix)
new_data_pd = pd.DataFrame(new_data, columns=['Year','Month'])
trans_data = trans_data.join(new_data_pd)

new_matrix = []
for i in range(data.shape[0]):
    ratio = 1.0 * data.iloc[i,8] / data.iloc[i,0]
    new_matrix.append(ratio)

new_data = np.array(new_matrix)
new_data_pd = pd.DataFrame(new_data, columns=['revenue_budget_ratio'])
trans_data = trans_data.join(new_data_pd)

trans_data.to_csv('tmdb_5000_movies_step2_with_keyword.csv',index = False)