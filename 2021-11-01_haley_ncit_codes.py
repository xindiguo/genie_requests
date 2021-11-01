import synapseclient
import pandas as pd

syn = synapseclient.Synapse()
syn.login()

def _get_synid_dd(cohort, synid_table_prissmm = "syn22684834"):

    query = 'SELECT id FROM ' + synid_table_prissmm + ' WHERE cohort = \'' + cohort + '\' ORDER BY name DESC LIMIT 1'
    query_results = syn.tableQuery(query)

    synid_folder_prissmm = query_results.asDataFrame()['id'][0]

    synid_prissmm_children = syn.getChildren(synid_folder_prissmm)

    for child in synid_prissmm_children:
        if child['name'] == "Data Dictionary non-PHI":
            return child['id'] 
    return None

def get_drug_mapping(cohort, synid_file_grs = 'syn24184523'):

    mapping = {}
    var_names = []

    synid_file_dd = _get_synid_dd(cohort)

    dd = pd.read_csv(syn.get(synid_file_dd).path)
    grs = pd.read_csv(syn.get(synid_file_grs).path)
    grs.columns = ['Variable / Field Name', 'Choices, Calculations, OR Slider Labels']

    for i in ['1','2','3','4','5']:
        var_names.append('drugs_drug_' + i)
        var_names.append('drugs_drug_oth' + i)

    for obj in dd, grs:

        for var_name in var_names:
            
            if var_name in obj['Variable / Field Name'].unique():
                choice_str = obj[obj['Variable / Field Name'] == var_name]['Choices, Calculations, OR Slider Labels'].values[0]
                choice_str = choice_str.replace('"', '')
            
                for pair in choice_str.split('|'):
                    code = pair.split(',')[0].strip()
                    value = pair.split(',')[1].strip()
                    label = value.split('(')[0].strip()
                    mapping[label] = code
    return(mapping)

def get_regimen_abbrev(regimen, mapping):
    abbrev = ''
    drugs = regimen.split(',')
    for drug in drugs:
        if drug == drugs[0]:
            abbrev = mapping[drug.strip()]
        else:
            abbrev = abbrev + '_' + mapping[drug.strip()]
    return(abbrev)

cohort = 'Prostate'
regimen = 'ARV110, CC94676, Vecabrutinib'
mapping = get_drug_mapping(cohort)
abbrev = get_regimen_abbrev(regimen, mapping)

print(abbrev)
