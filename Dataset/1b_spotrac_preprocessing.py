import pandas as pd

# Import scraped contracts
df = pd.read_csv('Source Datasets/spotrac_contracts.csv')
df['signed_using'] = df['signed_using'].fillna('Standard') # Fill empty values

# Dictionary to encode contract types, we have to reduce the number of options
contracts = {'Entry Level/Rookie':'Rookie Contract', '/Minimum':'Minimum', 'Entry Level':'Rookie Contract', '/Cap Space':'Standard',
    '/Bird':'Standard/Bird', 'Camp Invite/Minimum':'Minimum', 'Ten Day/Minimum':'Minimum', 'Retained':'Team Option', 
    'Entry Level/Minimum':'Rookie Contract', 'Entry Level/Cap Space':'Rookie Contract', '/MLE':'MLE', 'Exhibit 10/Minimum':'Minimum',
    'Rookie Extension/Bird':'Standard/Bird', 'Extension':'Standard', 'Two Way':'Minimum', 'Entry Level/MLE':'Rookie Contract',
    'Free Agent':'Standard', 'Free Agent/Minimum':'Minimum', 'Free Agent/Bird':'Standard/Bird', 'Rest Of Season/Minimum':'Minimum',
    'Veteran Extension':'Standard', 'Rookie Extension':'Standard', 'Free Agent/Cap Space':'Standard', 'Veteran Extension/Bird':'Standard/Bird',
    'Rookie Maximum Extension/Bird':'Maximum/Bird', '/Early Bird':'Standard/Bird', 'Ten Day':'Minimum', '/Mini MLE':'MLE', '/':'Standard',
    'Free Agent/MLE':'MLE', 'Two Way/':'Minimum', 'Sign And Trade/Bird':'Standard/Bird', 'Maximum/Bird':'Maximum/Bird', '/Room':'Standard',
    '/Non Bird':'Standard', 'Free Agent/Early Bird':'Standard/Bird', 'Rest Of Season/MLE':'MLE', 'Sign And Trade':'Standard', 
    'Maximum Extension/Bird':'Maximum/Bird', 'Maximum/Cap Space':'Maximum', 'Buyout':'Standard', 'Designated Player Veteran Extension/Bird':'Super-Maximum/Bird',
    '/Bi Annual':'Standard', 'Free Agent/Sign And Trade':'Standard', 'Free Agent/Room':'Standard', 'Amnesty':'Standard', 'Free Agent/':'Standard',
    'Sign And Trade/Cap Space':'Standard', 'Extension/':'Standard', 'Entry Level/Mini MLE':'Rookie Contract', 'Free Agent/Non Bird':'Standard',
    'Earned':'Standard', 'Sign And Trade/Sign And Trade':'Standard', 'Free Agent/Bi Annual':'Standard', 'Free Agent/Mini MLE':'MLE',
    'Extension/Bird': 'Standard/Bird', 'Maximum/Sign And Trade':'Maximum', 'Sign And Trade/Non Bird':'Standard', 'Camp Invite':'Minimum',
    'Sign And Trade/Early Bird':'Standard/Bird', 'Maximum Extension/Cap Space':'Maximum', 'Exhibit 9/Minimum':'Minimum', 'Veteran Extension/Cap Space':'Standard',
    'Rookie Extension/Cap Space':'Standard', 'Rest Of Season/Mini MLE':'MLE', 'Maximum/Early Bird':'Maximum/Bird', '/DPE':'Standard',
    'Qualifying Offer':'Standard', 'Two Way/Minimum':'Minimum', 'Entry Level/Bird':'Standard/Bird', 'Veteran Extension/Early Bird':'Standard/Bird',
    'Ten Day/':'Minimum', 'Earned/Minimum':'Minimum', 'Maximum':'Maximum', 'Extend And Trade/Bird':'Standard/Bird', 'Entry Level/Bi Annual':'Rookie Contract',
    '/Rookie':'Rookie Contract', 'Rookie Extension/Early Bird':'Standard/Bird', 'Entry Level/Room':'Rookie Contract', 'Entry Level/':'Rookie Contract',
    'Rest Of Season':'Standard', 'Rest Of Season/Cap Space':'Standard', 'Dead Include/':'Standard', 'Extend And Trade/':'Standard',
    'Exhibit 10/Mini MLE':'MLE', 'Maximum/Non Bird':'Maximum', 'Rest Of Season/Bi Annual':'Standard', 'Exhibit 10':'Standard', 'Standard':'Standard'}


# Let's create the columns for our new variables
df.insert(5, 'contract_type', '')     # "Simplified" contract type
df.insert(6, 'rookie_contract', 0)    # Rookie contract dummy
df.insert(7, 'bird_rights', 0)        # Bird rights dummy
df.insert(8, 'maximum_contract', 0)   # Maximum contract dummy
df.insert(9, 'super_max_contract', 0) # Super-maximum contract dummy

birds = ['Standard/Bird', 'Maximum/Bird', 'Super-Maximum/Bird'] # Contract signed using bird rights
maximums = ['Maximum/Bird', 'Maximum', 'Super-Maximum/Bird']    # Maximum contracts

# Let's create our new variables
for i, row in df.iterrows():
    df.iat[i, 5] = contracts[df.iat[i, 4]]
    ctype = df.iat[i, 5]
    if ctype == 'Rookie Contract':
        df.iat[i, 6] = 1
    if ctype in birds:
        df.iat[i, 7] = 1
    if ctype in maximums:
        df.iat[i, 8] = 1
    if ctype == 'Super-Maximum/Bird':
        df.iat[i, 9] = 1

# Save to csv
df.to_csv('Source Datasets/spotrac_contracts_encoded.csv', index=False)

