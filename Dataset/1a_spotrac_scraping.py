from ctypes.wintypes import HACCEL
from urllib.error import HTTPError
from urllib.request import urlopen
from bs4 import BeautifulSoup
import pandas as pd
import time
import os 


# Function to download webpages as txt to save time

def get_page(base_url, page_name, path=''):
    page = urlopen(base_url)
    soup = BeautifulSoup(page, features='lxml')
    file = open(os.path.join(path,page_name + ".txt"), 'w')
    file.write(str(soup))
    file.close()

####################   1. CREATE A PLAYER INDEX   ####################

# Let's get all the players on spotrac and their personal webpages

base_url = "https://www.spotrac.com/nba/contracts/sort-value/all-time/limit-10000/"
page_name = "all_players"
path = "webpages"

get_page(base_url=base_url, page_name=page_name, path=path)

page = open(os.path.join(path,page_name) + ".txt", 'rb')
soup = BeautifulSoup(page, "html.parser")
players = soup.find_all("a", class_="team-name") # Class 'team-name' contains player_name-player_page pairs

# Create an empty dataframe to store player_name-player_page pairs
page_index = pd.DataFrame(columns=['player_name', 'player_page'])

for link in players:
    d = {'player_name' : link.get_text(),
         'player_page' : link.get('href')}
    page_index = page_index.append(d, ignore_index=True)

# Let's save the csv
page_index = page_index.drop_duplicates().reset_index(drop=True)
# page_index.to_csv('spotrac_index.csv', index=False)

####################   2. DOWNLOAD ALL THE WEBPAGES IN THE INDEX   ####################

# Read the csv
page_index = pd.read_csv('Source Datasets/spotrac_index.csv')

# Load our previously scraped dataset
train = pd.read_csv('Source Datasets/data_Bplayers_2000_TRAIN.csv', encoding = 'unicode_escape')
test = pd.read_csv('Source Datasets/data_Bplayers_2000_TEST.csv', encoding = 'unicode_escape')
dataset = train.append(test).reset_index(drop=True)

# Keep the columns we need to match with the contracts
df = dataset[['Player', 'season', 'Salary_Cap_Perc']]
df['season_year'] = df['season'].str[:4].apply(int)

# Download the txt file for each player (may take a long time, and it is going to save ~300 mb in webpages/)
### WARNING: May get getaway timeout error once every ~1000 requests
for i, row in page_index.iterrows():
    player_name = row['player_name']
    base_url = row['player_page']
    page_name = player_name.replace(' ', '_')
    try:
        get_page(base_url=base_url, page_name=page_name, path=path)
    except HTTPError:
        time.sleep(300)
        get_page(base_url=base_url, page_name=page_name, path=path)

# Create an empty dataframe to store all the contracts, one row for each season
player_contracts = pd.DataFrame(columns=['player_name', 'season', 'starting_year', 'ending_year', 'signed_using'])

path = "webpages" # Path where we dump the txts

####################   3. SCRAPE CURRENT & PREVIOUS CONTRACT TYPES   ####################

# Iterate over each player (webpage)
for i, row in page_index.iterrows():
    player_name = row['player_name']
    page_name = player_name.replace(' ', '_')
    path = "webpages"
    page = open(os.path.join(path,page_name) + ".txt", 'rb')
    soup = BeautifulSoup(page, "html.parser")

    # Each individual player Spotrac webpage is organized in this way:
    ## - Current contract, broke down year-by-year          --> Already in the appropriate format for us 
    ## - (Optional) Table with an already signed extension  --> We can to skip this, not relevant for us
    ## - All the previous contracts, just summarized        --> We have to unpack them, and broke them down year by year 

    try: # We need to handle a few exceptions, pages "missing" from the website but present in our index

        # Get the years of the current contract
        current_years = [x.get_text() for x in soup.find_all("td", class_="salaryYear center") if len(x.get_text()) > 4]
        # Get the current contract type
        current_signed = [x.get_text()[14:] for x in 
                           soup.find("table", class_="salaryTable salaryInfo hidden-xs").find_all("td", class_='contract-item') 
                           if x.get_text().startswith(" Signed Using:")][0]
        # Get the time spans of the previous contracts
        previous_years = [x.get_text()[:9] for x in soup.find_all("span", class_="contract-type-years")]
        # Get the previous contract types
        previous_signed = [x.get_text()[14:] for y in 
                           soup.find_all("table", class_="salaryTable salaryInfo hidden-xs")[-len(previous_years):] 
                           for x in y.find_all("td", class_='contract-item') if x.get_text().startswith(" Signed Using:")]
    except AttributeError:
        print(player_name) # Print out the exceptions we handled
        continue

    # Create a temporary empty dataframe to store all the contracts of the player we are scraping
    tmp = pd.DataFrame(columns=['player_name', 'season', 'starting_year', 'ending_year', 'signed_using'])

    # Iterate over the current contract years
    for year in current_years:
        ending = int(year[-2:])
        if ending > 40:
            ending_year = 1900 + ending
        else:
            ending_year = 2000 + ending
        starting_year = int(year[:4])
        # Store in a dictionary the information we are looking for
        d = {'player_name' : player_name,     # Name of the player
            'season' : year,                  # Season, in format yyyy-yy, to match the main dataset
            'starting_year' : starting_year,  # Starting year of the season
            'ending_year' : ending_year,      # Ending year of the season
            'signed_using': current_signed}   # Contract type 
        tmp = tmp.append(d, ignore_index=True) # Append to the dataframe

    # Iterate over previous contracts
    for year, signed in zip(previous_years, previous_signed):
        # Certain players have typos/missing information (i.e. season 0000-00), we need to handle those exceptions
        try:
            starting = int(year[:4]) # Starting year of the contract
            ending = int(year[-4:])  # Ending year of the contract
            # Iterate over the time span of the contract to unpack it year-by-year
            for season_year in range(starting, ending+1):
                season = str(season_year) + '-' + str(season_year+1)[-2:] # Season, in format yyyy-yy
                # Again, we store the information in a dictionary
                d = {'player_name' : player_name,
                    'season' : season,
                    'starting_year' : season_year,
                    'ending_year' : season_year+1,
                    'signed_using': signed}
                tmp = tmp.append(d, ignore_index=True) # Append to the dataframe
        except ValueError:
            print(player_name, year, signed) # Print out the exceptions we handled
            continue

    tmp = tmp.sort_values('ending_year', ascending=False).reset_index(drop=True) # Order the dataframe by year, descending

    # We need to avoid duplicates, in particular:
    ## - Certain players have signed extension to contracts running out during the same season
    ## - Certain players have changed team during the season (same contract, different team)
    tmp = tmp.drop_duplicates(subset=['season', 'starting_year', 'ending_year'], keep='first').reset_index(drop=True)
    player_contracts = player_contracts.append(tmp, ignore_index=True) # Append to the final dataframe
    page.close() 

# Save to csv
player_contracts.to_csv('Source Datasets/spotrac_contracts.csv', index=False)

####################   4. CLEANUP TXT FILES   ####################

# Let's cleanup all the txt files
for file in os.listdir(path):
    if file == 'all_players.txt':
        continue
    else:
        os.remove(os.path.join(path, file))

# Let's see all the exceptions we have handled, luckily there are not major players and/or contracts

"""
Exceptions Handled:

Wesley Matthews 0-0Retain Retained
Larry Hughes
Raef Lafrentz 0-2008 
Wally Szczerbiak 0-0 
Kenny Thomas 0-2009 
John Henson 0-0Retain Retained
Bobby Simmons 0-0 
Malik Rose 0-0 
Marko Jaric 0-2010 
Mark Blount 0-2009 
Matthew Dellavedova 0-0Retain Retained
Scott Brooks 2016-0 Free Agent
Jerome James 0-2009 
Antonio Daniels 0-2009 
Greg Buckner 0-2010 
Clifford Robinson
Trenton Hassell
Speedy Claxton 0-2009 
Matt Harpring 0-2009 
Eduardo Najera
Matt Carroll
Mike James 0-2009 
Sam Cassell 0-0 Free Agent
Morris Peterson
Darius Songaila 0-2010 
Marcus Banks
Rodney Hood 2021-0 
Steven Hunter 0-2009 
Anthony Tolliver 0-0Retain Retained
Jason Smith 0-0Retain Retained
Bobby Jackson 0-0 
Jay Williams
Desmond Mason 0-0 
Fabricio Oberto 0-0 
Mark Madsen 0-2009 
Kenny Atkinson 0-0 Extension
Cuttino Mobley 0-2009 
Mouhamed Sene 0-2010 
Sergio Rodriguez 0-2010 
Bruce Bowen 0-2009 
Robert Swift 0-0 
Cedric Simmons 0-2010 
Sean Williams 0-2011 
Oleksiy Pecherov 0-2011 
Javaris Crittenton
Jarron Collins 0-0 
Quincy Douby 0-2010 
Jamorio Moon
Josh Boone
Scott Burrell
Chris Mihm 0-0 
Travis Diener 0-0 
David Andersen
Mardy Collins 0-2010 
Anthony Johnson 0-2009 
Roko Ukic 0-2010 
Jarvis Hayes
Brevin Knight 0-0 
Maceo Baston 0-0 
Sean May
Marcelo Huertas 0-0Retain Retained
Jacque Vaughn 0-2008 
Ricky Davis 0-2009 
Livio Jean-Charles 0-0 
Luis Montero 2017-0 Two Way
Calvin Booth 0-0 
Patrick O'Bryant
Rasho Nesterovic 0-0 
Flip Murray 0-2009 
David Vaughn
Yakhouba Diawara 0-0 
Leon Powe 0-2010 
Chris Hunter 0-2010 
Theo Pinson 2021-0 
Randolph Morris 0-2009 
Cory Higgins
Devean George 0-2009 
Bobby Brown 0-2009 
Reggie Perry 2021-0 
London Perrantes 0-0 Two Way
Tim Thomas 0-2010 
Brian Skinner 0-0 
Lindsey Hunter 0-2009 
DJ Mbenga 0-2010 
Jawad Williams 0-2010 
Nathan Jawai 0-0 
Sean Singletary
Mike Taylor 0-2009 
Sun Yue 0-2009 
Quinton Ross
Gabe Pruitt 0-2009 
Jermareo Davidson 0-0 
Devin Brown 0-2009 
Primoz Brezec 0-2009 
James Singleton 0-0 
Ronald Dupree 0-2010 
Luther Head 0-2010 
Pops Mensah-Bonsu 0-2010 
Larry Owens
Linton Johnson III 0-0 
Marcus Williams 0-2009 
Anthony Roberson 0-2009 
Marcus Williams 0-2009 
Will Conroy 0-2009 
Jonathan Bender 0-2009 
Taylor Griffin
Walter Sharpe 0-2009 
Mailk Hairston 0-0 
Mike James 0-2009 
Ryan Richards 0-0 
Josh Owens
Sergey Gladyr 0-2009 
Mustafa Shakur
JamesOn Curry 0-2009 
Mario West 0-0 
Othello Hunter 0-0 
Dontell Jefferson
DeMarcus Nelson 0-0 
Chris Richard 0-0 

"""