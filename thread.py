import threading
import lyricsgenius
import pandas as pd

genius_token = "Q7F1UAjgD74fWuudQiukK18h-wK6fO-KRAOiPE_8b2WTfCn_HGM5ucMZE_Ziuzbe"

genius = lyricsgenius.Genius(genius_token)

# Function to fetch lyrics
def fetch_lyrics(song_title, artist_name):
    try:
        # Search for the song
        song = genius.search_song(song_title, artist_name)
        if song:
            print(f"Fetched lyrics for '{song_title}' by '{artist_name}'")
            return song.lyrics
        else:
            print(f"No lyrics found for '{song_title}' by '{artist_name}'")
            return None
    except Exception as e:
        print(f"Error fetching lyrics for '{song_title}' by '{artist_name}': {e}")
        return None

# Read the data
song_list = pd.read_csv('songlist.csv')

# Clean the data
song_list = song_list.drop(columns=["Unnamed: 0"])
song_list["Year"] = song_list["Year"].astype(int) 

# Find where it gets bad 
error_index = song_list[song_list['Year'] == 2021].index[0]

# Split the data
df_correct = song_list.iloc[:error_index]
df_mistake = song_list.iloc[error_index:]

# Split 'Artist' column into 'Artist' and 'Additional Text'
df_mistake[['Song2', 'Artist']] = df_mistake['Artist'].str.split('and', n=1, expand=True)
df_mistake['Song2'] = df_mistake['Song2'].str.replace('"', '')
df_mistake["Artist"] = df_mistake['Artist'].str[:-3]
df_correct["Artist"] = df_correct['Artist'].str[:-3]
df_mistake["Song"] = df_mistake["Song2"]
df_mistake = df_mistake.drop(columns=["Song2"])

song_list = pd.concat([df_correct, df_mistake], ignore_index=True)

# Function to fetch lyrics and update the DataFrame
def fetch_and_update(index, song_title, artist_name):
    lyrics = fetch_lyrics(song_title, artist_name)
    song_list.at[index, 'Lyrics'] = lyrics

threads = []

# Iterate over songs and create a thread for each fetch operation
for index, row in song_list.iterrows():
    song_title = row['Song']
    artist_name = row['Artist']
    
    thread = threading.Thread(target=fetch_and_update, args=(index, song_title, artist_name))
    threads.append(thread)
    thread.start()

# Wait for all threads to complete
for thread in threads:
    thread.join()

# Save the dataframe to a CSV file
csv_file = 'songs_with_lyrics.csv'
song_list.to_csv(csv_file, index=False)

# Display the dataframe with lyrics
print(song_list)
