import asyncio
import lyricsgenius
import pandas as pd
import csv

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

# Sample data
data = {
    'Song': ['Shape of You', 'Blinding Lights', 'Bad Guy', 'Bohemian Rhapsody', 'Uptown Funk'],
    'Artist': ['Ed Sheeran', 'The Weeknd', 'Billie Eilish', 'Queen', 'Mark Ronson ft. Bruno Mars']
}
song_list = pd.DataFrame(data)

# Function to fetch lyrics asynchronously
async def fetch_lyrics_async(index, song_title, artist_name):
    lyrics = fetch_lyrics(song_title, artist_name)
    return (index, lyrics)

lyrics_dict = {}
tasks = []

# Iterate over songs
for index, row in song_list.iterrows():
    song_title = row['Song']
    artist_name = row['Artist']
    
    task = fetch_lyrics_async(index, song_title, artist_name)
    tasks.append(task)

# Run the tasks asynchronously
loop = asyncio.get_event_loop()
results = loop.run_until_complete(asyncio.gather(*tasks))

# Store the results in the dataframe
for index, lyrics in results:
    song_list.at[index, "Lyrics"] = lyrics

print(song_list)