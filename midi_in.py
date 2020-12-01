#!/usr/bin/env python
#
# midiin_poll.py
#
"""Show how to receive MIDI input by polling an input port."""

from __future__ import print_function

import logging
import sys
import time
import os

from rtmidi.midiutil import open_midiinput
import pandas
import datetime

log = logging.getLogger('midiin_poll')
logging.basicConfig(level=logging.DEBUG)

# Prompt user to indicate what hand(s) they will be practicing with
hand = input("Pleast type 'left', 'right', or 'both' to indicate the hand(s) you are practicing: ")

# Prompt user for practice module name
module = input("Please type the name of the practice module: ")
                
# Prompts user for MIDI input port, unless a valid port number or name
# is given as the first argument on the command line.
# API backend defaults to ALSA on Linux.
port = sys.argv[1] if len(sys.argv) > 1 else None


# Opens a midi input port and assigns a midiin object and captures name of port
midiin, port_name = open_midiinput(port)

# Prompt for performing a hard exit
print("Entering main loop. Press Control-C to exit.")

# Starts time before any keys are pressed e.g.: beginning of practice session
timer = time.time()

# Create a dataframe to store pressed key data in locally
df = pandas.DataFrame(columns = ['Date', 'Module', 'Hand', 'Time', 'Stroke', 'Key', 'Speed'])

# Main loop for capturing pressed key data
while True:
    # Retrieves most recent midi input message
    msg = midiin.get_message()
    
    if msg: # Following creates variables and adds them to the dataframe
        message, deltatime = msg
        timer += deltatime
        date = datetime.datetime.now().date()
        df = df.append({
            'Date' : date,
            'Module' : module,
            'Hand' : hand,
            'Time' : timer,
            'Stroke' : message[0],
            'Key' : message[1],
            'Speed' : message[2]
            },  
                ignore_index = True)

        # Filters dataframe for beginning and end of key stroke AND foot pedal presses
        df = df[(df.Stroke == 144) | (df.Stroke == 128) | (df.Stroke == 177)]

        # Removes the duplicate foot pedal speed data; foot pedal data can now be IDed as Stroke=177 and Soeed=0
        df = df[-((df.Stroke == 177) & (df.Speed == 127))]

        # Write the data to the datastorace csv
        df.to_csv('piano_practice_data.csv', mode='w', header=True, index=True)
        
        time.sleep(0.01)
