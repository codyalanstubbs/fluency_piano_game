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

midiin, port_name = open_midiinput(port)

print("Entering main loop. Press Control-C to exit.")

timer = time.time()

df = pandas.DataFrame(columns = ['Date', 'Module', 'Hand', 'Time', 'Stroke', 'Key', 'Speed'])

while True:
    msg = midiin.get_message()
    
    if msg:
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
        
        df = df[(df.Stroke == 144) | (df.Stroke == 128) | (df.Stroke == 177)]
        df = df[-((df.Stroke == 177) & (df.Speed == 127))]

        df.to_csv('piano_practice_data.csv', mode='w', header=True, index=True)

        time.sleep(0.01)
