#! /usr/bin/env python

import sys

def say(arg):
    sys.stderr.write(str(arg) + '\n')
    return

if __name__ == '__main__':
    say('You are the human worker!!')
    
    
