#! /usr/bin/env python

import client
import subprocess
import time
import signal
import solver

if __name__ == '__main__':
    import sys
    command = sys.argv[1] if len(sys.argv) >= 2 else './enumerate_ml/enumerate'

    print 'solver: start.'

    for i in xrange(20): # set number of test cases.
        while True:
            train = client.post_train(size = 12)
            if train.has_key('status'):
                print train
            else:
                break
        solver.solve(train, command)
