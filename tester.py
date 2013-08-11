#! /usr/bin/env python

import subprocess
import time
import signal
import solver
import sys

is_local = '--local' in sys.argv
if is_local:
    import local_client as client
else:
    import client

if __name__ == '__main__':
    command = sys.argv[1] if len(sys.argv) >= 2 else './enumerate_ml/enumerate'
    size = int(sys.argv[2]) if len(sys.argv) >= 3 else 8
    print 'solver: start.'
    
    problem = eval("{u'challenge': u'(lambda (x_29783) (fold (or (shr4 x_29783) (or 1 x_29783)) 0 (lambda (x_29784 x_29785) (or (not x_29784) x_29785))))', u'operators': [u'fold', u'not', u'or', u'shr4'], u'id': u'54B6G032B4tZubH2ktEeLXl4', u'size': 14}")
    solver.solve(problem, command)

    quit()

    for i in xrange(200): # set number of test cases.
        while True:
            train = client.post_train(size = size)
            if train.has_key('status'):
                print train
            else:
                break
        solver.solve(train, command)
