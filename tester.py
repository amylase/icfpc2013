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

    solver.solve(client.get_problem_by_id('ttHHkcrjKRlzBmGa9zyjHeOT'), command)

    quit()

    for i in xrange(200): # set number of test cases.
        while True:
            train = client.post_train(size = size)
            if train.has_key('status'):
                print train
            else:
                break
        solver.solve(train, command)
