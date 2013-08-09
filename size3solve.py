#! /usr/bin/env python

import client

ps = client.post_myproblems()

for p in ps:
    if p['size'] == 3 and 'not' in p['operators']:
        print p
        print client.post_eval(p, [2])
        res = int(client.post_eval(p, [2])['outputs'][0], 16)
        print res
        program = '(lambda (x) (not arg))'
        if res == (1 << 64) - 3:
            # arg x
            program = program.replace('arg', 'x')
        elif res == (1 << 64) - 1:
            # arg 0
            program = program.replace('arg', '0')
        else:
            # arg 1
            program = program.replace('arg', '1')
        
        print program
        print client.post_guess(p, program)
