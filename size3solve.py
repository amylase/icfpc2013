#! /usr/bin/env python

import client
import time

ps = client.post_myproblems()

for p in ps:
    if p['size'] == 3:
        if 'not' in p['operators']:
            continue
            print p
            print 'this case is already solved.'
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

        elif 'shl1' in p['operators']:
            continue
            print p
            print 'this case is already solved.'
            res = int(client.post_eval(p, [2])['outputs'][0], 16)
            print res
            program = '(lambda (x) (shl1 arg))'
            if res == 4:
                # arg x
                program = program.replace('arg', 'x')
            elif res == 0:
                # arg 0
                program = program.replace('arg', '0')
            else:
                # arg 1
                program = program.replace('arg', '1')
        
            print program
            print client.post_guess(p, program)

        elif 'shr1' in p['operators']:
            continue
            print p
            res = int(client.post_eval(p, [2])['outputs'][0], 16)
            print res
            program = '(lambda (x) (shr1 arg))'
            if res == 1:
                # arg x
                program = program.replace('arg', 'x')
            else:
                # arg 1
                program = program.replace('arg', '1')
        
            print program
            print client.post_guess(p, program)
            time.sleep(10)

        elif 'shr4' in p['operators']:
            continue
            print p
            res = int(client.post_eval(p, [1 << 4])['outputs'][0], 16)
            print res
            program = '(lambda (x) (shr4 arg))'
            programx = program.replace('arg', 'x')
            program1 = program.replace('arg', '1')
                
            print client.post_guess(p, programx)
            print client.post_guess(p, program1)
            time.sleep(10)

        elif 'shr16' in p['operators']:
            print p
            res = int(client.post_eval(p, [1 << 16])['outputs'][0], 16)
            print res
            program = '(lambda (x) (shr16 arg))'
            if res == 1:
                # arg x
                program = program.replace('arg', 'x')
            else:
                # arg 1
                program = program.replace('arg', '1')
        
            print program
            print client.post_guess(p, program)
            time.sleep(10)
