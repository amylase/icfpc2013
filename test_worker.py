#! /usr/bin/env python

import sys

def say(arg):
    sys.stderr.write(str(arg) + '\n')
    return

def query(arg):
    sys.stdout.write(str(arg) + '\n')
    sys.stdout.flush()
    return

if __name__ == '__main__':
    say('worker: start')

    problem = sys.stdin.readline().strip().split(' ')
    size = int(problem[0])
    operators = problem[2:]
    say('worker: problem recieved.')
    say('worker: size = ' + str(size))
    say('worker: operators = ' + str(operators))
    
    query('eval 3 0 1 2')
    say('worker: query post. eval 3 0 1 2')
    eval_result = sys.stdin.readline().strip().split(' ')
    say('worker: eval result.' + str(eval_result))

    guess_prog = 'guess (lambda (x) (%s x))' % operators[0]
    query(guess_prog)
    say('worker: query post. ' + guess_prog)
    guess_result = sys.stdin.readline().strip().split(' ')
    say('worker: result recieved. ' + guess_result[0])

    guess_prog = 'guess (lambda (x) (%s x))' % operators[0]
    query(guess_prog)
    say('worker: query post. ' + guess_prog)
    guess_result = sys.stdin.readline().strip().split(' ')
    say('worker: result recieved. ' + guess_result[0])

    guess_prog = 'guess (lambda (x) (%s x))' % operators[0]
    query(guess_prog)
    say('worker: query post. ' + guess_prog)
    guess_result = sys.stdin.readline().strip().split(' ')
    say('worker: result recieved. ' + guess_result[0])


