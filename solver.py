#! /usr/bin/env python

import client
import subprocess
import time

wait_time = 1

def problem_string(problem):
    ret = ''
    ret += str(problem['size']) + ' '
    ret += str(len(problem['operators'])) + ' '
    ret += ' '.join(problem['operators'])
    return ret

def parse_query(query_line):
    words = query_line.strip().split(' ')
    query = {'type': words[0]}
    if query['type'] == 'eval':
        # eval query
        query['arguments'] = map(int, words[2:])
    else:
        # guess query
        query['program'] = query_line[6:].strip()
    return query

def eval_result_string(eval_result):
    decs = map(lambda x: str(int(x, 16)), eval_result['outputs'])
    return ('eval %d ' % len(decs)) + ' '.join(decs)

def guess_result_string(guess_result):
    ret = guess_result['status']
    if ret == 'mismatch':
        decs = map(lambda x: str(int(x, 16)), guess_result['values'])
        ret += ' ' + ' '.join(decs)
    return ret

def solve(problem, solver_command):
    print 'solver: problem:', problem
    # run actual solver as subprocess
    worker = subprocess.Popen(solver_command, stdin = subprocess.PIPE, stdout = subprocess.PIPE)

    # give problem to worker
    worker.stdin.write(problem_string(problem) + '\n')
    worker.stdin.flush()

    # execute commands from worker
    while True:
        raw_query = worker.stdout.readline()
        query = parse_query(raw_query)
        if query['type'] == 'eval':
            while True:
                print 'solver: query:', query
                time.sleep(wait_time)
                result = client.post_eval(problem, query['arguments'])
                if result['status'] == 'error':
                    if result['message'].find('Too many requests') >= 0:
                        print 'solver: Query refused because of too many requests. Retry.'
                    else:
                        print 'solver: error occured in eval query.'
                        print result['message']
                        return
                else:
                    break
            worker.stdin.write(eval_result_string(result) + '\n')
            worker.stdin.flush()
        else:
            while True:
                print 'solver: query:', query
                time.sleep(wait_time)
                result = client.post_guess(problem, query['program'])
                if result['status'] == 'error':
                    if result['message'].find('Too many requests') >= 0:
                        print 'solver: Query refused because of too many requests. Retry.'
                    else:
                        print 'solver: error occured in guess query.'
                        print result['message']
                        return
                else:
                    break
            worker.stdin.write(guess_result_string(result) + '\n')
            worker.stdin.flush()
            if result['status'] == 'win':
                print 'solver: We have solved the problem id', problem['id'], '!!'
                return


def solve_honban(condition, command):
    problems = client.post_myproblems(update = True)
    time.sleep(wait_time)
    for problem in problems:
        if condition(problem) and not problem.get('solved', False) and problem.get('timeLeft', 300.) > 0.:
            solve(problem, command)

def solve_honban_id(ids, command):
    problems = client.post_myproblems(update = False)
    for problem in problems:
        if problem['id'] == ids and not problem.get('solved', False) and problem.get('timeLeft', 300.) > 0.:
            solve(command)

if __name__ == '__main__':
    import sys
    command = sys.argv[1] if len(sys.argv) == 2 else './test_worker.py'

    print 'solver: start.'
    for i in xrange(20):
        while True:
            train = client.post_train(size = 5)
            if train.has_key('status'):
                print train
            else:
                break
        solve(train, command)

    quit()

    def cond(prob):
        return prob['size'] <= 5
    solve_honban(cond, command)



