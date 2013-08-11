#! /usr/bin/env python
import sys
import subprocess
import time
import signal

is_local = '--local' in sys.argv
if is_local:
    import local_client as client
else:
    import client

def emph(string):
    return '\033[31m' + string + '\033[0m'

default_wait = 5
def wait_server(wait_time = default_wait):
    if not is_local: time.sleep(max(wait_time, 0))

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
    elif query['type'] == 'guess':
        # guess query
        query['program'] = query_line[6:].strip()
    else:
        # skip query
        pass
    return query

def eval_result_string(eval_result):
    decs = map(lambda x: str(int(x, 16)), eval_result['outputs'])
    return ('eval %d ' % len(decs)) + ' '.join(decs)

def guess_result_string(guess_result):
    ret = guess_result['status']
    if ret == 'mismatch':
        decs = map(lambda x: str(int(x, 16)), guess_result['values'])
        ret += ' ' + ' '.join(decs)
    if ret == 'error' and guess_result['message'].find('Unable to decide equality') >= 0:
        # server unable to decide.
        ret = 'unknown'
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
        # wait at most 60 seconds
        try:
            def handler(signum, frame): 
                if signum == signal.SIGALRM: raise Exception('Enumeration failed.')
            signal.signal(signal.SIGALRM, handler)
            signal.alarm(60)
            worker_time = time.time()
            raw_query = worker.stdout.readline()
            worker_time = time.time() - worker_time
            print 'solver: worker elapsed time:', worker_time, 'sec.'
            signal.alarm(0)
        except Exception:
            print emph('Enumeration takes more than 60 secs. Skipped.')
            signal.alarm(0)
            break
        
        query = parse_query(raw_query)
        if query['type'] == 'eval':
            while True:
                print 'solver: query:', query
                wait_server(default_wait - worker_time)
                result = client.post_eval(problem, query['arguments'])
                if result['status'] == 'error':
                    if result['message'].find('Too many requests') >= 0:
                        print emph('solver: Query refused because of too many requests. Retry.')
                    else:
                        print emph('solver: error occured in eval query.')
                        print result['message']
                        return
                else:
                    break
            worker.stdin.write(eval_result_string(result) + '\n')
            worker.stdin.flush()
        elif query['type'] == 'guess':
            while True:
                print 'solver: query:', query
                wait_server(default_wait - worker_time)
                result = client.post_guess(problem, query['program'])
                if result['status'] == 'error':
                    if result['message'].find('Too many requests') >= 0:
                        print emph('solver: Query refused because of too many requests. Retry.')
                    if result['message'].find('Unable to decide equality') >= 0:
                        print emph('solver: Server could not decide equality. Demand new guess.')
                        break
                    else:
                        print emph('solver: error occured in guess query.')
                        print result['message']
                        return
                else:
                    break
            worker.stdin.write(guess_result_string(result) + '\n')
            worker.stdin.flush()
            if result['status'] == 'win':
                print emph('solver: We have solved the problem id ' + problem['id'] + ' !!')
                return
        else:
            print emph('solver: This problem was skipped.')
            break
    worker.kill()
    return

def solve_honban(condition, command):
    wait_server()
    problems = client.post_myproblems(update = True)
    for problem in problems:
        if condition(problem) and not problem.get('solved', False) and problem.get('timeLeft', 300.) > 0.:
            solve(problem, command)
            print 'solver: You can kill solver now. (3 secs given)'
            time.sleep(3)

def solve_honban_id(ids, command):
    problems = client.post_myproblems(update = False)
    for problem in problems:
        if problem['id'] == ids and not problem.get('solved', False) and problem.get('timeLeft', 300.) > 0.:
            solve(command)
            print 'solver: You can kill solver now. (3 secs given)'
            time.sleep(3)

if __name__ == '__main__':
    command = sys.argv[1] if len(sys.argv) >= 2 else './enumerate_ml/enumerate'

    print 'solver: start.'

    def cond(prob): # problem(dict) -> bool. true iff prob is what you want to tackle.
        return prob['size'] <= 11 
    solve_honban(cond, command)


