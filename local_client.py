#! /usr/bin/env python
import os
import json
import random
import subprocess
import re

problems = json.load(open('train_mod.json'))

def get_problem_by_id(idstr):
    l = [p for p in problems if p['id'] == idstr]
    if len(l) > 0: return l[0]
    raise Exception('get_problem_by_id: No such id. idstr = %s' % idstr)

def post_myproblems(update = True):
    return problems

def post_train(size = 0, operators = None):
    operators = operators if operators in ['', 'tfold', 'fold'] else None
    def check(problem):
        if size != 0:
            if problem['size'] != size: return False
        if operators != None:
            if operators == '' and ('tfold' in problem['operators'] or 'fold' in problem['operators']):
                return False
            if operators != '' and operators not in problem['operators']:
                return False
        return True                
    cands = [p for p in problems if check(p)]
    return random.choice(cands if len(cands) > 0 else problem)

def int_to_hex(x):
    return u'0x%016x' % x

def paren_match(program):
    stack = []
    ret = dict()
    for i, ch in enumerate(program):
        if ch == '(':
            stack.append(i)
        elif ch == ')':
            ret[stack.pop()] = i + 1
    return ret
       
train_var = re.compile('x_\d+') 
top_lambda = re.compile('\(lambda \((x_\d+)\)')
fold_lambda = re.compile('\(lambda \((x_\d+) (x_\d+)\)')
def normalize_program(program):
    if re.search(train_var, program) == None: return program
    ret = program
    if program.find('fold') >= 0:
        fold_match = re.search(fold_lambda, program)
        y_var, z_var = fold_match.group(1, 2)
        start = fold_match.start()
        end = paren_match(program)[start]
        ret_hd, ret_fold, ret_tl = ret[:start], ret[start:end], ret[end:]
        ret_fold = ret_fold.replace(y_var, 'y')
        ret_fold = ret_fold.replace(z_var, 'z')
        ret = ret_hd + ret_fold + ret_tl
    x_var = re.search(top_lambda, program).group(1)
    ret = ret.replace(x_var, 'x')
    return ret

def eval_program(program, arg):
    output = subprocess.check_output(['./interp/interp', str(arg), normalize_program(program)])
    return int_to_hex(int(output.strip()))

def post_eval(problem, args):
    return {u'status': u'ok', u'outputs': map(lambda arg: eval_program(problem['challenge'], arg), args)}

def post_eval_program(program, args):
    return {u'status': u'ok', u'outputs': map(lambda arg: eval_program(program, arg), args)}
    
def post_guess(problem, program):
    test_cases = list(xrange(256)) + map(lambda x: 1 << x, xrange(64)) + map(lambda x: random.randrange(1 << 64), xrange(256))
    
    for arg in test_cases:
        expected = post_eval(problem, [arg])
        returns = post_eval_program(program, [arg])
        if expected['outputs'][0] != returns['outputs'][0]:
            return {u'status': u'mismatch', u'values': [int_to_hex(arg), expected['outputs'][0], returns['outputs'][0]]}

    return {u'status': u'win'} # precisely it should return 'error: unable to decide equality.'
        
