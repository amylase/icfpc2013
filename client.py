#! /usr/bin/env python
import urllib2
import os
import json

user_id = '0352iTeh6NszUZhvuvJktMHZnQlc93aNwuW4KIJwvpsH1H'
url_base = 'http://icfpc2013.cloudapp.net/'
# url_base + {'myproblems, train, eval, guess'} + '?auth=' + user_id

problems = json.load(open('train_mod.json'))

def get_problem_by_id(idstr):
    l = [p for p in problems if p['id'] == idstr]
    if len(l) > 0: return l[0]
    raise Exception('get_problem_by_id: No such id. idstr = %s' % idstr)

def post_myproblems(update = True):
    if not os.path.exists('problems.json') or update == True:
        url_problem = url_base + 'myproblems?auth=' + user_id
        print url_problem
        with open('problems.json', 'w') as writer:
            try: 
                writer.write(urllib2.urlopen(url_problem).read())
            except urllib2.HTTPError as err:
                return {u'status': u'error', u'message': err.__str__()}
            except:
                print 'unknown error on post_myproblems'
                raise
    else:
        print 'problems.json exists.'
    with open('problems.json') as reader:
        return json.load(reader)

def post_train(size = 0, operators = None):
    # operators is '' or 'tfold' or 'fold'
    try:
        url_train = url_base + 'train?auth=' + user_id
        data = dict()
        if 3 <= size <= 30 or size == 42:
            data['size'] = size
        if operators in ['tfold', 'fold']:
            data['operators'] = [operators]
        if operators == '':
            data['operators'] = []
        return json.load(urllib2.urlopen(url_train, json.dumps(data)))
    except urllib2.HTTPError as err:
        return {u'status': u'error', u'message': err.__str__()}
    except:
        print 'unknown error on post_train'
        raise

def post_eval(problem, args):
    try:
        url_eval = url_base + 'eval?auth=' + user_id
        argstrs = map(lambda x: ('0x%016x' % x).upper(), args)
        data = json.dumps({'id': problem['id'], 'arguments': argstrs})
        return json.load(urllib2.urlopen(url_eval, data))
    except urllib2.HTTPError as err:
        return {u'status': u'error', u'message': err.__str__()}
    except:
        print 'unknown error on post_eval'
        raise

def post_eval_program(program, args):
    try:
        url_eval = url_base + 'eval?auth=' + user_id
        argstrs = map(lambda x: ('0x%016x' % x).upper(), args)
        data = json.dumps({'program': program, 'arguments': argstrs})
        return json.load(urllib2.urlopen(url_eval, data))
    except urllib2.HTTPError as err:
        return {u'status': u'error', u'message': err.__str__()}
    except:
        print 'unknown error on post_eval_program'
        raise
    
def post_guess(problem, program):
    try:
        url_guess = url_base + 'guess?auth=' + user_id
        data = json.dumps({'id': problem['id'], 'program': program})
        return json.load(urllib2.urlopen(url_guess, data))
    except urllib2.HTTPError as err:
        return {u'status': u'error', u'message': err.__str__()}
    except:
        print 'unknown error on post_guess'
        raise

# print 'size:', train_problem['size']
# print post_eval(train_problem, xrange(0, 32))
# print post_guess(train_problem, train_problem['challenge'])
