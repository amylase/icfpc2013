#! /usr/bin/env python
import urllib2
import os
import json

user_id = '0352iTeh6NszUZhvuvJktMHZnQlc93aNwuW4KIJwvpsH1H'
url_base = 'http://icfpc2013.cloudapp.net/'
# url_base + {'myproblems, train, eval, guess'} + '?auth=' + user_id

def post_myproblems(update = False):
    if not os.path.exists('problems.json') or update == True:
        url_problem = url_base + 'myproblems?auth=' + user_id
        print url_problem
        with open('problems.json', 'w') as writer:
            writer.write(urllib2.urlopen(url_problem).read())
    else:
        print 'problems.json exists.'
    with open('problems.json') as reader:
        return json.load(reader)

def post_train():
    url_train = url_base + 'train?auth=' + user_id
    return json.load(urllib2.urlopen(url_train))

def post_eval(problem, args):
    url_eval = url_base + 'eval?auth=' + user_id
    argstrs = map(lambda x: ('0x%016x' % x).upper(), args)
    data = json.dumps({'id': problem['id'], 'arguments': argstrs})
    return json.load(urllib2.urlopen(url_eval, data))

def post_eval_program(program, args):
    url_eval = url_base + 'eval?auth=' + user_id
    argstrs = map(lambda x: ('0x%016x' % x).upper(), args)
    data = json.dumps({'program': program, 'arguments': argstrs})
    return json.load(urllib2.urlopen(url_eval, data))

def post_guess(problem, program):
    url_guess = url_base + 'guess?auth=' + user_id
    data = json.dumps({'id': problem['id'], 'program': program})
    return json.load(urllib2.urlopen(url_guess, data))

# print 'size:', train_problem['size']
# print post_eval(train_problem, xrange(0, 32))
# print post_guess(train_problem, train_problem['challenge'])
