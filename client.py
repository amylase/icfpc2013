#! /usr/bin/env python
import urllib2
import os

user_id = '0352iTeh6NszUZhvuvJktMHZnQlc93aNwuW4KIJwvpsH1H'
url_base = 'http://icfpc2013.cloudapp.net/'
# url_base + {'myproblems, train, eval, guess'} + '?auth=' + user_id

if not os.path.exists('problems.json'):
    url_problem = url_base + 'myproblems' + '?auth=' + user_id
    print url_problem
    with open('problems.json', 'w') as writer:
        writer.write(urllib2.urlopen(url_problem).read())
else:
    print 'problems.json exists.'
 
import json
with open('problems.json') as reader:
    ps = json.load(reader)

ps.sort(key = lambda p: p['size'])

print len([x for x in ps if 'fold' not in x['operators'] and 'plus' not in x['operators'] and x['size'] <= 15])
 
quit()

# try train
url_train = url_base + 'train' + '?auth=' + user_id
train_problem = json.load(urllib2.urlopen(url_train))
print train_problem

def post_eval(problem, args):
    url_eval = url_base + 'eval?auth=' + user_id
    argstrs = map(str, args)
    data = json.dumps({'id': problem['id'], 'arguments': argstrs})
    return json.load(urllib2.urlopen(url_eval, data))

def post_guess(problem, program):
    url_guess = url_base + 'guess?auth=' + user_id
    data = json.dumps({'id': problem['id'], 'program': program})
    return json.load(urllib2.urlopen(url_guess, data))

print 'size:', train_problem['size']
print post_eval(train_problem, xrange(0, 32))
print post_guess(train_problem, train_problem['challenge'])
