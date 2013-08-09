import client
import json
import time

while True:
    json_str = json.dumps(client.post_train())
    file = open('train.json', 'a')
    file.write(json_str)
    file.close()
    time.sleep(4)
