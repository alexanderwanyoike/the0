"""Answers the handlers of the sdk/contract case in case.json through the Python SDK."""

import json

import contract_bot
from the0 import query

with open("case.json") as f:
    case = json.load(f)

for path, steps in case["handlers"].items():
    query.handler(path)(contract_bot.handler_for(steps))
query.run()
