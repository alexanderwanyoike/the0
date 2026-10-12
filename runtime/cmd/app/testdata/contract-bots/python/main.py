"""Plays the script of the sdk/contract case in case.json through the Python SDK.

contract_bot is the Python SDK's own contract bot (sdk/python/tests/contract_bot.py),
copied in beside this file, so the ops make the same SDK calls as in the SDK's suite.
"""

import json

import contract_bot


def main(bot_id, config):
    with open("case.json") as f:
        contract_bot.run_steps(json.load(f)["script"])
