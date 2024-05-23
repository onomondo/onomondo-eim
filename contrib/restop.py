#!/usr/bin/env python3
# Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

import sys
import argparse
import json
import requests
import erlang
import pprint

DOWNLOAD_DEFAULT='{ "eidValue" : "89882119900000000000000000000005", "order" : {"activationCode" : "1$testsmdpplus1.example.com$OPxLD-UVRuC-jysPI-YkOwT"}}'
PSMO_DEFAULT='{ "eidValue" : "89882119900000000000000000000005", "order" : [{"psmo" : "enable", "iccid" : "98001032547698103285", "rollback" : false }]}'

def h2b(s) -> bytearray:
    """convert from a string of hex nibbles to a sequence of bytes"""
    return bytes.fromhex(s)

def rest_create(host, facility, Json):
    r = requests.post("http://" + str(host) + "/" + str(facility) + "/create", json=Json)
    return str(r.url)

def rest_lookup(host, facility, ResourceId):
    r = requests.get("http://" + str(host) + "/" + str(facility) + "/lookup/" + str(ResourceId))
    return r.json()

def rest_delete(host, facility, ResourceId):
    r = requests.get("http://" + str(host) + "/" + str(facility) + "/delete/" + str(ResourceId))
    return r.json()

def rest_list(host, facility):
    r = requests.get("http://" + str(host) + "/" + str(facility) + "/list/")
    return r.json()

def main(argv):
    parser = argparse.ArgumentParser(prog='restop', description='utility to operate on the REST API of onomondo_eim')
    parser.add_argument("-s", "--host", default="127.0.0.1:8080")
    parser.add_argument("-f", "--facility", default="download")
    parser.add_argument("-c", "--create", action='store_true', default=False)
    parser.add_argument("-l", "--lookup", action='store_true', default=False)
    parser.add_argument("-d", "--delete", action='store_true', default=False)
    parser.add_argument("-t", "--list", action='store_true', default=False)
    parser.add_argument("-r", "--resource-id")
    parser.add_argument("-j", "--json")

    args = parser.parse_args()

    if args.create:
        print("create on: " + str(args.host), file=sys.stderr)
        print(" facility: " + str(args.facility), file=sys.stderr)
        if args.json:
            args_json = str(args.json)
        else:
            if args.facility == "download":
                args_json = DOWNLOAD_DEFAULT
            elif args.facility == "psmo":
                args_json = PSMO_DEFAULT
        print(" json: " + args_json, file=sys.stderr)
        print("result:", file=sys.stderr)
        print(rest_create(args.host, args.facility, json.loads(args_json)))
    elif args.lookup:
        print("lookup on: " + str(args.host), file=sys.stderr)
        print(" facility: " + str(args.facility), file=sys.stderr)
        print(" resourceId: " + str(args.resource_id), file=sys.stderr)
        result = rest_lookup(args.host, args.facility, args.resource_id)
        print(json.dumps(result))
        if 'debuginfo' in result:
            debuginfo_hexstr=result['debuginfo']
            debuginfo_bytes=h2b(debuginfo_hexstr)
            debuginfo_term=erlang.binary_to_term(debuginfo_bytes)
            pp = pprint.PrettyPrinter(depth=4)
            debuginfo_pretty = pp.pformat(debuginfo_term)
            print("decoded debuginfo: " + str(debuginfo_pretty), file=sys.stderr)
    elif args.delete:
        print("delete on: " + str(args.host), file=sys.stderr)
        print(" facility: " + str(args.facility), file=sys.stderr)
        print(" resourceId: " + str(args.resource_id), file=sys.stderr)
        print("result:", file=sys.stderr)
        print(json.dumps(rest_delete(args.host, args.facility, args.resource_id)))
    elif args.list:
        print("list on: " + str(args.host), file=sys.stderr)
        print(" facility: " + str(args.facility), file=sys.stderr)
        print("result:", file=sys.stderr)
        print(json.dumps(rest_list(args.host, args.facility), file=sys.stderr))

if __name__ == "__main__":
    main(sys.argv[1:])

