#!/usr/bin/env python3

import requests

def rest_create(host, facility, Json):
    r = requests.post("http://" + str(host) + "/" + str(facility) + "/create", json=Json)
    print(r.json())

def rest_lookup(host, facility, ResourceId):
    r = requests.get("http://" + str(host) + "/" + str(facility) + "/lookup/" + str(ResourceId))
    print(r.json())

def rest_delete(host, facility, ResourceId):
    r = requests.get("http://" + str(host) + "/" + str(facility) + "/delete/" + str(ResourceId))
    print(r.json())

def rest_list(host, facility):
    r = requests.get("http://" + str(host) + "/" + str(facility) + "/list/")
    print(r.json())


#rest_lookup("127.0.0.1:8080", "download", "939dae86-a306-405a-852f-6397f2cd0aaa")
#rest_delete("127.0.0.1:8080", "download", "939dae86-a306-405a-852f-6397f2cd0aaa")
rest_create("127.0.0.1:8080", "download",
            { "eidValue" : "123",
              "order" : {"activationCode" : "1$testsmdpplus1.example.com$OPxLD-UVRuC-jysPI-YkOwT"}})
rest_list("127.0.0.1:8080", "download")
