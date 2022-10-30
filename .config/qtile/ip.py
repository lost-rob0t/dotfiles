#!/usr/bin/env python3

import os
import json
from libqtile.widget import base
from libqtile.log_utils import logger

try:
    import requests as r
except ImportError:
    logger.warning("requests are not installed, will not be able to display public ip!")

def getIps():
    d = {}
    data = json.load(os.popen("ip -j addr"))
    adata = []
    for i in data:
        for ad in i["addr_info"]:
            adata.append(ad)

    n = 0
    for i in adata:
        l = i.get("label")
        ip = i.get("local")
        if l is not None and ip is not None and l != "lo":
            d[n] = l + ": " + ip
            n += 1
    #n += 1
    d[n] = "Public: " + publicIp()

    return d

def publicIp():
    url = "http://ifconfig.me"
    try:
        resp = r.get(url).text
        return resp
    except r.IoError:
        return "Error Getting IP"
    except r.TimeoutError:
        return "Timeout getting IP"
class IpWidget(base.ThreadPoolText):
    defaults = [
        ("update_interval", 10),
        ("font", "Noto Sans Bold"),
        ("text", "No ip"),

    ]
    def __init__(self, **config):
        base.ThreadPoolText.__init__(self, "", **config)
        self.add_defaults(IpWidget.defaults)
        self.ip = getIps()
        logger.warning(self.ip)
        self.n = 0

    def poll(self):
        #self.ip = getIps()
        self.n += 1
        if self.n > len(self.ip) - 1:
            self.n = 0
        text = self.ip[self.n]
        return text
