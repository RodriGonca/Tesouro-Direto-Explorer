# -*- coding: utf-8 -*-
"""
Created on Fri Jun 28 19:23:37 2019

@author: @RodriGonca
"""

#%% import
import concurrent.futures
import re
from datetime import datetime
import requests
from bs4 import BeautifulSoup

#%% function 1
def get_links():
    URL = 'http://sisweb.tesouro.gov.br/apex/f?p=2031:2'
    result = requests.get(URL).content
    soup = BeautifulSoup(result, "html.parser")
    a_tags = soup.find_all("a")

    links = []
    anchor_url = URL.split('f?')[0]

    for a_tag in a_tags:
        link = re.findall(r'href="(.*)" style="padding-right:5px;"', str(a_tag))
        if link:
            links.append(anchor_url + link[0])

    return links

#%% function 2
def download_data(link: str):
    resp = requests.get(link)
    if resp.status_code == 200:
        try:
            filename = resp.headers['Content-Disposition'].split()[1].replace('filename="', '').replace('";', '')
            with open(f'data\\raw\\{filename}', 'wb') as output:
                output.write(resp.content)
        except KeyError:
            return f'fail to download {link}'
    else:
        download_data(link)

    return f'file {link} downloaded'

#%% step 1
if __name__ == '__main__':

    print(f'start: {datetime.now()}')    

    links = get_links()

    with concurrent.futures.ThreadPoolExecutor() as executor:
        results = executor.map(download_data, links)

    for result in results:
        print(result)

    print(f'end: {datetime.now()}')
