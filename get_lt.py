#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar  1 23:31:23 2022

@author: qxa
"""

import requests
from bs4 import BeautifulSoup as BS
import pandas as pd


# 百度API密钥，免费额度6000次，全国区划3000+足够用
ak = "mR2x438jrsgn6myhtqk28lsFYh3zlWeY"


# 根据区划名称获取经纬度信息
def get_location_by_region_name(region_name):
    url = f"https://api.map.baidu.com/geocoding/v3/?address={region_name}&output=json&ak={ak}"
    content = requests.get(url).json()
    try:
        location = content["result"]["location"]
        lon, lat = location["lng"], location["lat"]
        return (lon, lat)
    except:
        print(f"{region_name}获取位置出错！")
        return None


# 获取全国省市区划编码
def get_all_region_codes():
    url = "http://www.mca.gov.cn/article/sj/xzqh/2020/20201201.html"
    content = requests.get(url).text
    bs = BS(content, "lxml")
    
    table = bs.find("table")
    region_list = list()
    for tr in table.find_all("tr"):
        tds = tr.find_all("td")
        
        if len(tds)<3 or tds[1].text == "":
            continue
            
        code, name = tds[1].text.strip(), tds[2].text.strip()
        if code == "行政区划代码":
            continue
        elif code.endswith("0000"):
            level = "省"
        elif code.endswith("00"):
            level = "市"
        else:
            level = "区"
            
        region_list.append((level,code,name))
        
    return region_list
    
    
if __name__ == "__main__":
    columns=['区划级别',	'区划编码	','区划名称','经度',	'纬度']
    
    df=pd.DataFrame(columns=columns) #新建一个Dataframe
    i=0

    for (level,code,name) in get_all_region_codes():
        if (i%200==0):
            print(i)
        i+=1
        location = get_location_by_region_name(name)
        if not location:
            lon, lat = "",""
        else:
            lon, lat = location
            df=df.append([{'区划级别':level, '区划编码':code,'区划名称':name,
                          '经度':lon,'纬度':lat}], ignore_index=True)
    
    df.to_csv('region.csv')
    print("全国区划经纬度信息写入完成！")
    
    pos_date = pd.read_csv('data/pos_date.csv')
    df.columns = ['区划级别',	'a','区划名称','经度',	'纬度','地区编码']
    
    df['地区编码'] =  df['地区编码'].astype(str)
    pos_date['地区编码'] =  pos_date['地区编码'].astype(str)
    dat = pd.merge(pos_date, df,how='right',on='地区编码')
    
    dat.to_csv('data/prepared_dat_all_district.csv')
    
    
    
    
