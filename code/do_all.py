# -*- coding: utf-8 -*-
# <nbformat>3.0</nbformat>

# <codecell>

# Get today's date
import time
today = time.strftime("%Y-%m-%d")

# <codecell>

# Specify which directories we'll be using (different on Ben vs. Joe-linux vs. Joe-Windows)
import platform
plat = platform.uname()
if 'joebrew' in plat:
    private = '/media/joebrew/JB/fdoh/private/surv'
    public = '/home/joebrew/Documents/surv_public'
elif 'benbrew' in plat:
    private = '/home/benbrew/Documents/private/surv/'
    public = '/home/benbrew/Documents/surv_public'

else:
    private = 'E:/fdoh/private/surv'
    public = 'C:/Users/BrewJR/Documents/surv_public'

private_today = private + '/' + today

# <codecell>

# If necessary, create today's directory
# and set working directory there
import os
if not os.path.exists(private_today):
  os.makedirs(private_today)

os.chdir(private)

# <codecell>

# Import necessary libraries
import mechanize
import cookielib
from BeautifulSoup import BeautifulSoup
import html2text
import pandas as pd
import subprocess
import rpy2.robjects as robjects

# <codecell>

# Run the r script to get the dates and URLs for today's data download
os.chdir(public)
robjects.r['source']("code/00_get_links.R")

# <codecell>

# Read in which links I need for today
import pandas as pd
os.chdir(private)
todays_links = pd.read_csv('todays_links.csv')

# <codecell>

# Browser
br = mechanize.Browser()

# Cookie Jar
cj = cookielib.LWPCookieJar()
br.set_cookiejar(cj)

# <codecell>

# Browser options
br.set_handle_equiv(True)
br.set_handle_gzip(True)
br.set_handle_redirect(True)
br.set_handle_referer(True)
br.set_handle_robots(False)
br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)

br.addheaders = [('User-agent', 'Chrome')]

# <codecell>

# The site we will navigate into, handling it's session
br.open('https://www.essencefl.com/florida_5_1_14/servlet/Login')
#br.open('https://github.com/login')

# View available forms
#for f in br.forms():
#    print f

# Select the second (index one) form (the first form is a search query box)
br.select_form(nr=0)

# <codecell>

# Read in credentials
pu = pd.read_csv('/home/joebrew/Documents/private/essence/pu.csv', dtype = 'string')

# <codecell>

#u = u.read()
#p = p.read()

# <codecell>

u = list(pu['u'])[0]
p = list(pu['p'])[0]

# <codecell>

# User credentials
br.form['j_username'] = u
br.form['j_password'] = p

# Login
br.submit()

# <codecell>

# Set working directory to today's private folder
os.chdir(private_today)

# <codecell>

for i in range(0,9,1):
    my_file = br.open(todays_links[0:]['link'][i])
    # Write a text file
    f = open(todays_links[0:]['file'][i], 'w')
    f.write(my_file.read())
    f.close()

# <codecell>

# If zap files are needed, copy and paste them into the new folder
import shutil
def copy_zap(file_name):
   if not file_name in os.listdir(private_today):
    shutil.copyfile(src = public + '/code/' + file_name, 
                    dst = private_today + '/' + file_name) 

copy_zap('zap.R')
copy_zap('zap.Rnw')
copy_zap('doh.png')
copy_zap('zap_compile.R')

# <codecell>

# Run the zap file (daily surveillance)
#os.chdir(private_today)
#robjects.r['source']("zap.R")

# <codecell>

# Compile the pdf
#os.chdir(private_today)
#robjects.r['source']("zap_compile.R")

# <codecell>

