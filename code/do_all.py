#Import necessary libraries
import mechanize
import cookielib
from BeautifulSoup import BeautifulSoup
import html2text
import pandas as pd
import subprocess
import os
#os.environ['R_HOME']='/usr/lib/R'
import rpy2
import rpy2.robjects as robjects
import re
import time
import platform
import shutil

# Get today's date
today = time.strftime("%Y-%m-%d")

# Get yesterday's date
from datetime import datetime, timedelta
yesterday = (datetime.today() - timedelta(days=1)).strftime("%b %d %Y")

# Adjust for oddity
#today = (datetime.today() - timedelta(days=1))#.strftime("%b %d %Y")
#yesterday = (datetime.today() - timedelta(days=2)).strftime("%b %d %Y")
#today = today.strftime("%Y-%m-%d")

print 'today is ' + today
print 'yesterday is ' + yesterday
# Specify which directories we'll be using (different on Ben vs. Joe-linux vs. Joe-Windows)
plat = platform.uname()
if 'benbrew' in plat:
    private = '/home/benbrew/Documents/private/surv/'
    public = '/home/benbrew/Documents/surv_public'
elif 'joebrew' in plat:
    private = '/media/joebrew/JB/fdoh/private/surv'
    public = '/home/joebrew/Documents/surv_public'
elif 'Linux' in plat:
    private = '/media/joebrew/JB/fdoh/private/surv'
    public = '/home/joebrew/Documents/surv_public'
else:
    private = 'E:/fdoh/private/surv'
    public = 'C:/Users/BrewJR/Documents/surv_public'

private_today = private + '/' + today

print plat

# If necessary, create today's directory
# and set working directory there
if not os.path.exists(private_today):
  os.makedirs(private_today)

os.chdir(private)

# Run the r script to get the dates and URLs for today's data download
os.chdir(public)
os.system('Rscript code/00_get_links.R')
#robjects.r['source']("code/00_get_links.R")

# Read in which links I need for today
os.chdir(private)
todays_links = pd.read_csv('todays_links.csv')

# Browser
br = mechanize.Browser()

# Cookie Jar
cj = cookielib.LWPCookieJar()
br.set_cookiejar(cj)


# Browser options
br.set_handle_equiv(True)
br.set_handle_gzip(True)
br.set_handle_redirect(True)
br.set_handle_referer(True)
br.set_handle_robots(False)
br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)

br.addheaders = [('User-agent', 'Chrome')]

# The site we will navigate into, handling it's session
br.open('https://www.essencefl.com/florida_5_1_14/servlet/Login')
#br.open('https://github.com/login')

# View available forms
#for f in br.forms():
#    print f

# Select the second (index one) form (the first form is a search query box)
br.select_form(nr=0)

# Read in credentials
os.chdir(private)
pu = pd.read_csv('essence/pu.csv', dtype = 'string')

# Extract ESSENCE username and password from the pu file
u = list(pu['u'])[0]
p = list(pu['p'])[0]

# User credentials
br.form['j_username'] = u
br.form['j_password'] = p

# Login
br.submit()


# Check to see if Alachua has reported yet
reported_yet = br.open('https://www.essencefl.com/florida_5_1_14/servlet/HomePageServlet')
reported_text = reported_yet.read()
cleaned_yesterday = re.sub(' 0', '  ', yesterday)
if r'Alachua              reporting (2\/2) hospitals for ' + cleaned_yesterday in reported_text:
    print 'Good to go - both hospitals are reporting'
else:
    print 'Stop here - not all hospitals have reported yet for today'

# Loop through each link, download the data for that link, and write that data to a file
os.chdir(private_today)
for i in range(0,9,1):
    my_file = br.open(todays_links[0:]['link'][i])
    # Write a text file
    f = open(todays_links[0:]['file'][i], 'w')
    f.write(my_file.read())
    f.close()

# If zap files are needed, copy and paste them into the new folder
def copy_zap(file_name):
   if not file_name in os.listdir(private_today):
    shutil.copyfile(src = public + '/code/' + file_name, 
                    dst = private_today + '/' + file_name) 

copy_zap('zap.R')
copy_zap('zap.Rnw')
copy_zap('doh.png')
copy_zap('zap_compile.R')

# Run the zap file (daily surveillance)
os.chdir(private_today)
os.system('Rscript zap.R')
#os.system('R CMD BATCH --no-save --no-restore zap.R')
#robjects.r['source']("zap.R")

# Compile the pdf
os.chdir(public)
os.system('Rscript code/sweave_that_shit.R')

#os.chdir(private_today)
#os.system('ls -l -h')
#os.system('R CMD Sweave --pdf zap.Rnw')

os.system('gnome-open zap.pdf')
