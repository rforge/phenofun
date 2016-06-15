###########################################################################
# Functions for downloading data from AMOS
# Date: 15.06.16
# Author: Ludwig Bothmann
###########################################################################

###################################
# download_amos_func()
###################################

#' Function to download image data from AMOS
#' 
#' @param camera Name(s) of the camera (is a number, see URL) as vector
#' @param year Year(s) to be downloaded as vector
#' @param month Month(s) to be downloaded as vector
#' @export
download_amos_func <- function(camera,
										 year,
										 month){

python_string1 <- "# download_amos.py
# Austin Abrams, 2/16/10
# a helper utility to download and unzip a lot of images from the AMOS dataset.

import os
import sys
import urllib2
import StringIO
import zipfile
import threading
import time
# Change this to where you want data to be dumped off.  If not supplied, defaults to
# the current working directory.
# example:
# ROOT_LOCATION = '/path/to/where/you/want/AMOS_Data/'
ROOT_LOCATION = None

# change these parameters as necessary to download whichever camera or year or month you
# want to download.
CAMERAS_TO_DOWNLOAD = ["

python_string2 <- "]
YEARS_TO_DOWNLOAD = ["


python_string3 <- "]
#MONTHS_TO_DOWNLOAD = range(1,13)
MONTHS_TO_DOWNLOAD = ["

python_string4 <- "]
# if the script crashed or the power went out or something, this flag will
# skip downloading and unzipping a month's worth of images if there's already
# a folder where it should be.  If you set this to false, then downloads
# will overwrite any existing files in case of filename conflict.
SKIP_ALREADY_DOWNLOADED = True

# maximum number of threads allowed. This can be changed.
MAX_THREADS = 100

class DownloadThread(threading.Thread):
	camera_id = None
	year = None
	month = None

	def __init__(self, camera_id, year, month):
		threading.Thread.__init__(self)

		self.camera_id = camera_id
		self.year = year
		self.month = month

	def run(self):
		location = ROOT_LOCATION + '%08d/%04d.%02d/' % (self.camera_id, self.year, self.month)

		if SKIP_ALREADY_DOWNLOADED and os.path.exists(location):
			print(location + ' already downloaded.')
			return

		print('downloading to ' + location)
		zf = download(self.camera_id, self.month, self.year)
		print('completed downloading to ' + location)

		if not zf:
			print('skipping ' + location)
			return

		ensure_directory_exists(location)

		print('Extracting from ' + location)
		extract(zf, location)
		print('Done')


def download(camera_id, month, year):

# Downloads a zip file from AMOS, returns a file.

	last_two_digits = camera_id % 100;
	last_four_digits = camera_id % 10000;

	if year < 2013 or year == 2013 and month < 9:
		ZIPFILE_URL = 'http://amosweb.cse.wustl.edu/2012zipfiles/'
	else :
		ZIPFILE_URL = 'http://amosweb.cse.wustl.edu/zipfiles/'
	url = ZIPFILE_URL + '%04d/%02d/%04d/%08d/%04d.%02d.zip' % (year, last_two_digits, last_four_digits, camera_id, year, month)
	#print '    downloading...',
	sys.stdout.flush()

	try:
		result = urllib2.urlopen(url)
	except urllib2.HTTPError as e:
		print e.code, 'error.'
		return None

	handle = StringIO.StringIO(result.read())

	#print 'done.'
	sys.stdout.flush()

	return handle

def extract(file_obj, location):

	#Extracts a bunch of images from a zip file.

	#print '    extracting zip...',
	sys.stdout.flush()

	zf = zipfile.ZipFile(file_obj, 'r')
	zf.extractall(location)
	zf.close()
	file_obj.close()

	#print 'done.'
	sys.stdout.flush()

def ensure_directory_exists(path):

	#Makes a directory, if it doesn't already exist.

	dir_path = path.rstrip('/')       

	if not os.path.exists(dir_path):
		parent_dir_path = os.path.dirname(dir_path)
		ensure_directory_exists(parent_dir_path)

		try:
			os.mkdir(dir_path)
		except OSError:
			pass


def main():
	# for all cameras...
	for camera_id in CAMERAS_TO_DOWNLOAD:
		# for all years...
		for year in YEARS_TO_DOWNLOAD:
			# for all months of imagery...
			for month in MONTHS_TO_DOWNLOAD:

					thread_count = threading.activeCount()
					while thread_count > MAX_THREADS:
						print('Waiting for threads to finish...')
						time.sleep(1)
						thread_count = threading.activeCount()              

					download_thread = DownloadThread(camera_id=camera_id, year=year, month=month)
					download_thread.start()


if __name__ == '__main__':

	if ROOT_LOCATION == None:
		ROOT_LOCATION = os.getcwd() + '/AMOS_Data'

	if ROOT_LOCATION[-1] != '/':
		ROOT_LOCATION = ROOT_LOCATION + '/'
	print 'Downloading images to:'

	main()"

camera_paste <- paste(camera,collapse=",")
year_paste <- paste(year,collapse=",")
month_paste <- paste(month,collapse=",")

python_string <- paste0(python_string1,camera_paste,
								python_string2,year_paste,
								python_string3,month_paste,
								python_string4)

write.table(python_string, "download.txt", quote=FALSE, 
				row.names = FALSE, col.names = FALSE)

system("python download.txt")
}
