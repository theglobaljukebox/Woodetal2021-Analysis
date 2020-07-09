# Running this codebook

Before running this codebook you will need access to the Google Spreadsheets that have all the canto coding data and metadata.

Addititionally you will need to place a credentials.json that can be created using the google sheets API. This will allow you to read data from the Canto Coding Google Sheet.

Place this in root of this directory, in the same level as the python notebook.

## Installation Guide/Dependencies

### Python Version 3.4

You must have a working version of Python 3 installed.
You may download it here https://www.python.org/downloads/

To check to see if your Python has been added to your path open terminal and type:

`python --version`

Ensure that your pip distribution is for Python 3.

`pip --version`

If you get and error, refer to online forums on how to get around this, as the solutions are installation specific, and vary depedning on your operating system.

If all is well until now, you may proceed to install dependencies

`pip install -r requirements.txt`

After that's taken care of, launch the jupyter notebook by issuing the command:

`jupyter notebook`

