import os
from os.path import *
import GPS

PATH = os.getenv("PATH").split(os.pathsep)
PATH.append(join(dirname(GPS.Project.project_changing_to),"bin"))
os.putenv("PATH", os.pathsep.join(PATH))
