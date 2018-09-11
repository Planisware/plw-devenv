#!/usr/bin/python3
## -*- coding: windows-1252 -*-
##  COPYRIGHT (C) PLANISWARE 2018
##
##  All Rights Reserved
##
##  This program and the information contained herein are confidential to
##  and the property of PLANISWARE and are made available only to PLANISWARE
##  employees for the sole purpose of conducting PLANISWARE business.
##
##**************************************************************************

import distutils.dir_util
import os
import shutil
import subprocess
import sys
import zipfile

SRC_DIR = 'src'
DIST_DIR = 'dist'
ZIP_DIR = 'zip'
ELI_DIR = '../eli'
EL_DIR = '../el'
DEVENV_DIR = DIST_DIR+'/devenv'

PATCHES_SOURCE = 'E:/versions/plw6/release/610SP1/updates/_en_dev/' if sys.platform == 'win32' else '/E/versions/plw6/release/610SP1/updates/_en_dev/'

TARGETS = {'zip' : ['clean', 'init', 'copyfiles', 'version', 'patches', 'buildzip'],
       'env' : ['init', 'copyfiles'],
       'clean' : ['clean']}

def make(target):
  if target not in TARGETS:
    usage()

  for _function in TARGETS[target]:
    getattr(sys.modules[__name__],_function)()

def getversion():
  if os.getenv('VERSION'):
    return os.getenv('VERSION')
  elif os.getenv('BUILD_NUMBER'):
    return "incremental-"+os.getenv('BUILD_NUMBER')
  else:
    tag = subprocess.check_output(['git', 'describe', '--tags', '--abbrev=10']).strip("\n\r ")
    return "incremental-"+tag

def buildzip():
  filename = 'emacs-runtime-'+getversion()+".zip"
  print("Building "+filename)
  base = DIST_DIR + '/'
  with zipfile.ZipFile(ZIP_DIR+ '/'+filename, 'w') as _zipfile:
    ## zip the content of the dist directory as base
    for subdir, dirs, files in os.walk(DIST_DIR):
      for _f in files:
        _file = os.path.join(subdir, _f)
        _zipfile.write(_file, _file[len(base):] if _file.startswith(base) else _file)

def patches():
  destdir = DIST_DIR + '/patches'
  if not os.path.isdir(destdir):
    os.mkdir(destdir)
  with open('patches', 'r') as _f:
    for patch in _f.readlines():
      shutil.copy2(PATCHES_SOURCE + patch.strip("\n\r ") + '-is.obin', destdir)

def version():
  vfile = DIST_DIR + '/version.txt'
  if os.path.isfile(vfile):
    os.remove(vfile)
  with open(vfile, 'w') as _f:
    _f.write(getversion() + '\n')
    tag = subprocess.check_output(['git', 'describe', '--tags', '--abbrev=100']).strip("\n\r ")
    _f.write(tag + '\n')

def init():
  for _dir in [DIST_DIR, ZIP_DIR, DEVENV_DIR, DEVENV_DIR+'/el']:
    if not os.path.isdir(_dir):
      os.mkdir(_dir)

def copyfiles():
  print("Copying files")

  print("Copying source files")
  copyall(SRC_DIR, DIST_DIR)

  print("Copying eli files")
  distutils.dir_util.copy_tree(ELI_DIR, DIST_DIR+'/eli')

  print("Copying el files")
  copyall(EL_DIR, DEVENV_DIR + '/el')
  shutil.copy('../emacs-plw-ext.el', DEVENV_DIR)

def clean():
  for _dir in [DIST_DIR,ZIP_DIR]:
    if os.path.isdir(_dir):
      shutil.rmtree(_dir)

def copyall(src, dst, symlinks=False, ignore=None):
  for item in os.listdir(src):
    s = os.path.join(src, item)
    d = os.path.join(dst, item)
    if os.path.isdir(s):
      if not os.path.isdir(d):
        os.mkdir(d)

      copyall(s, d, symlinks, ignore)
    else:
      shutil.copy2(s, d)

def usage():
  print("Usage : ./make.py target")
  print("  target can be : " + ", ".join(TARGETS.keys()))
  sys.exit(1)


if __name__ == '__main__':
  if len(sys.argv) != 2:
    usage()
  else:
    make(sys.argv[1])
