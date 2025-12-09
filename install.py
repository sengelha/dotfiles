#!/usr/bin/env python -tt

import fnmatch
import os
import shutil

def determine_destination_directory():
    if os.name == 'nt':
        return os.path.join(os.environ['APPDATA'], '')
    else:
        return os.environ['HOME']

def remove_if_exists(lst, item=None, glob=None):
    if item is not None:
        try:
            lst.remove(item)
        except ValueError:
            pass
    elif glob is not None:
        for x in lst:
            if fnmatch.fnmatch(x, glob):
                lst.remove(x)
    else:
        raise Exception("One of item or glob must be provided")

def copy_file_if_newer(src, dst):
    if not os.path.exists(dst) or os.path.getmtime(src) > os.path.getmtime(dst):
        print(f"Copying {src} to {dst}")
        dst_dir = os.path.dirname(dst)
        if not os.path.exists(dst_dir):
            os.makedirs(dst_dir)
        shutil.copy2(src, dst)

def main():
    src_root = os.path.abspath(os.path.dirname(__file__))
    dst_root = determine_destination_directory()
    for root, dirs, files in os.walk(src_root):
        remove_if_exists(dirs, item=".git")
        remove_if_exists(files, item=".gitignore")
        remove_if_exists(files, item="Makefile")
        remove_if_exists(files, item="install.py")
        remove_if_exists(files, glob=".#*")
        remove_if_exists(files, glob="#*")

        relpath = os.path.relpath(root, src_root)
        for file in files:
            src = os.path.join(root, file)
            if relpath == ".":
                dst = os.path.join(dst_root, "." + file)
            else:
                dst = os.path.join(os.path.join(dst_root, "." + relpath), file)
            copy_file_if_newer(src=src, dst=dst)

if __name__ == '__main__':
    main()
