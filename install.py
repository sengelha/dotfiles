#!/usr/bin/env python -tt

import logging
import os
import re
import shutil
import sys

DOTFILES_CONFIG = {
    'win32': {
        'config/nvim': "%LOCALAPPDATA%/nvim",
        'emacs.d': "%APPDATA%/.emacs.d",
    },
    'linux': {
        'config': '~/.config',
        'emacs.d': '~/.emacs.d',
        'gitconfig': '~/.gitconfig',
        'mutt': '~/.mutt',
        'muttrc': '~/.muttrc',
        'zshrc': '~/.zshrc',
    }
}

def copy_file(src, dst):
    if not os.path.exists(dst) or os.path.getmtime(src) > os.path.getmtime(dst):
        logging.info(f"Copying {src} to {dst}")
        dst_dir = os.path.dirname(dst)
        if not os.path.exists(dst_dir):
            os.makedirs(dst_dir)
        shutil.copy2(src, dst)
    elif os.path.getmtime(dst) > os.path.getmtime(src):
        logging.warning(f"{dst} has modification time after {src}, skipping")
    else:
        logging.debug(f"{dst} has same modification time as {src}, nothing to do")

def copy_dir(src, dst):
    shutil.copytree(src, dst, dirs_exist_ok=True, copy_function=copy_file)

def canonicalize_path(p):
    p = p.replace("/", os.sep)
    p = os.path.expandvars(p)
    p = os.path.expanduser(p)
    return p

def copy_all_dotfiles():
    for src, dst in DOTFILES_CONFIG[sys.platform].items():
        src = canonicalize_path(src)
        dst = canonicalize_path(dst)
        if os.path.isdir(src):
            copy_dir(src, dst)
        elif os.path.isfile(src):
            copy_file(src, dst)
        else:
            raise Exception(f"{src}: Unrecognized file type")

def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)s %(message)s')
    copy_all_dotfiles()

if __name__ == '__main__':
    main()
