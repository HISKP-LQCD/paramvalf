#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright © 2018 Martin Ueding <dev@martin-ueding.de>

import argparse
import collections
import glob
import os
import pprint
import re
import subprocess

import jinja2

pattern_load = re.compile(r'^load\(\'output/([^.]+).Rdata\'\)', re.M)
pattern_save = re.compile(r'^save\([^,]+, file = \'output/([^.]+).Rdata\'\)', re.M)

def process_file(filename):
    with open(filename) as f:
        contents = f.read()

    loads = pattern_load.findall(contents)
    saves = pattern_save.findall(contents)

    basename = os.path.basename(filename)
    barename = os.path.splitext(basename)[0]

    return dict(filename=filename,
                basename=basename.replace('-', '_'),
                barename=barename.replace('-', '_'),
                loads=loads,
                saves=saves)


def add_rdata(barenames):
    return ['output/{}.Rdata'.format(x) for x in barenames]


def main():
    options = _parse_args()

    files = [process_file(filename) for filename in glob.glob('R/*.R')]
    files_rmd = [process_file(filename) for filename in glob.glob('*.Rmd')]

    source_dir = os.path.dirname(__file__)

    env = jinja2.Environment(loader=jinja2.FileSystemLoader(source_dir))
    dot_template = env.get_template('autoflow.dot.j2')
    make_template = env.get_template('Makefile.j2')

    dot_rendered = dot_template.render(files=list(files), rmds=files_rmd)
    with open('autoflow.dot', 'w') as f:
        f.write(dot_rendered)
    subprocess.check_call(['dot', '-T', 'pdf', 'autoflow.dot', '-o', 'autoflow.pdf'])

    make = [dict(dest=add_rdata(f['saves']),
                 src=[f['filename']] + add_rdata(f['loads']),
                 task='Rscript $<')
            for f in files]

    for f in files_rmd:
        make.append(dict(dest=['{}.pdf'.format(f['barename'])],
                         src=['{}.Rmd'.format(f['barename'])] + ['output/{}.Rdata'.format(elem)
                                                                     for elem in f['loads']],
                         task='Rscript -e "rmarkdown::render(\'{}.Rmd\')"'.format(f['barename'])))

    make_all = [dest
                for item in make
                for dest in item['dest']]


    make_rendered = make_template.render(make=make, all=make_all, source_dir=source_dir)
    with open('Makefile', 'w') as f:
        f.write(make_rendered)


def _parse_args():
    '''
    Parses the command line arguments.

    :return: Namespace with arguments.
    :rtype: Namespace
    '''
    parser = argparse.ArgumentParser(description='')
    options = parser.parse_args()

    return options


if __name__ == '__main__':
    main()
