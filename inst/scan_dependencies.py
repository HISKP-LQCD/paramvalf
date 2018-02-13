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
#pattern_call = re.compile(r'(\S+) <- call\.pvcontainer\(([^,)]+)(?:, ([^,)]+))+\)', re.M)
pattern_call = re.compile(r'^([^#\s]+) <- call\.pvcontainer\(([^)]+)\)', re.M)
#pattern_sink = re.compile(r'^call\.pvcontainer\(([^,)]+)(?:, ([^,)]+))+\)', re.M)
pattern_sink = re.compile(r'^call\.pvcontainer\(([^)]+)\)', re.M)

def process_file(filename):
    with open(filename) as f:
        contents = f.read()

    loads = pattern_load.findall(contents)
    saves = pattern_save.findall(contents)
    calls = pattern_call.findall(contents)
    sinks = pattern_sink.findall(contents)

    calls2 = []
    for call in calls:
        rvalue = call[0]

        tokens = call[1].split(', ')
        name = tokens[0]
        args = tokens[1:]

        calls2.append(dict(rvalue=rvalue,
                           name=name,
                           args=args))
        
    sinks2 = []
    for sink in sinks:
        tokens = sink.split(', ')
        name = tokens[0]
        args = tokens[1:]

        sinks2.append(dict(name=name,
                           args=args))

    basename = os.path.basename(filename)
    barename = os.path.splitext(basename)[0]

    return dict(filename=filename,
                basename=basename.replace('-', '_'),
                barename=barename.replace('-', '_'),
                loads=loads,
                calls=calls2,
                saves=saves,
                sinks=sinks2)


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
    subprocess.run(['dot', '-T', 'pdf', 'autoflow.dot', '-o', 'autoflow.pdf'])


    make = [dict(dest=['output/{}.Rdata'.format(call['rvalue']) for call in f['calls']],
                 src=[f['filename']] + ['output/{}.Rdata'.format(elem)
                      for elem in f['loads']],
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
