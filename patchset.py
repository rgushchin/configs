#!/usr/bin/env python

from os.path import isdir
from os import listdir
import subprocess

def check_no_old_patches():
    for f in listdir('.'):
        if f.startswith('0') and f.endswith('.patch'):
            raise Exception('Looks like there is an old patch: %s' % f)

def find_cover():
    cmd = 'git log -50 --oneline --author="Roman Gushchin"'
    raw = subprocess.getoutput(cmd)
    for line in raw.split('\n'):
        if 'cover' in line or 'COVER' in line:
            return line.split(' ')[0]
    raise Exception('Can\'t find saved cover letter')


def read_commit(obj, only_log=True):
    cmd = 'git show --format="format:%%b"'
    if only_log:
        cmd += ' -s'
    cmd += ' %s' % obj
    raw = subprocess.getoutput(cmd)
    return raw


def read_cover(obj):
    for line in read_commit(obj, False).split('\n'):
        if line.startswith('+++ b/'):
            name = line.split('/')[-1]

    with open(name) as fd:
        raw = fd.read()

    return raw


def check_git_repo():
    if isdir('.git'):
        return True
    return False


def current_branch():
    with open('.git/HEAD') as fd:
        raw = fd.read().strip()
    if not raw.startswith('ref: refs/heads/'):
        raise Exception('Can\'t detect the current branch')
    try:
        branch = raw.split('/')[-1]
    except IndexError:
        raise Exception('Can\'t detect the current branch')
    return branch


def guess_version():
    branch = current_branch()
    if '.' not in branch:
        return None
    version = '.'.join(branch.split('.')[1:])
    if version[0].isnumeric():
        version = "v%s" % version
    return version


def format_patches(obj, cover, version):
    cmd = 'git format-patch %s --cover-letter' % obj
    if version:
        cmd += ' --subject-prefix=\"PATCH %s\"' % version
    subprocess.getoutput(cmd)

    with open('0000-cover-letter.patch') as fd:
        raw = fd.read()

    if '*** SUBJECT HERE ***\n' not in raw:
        raise Exception('Cover letter corrupted')
    if '*** BLURB HERE ***\n' not in raw:
        raise Exception('Cover letter corrupted')

    raw = raw.replace('*** SUBJECT HERE ***\n', cover)
    raw = raw.replace('*** BLURB HERE ***\n', '')

    if '*** SUBJECT HERE ***\n' in raw:
        raise Exception('Cover letter corrupted')
    if '*** BLURB HERE ***\n' in raw:
        raise Exception('Cover letter corrupted')

    with open('0000-cover-letter.patch', 'w') as fd:
        fd.write(raw)


def check_patches():
    for f in listdir('.'):
        if not f.startswith('0'):
            continue
        if not f.endswith('.patch'):
            continue
        if f == '0000-cover-letter.patch':
            continue
        cmd = './scripts/checkpatch.pl %s' % f
        (fail, desc) = subprocess.getstatusoutput(cmd)

        if fail:
            print('--- %s ---\n%s\n\n' % (f, desc))


def guess_recipients():
    print('git send-email --to="" --cc="linux-kernel@vger.kernel.org" --cc="kernel-team@fb.com" 00*')


def main():
    if not check_git_repo():
        raise Exception('Can\'t find a git repository')

    branch = current_branch()
    print('On branch %s' % branch)

    version = guess_version()
    print('Version %s' % version)

    cover_obj = find_cover()
    cover = read_cover(cover_obj)

    check_no_old_patches()

    format_patches(cover_obj, cover, version)
    check_patches()

    print('---\n')
    print('On branch %s' % branch)
    print('Version %s' % version)
    guess_recipients()


if __name__ == '__main__':
    main()
